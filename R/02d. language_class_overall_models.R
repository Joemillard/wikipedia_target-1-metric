# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(rvest)
library(xml2)
library(forcats)
library(patchwork)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/average_lambda")

# read in the string of languages - original order sorted alphabetically for files read in - CHECK THAT THIS SHOULD BE SORTED
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")

# read in the lambda files 
random_trend <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")

# read in the rds for total monthly views to retrieve the lambda ids
average_monthly_views <- readRDS("Z:/submission_2/daily_average_views_10-languages.rds")

## format for the lpi function
# rescale each dataframe to start at 1970 and merge back with the views, then output lpi structure with original id
iucn_views_poll <- list()
for(i in 1:length(average_monthly_views)){
  iucn_views_poll[[i]] <- lapply(average_monthly_views[[i]], rescale_iucn)
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], select_comp) # select time series length
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], function(x){
    data_fin <- x %>%
      select(article, q_wikidata, dec_date, av_views) %>%
      mutate(SpeciesSSet = as.character(as.numeric(as.factor(article)))) %>%
      filter(complete.cases(.)) %>%
      select(q_wikidata, SpeciesSSet, article) %>%
      unique() %>%
      mutate(SpeciesSSet = as.character(SpeciesSSet))
    return(data_fin)
  })
}

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend)){
  random_trend[[i]]$date <- as.numeric(rownames(random_trend[[i]]))
  random_trend[[i]]$Year <- (random_trend[[i]]$date - 1970)/12 + 2015
  random_trend[[i]]$Year <- as.character(random_trend[[i]]$Year)
  
  # calculate lambda for random
  random_trend[[i]] <- random_trend[[i]] %>%
    filter(date %in% c(1977:2033))
  random_trend[[i]]$lamda = c(0, diff(log10(random_trend[[i]]$LPI_final[1:57])))
  random_trend[[i]]$date <- paste("X", random_trend[[i]]$date, sep = "")
  random_trend[[i]]$language <- languages[i]
}

# bind together and plot the random trends
random_trend_figure <- rbindlist(random_trend) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_line(aes(x = Year, y = LPI_final, group = language)) +
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = language), alpha = 0.3) +
  scale_y_continuous("Random index", breaks = c(0.6, 1, 1.4, 1.8)) +
  scale_x_continuous(NULL, breaks = c(2016, 2017, 2018, 2019, 2020), labels = c(2016, 2017, 2018, 2019, 2020)) +
  facet_wrap(~language) +
  theme_bw() +
  theme(panel.grid = element_blank())

# string for pollinating classes
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, directory, languages){
  
  # bring in all the files in that directory and assign to a list
  view_files <- list()
  for(i in 1:length(languages)){
    view_files[[i]] <- list.files(directory, pattern = languages[i])
  }
  
  # unlist the files in the correct order
  file_order <- unlist(view_files)
  
  # set up empty list for files for each language
  user_files_dir <- list()
  user_files <- list()
  
  # set up each of the file directories and order consisten with the random overall trend
  for(i in 1:length(classes)){
    user_files[[i]] <- list.files(directory, pattern = classes[i])
    user_files[[i]] <- user_files[[i]][order(match(user_files[[i]], file_order))]
    user_files_dir[[i]] <- paste0(directory, "/", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files_dir)
}
# run the function with 10 languages, specifying the directory
user_files <- view_directories(classes,
                               directory,
                               language = languages)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(language_views)){
  for(j in 1:length(random_trend)){
    data_file <- language_views[[i]][[j]] %>%
      mutate(SpeciesSSet = as.character(SpeciesSSet))
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend[[j]]$lamda))
  }
  all_lambdas[[i]] <- adj_lambdas
}

#### additional smoothing of the random adjusted indices

# smooth the adjusted random lambda for each species
# iterate through all the articles of that class/language
smooth_series <- function(X){
  
  # create index
  index <- cumprod(10^c(0, X))
  
  # smooth the index
  x_range <- 1:length(index)
  y.loess <- loess(index~x_range, span = 0.30)
  data_fin <- predict(y.loess, data.frame(x_range))
  return(data_fin)
}

# convert the index back to lambda
create_lambda <- function(X){
  lambda <- c(1, diff(log10(X)))
  return(lambda)
}

# convert back to index, run the smooth for random adjusted lambda, and then convert back the lamda
smooth_all_groups <- function(data_file){
  
  # set up an empty list for smoothed values
  smoothed_indices <- list()
  
  # smooth the series for each row (species)
  for(i in 1:nrow(data_file)){
    smoothed_indices[[i]] <- smooth_series(X = as.numeric(as.vector(data_file[i, 5:ncol(data_file)])))
    smoothed_indices[[i]] <- create_lambda(smoothed_indices[[i]])
  }
  
  smoothed_lambda <- as.data.frame(do.call(rbind, smoothed_indices))
  
  # add back in the original column names
  colnames(smoothed_lambda) <- colnames(data_file)[4:ncol(data_file)]
  
  # bind the adjusted smoothed lambda back onto the first four columns
  smoothed_lambda <- cbind(data_file[,1:3], smoothed_lambda)
  
  return(smoothed_lambda)
  
}

# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}

####

# merge each lambda file with the speciesSSet ID from view data
merge_lambda <- list()
merge_fin_lambda <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  for(j in 1:length(smoothed_adjusted_lamda[[i]])){
    merge_lambda[[j]] <- inner_join(iucn_views_poll[[j]][[i]], smoothed_adjusted_lamda[[i]][[j]], by = "SpeciesSSet") %>%
      mutate(taxa = classes[i]) %>%
      mutate(language = languages[j]) %>%
      select(-taxa, -language)
    
    # merge each set of lambda files with the q_wikidata and add columns for class and language
    print(nrow(smoothed_adjusted_lamda[[i]][[j]]) - nrow(merge_lambda[[j]]))
  }
  merge_fin_lambda[[i]] <- merge_lambda
}

# assign new column for class and filter out the extra lambda rows
model_format <- function(data_file, taxa, language){
  data_fin <- data_file %>%
    select(av_lambda, V1, Freq, SpeciesSSet, q_wikidata) %>%
    mutate(taxonomic_class = taxa)
  return(data_fin)
}

# calculate average for each row and reformat with language and class included
average_lambda <- function(data_file, taxa, series_start, series_end){
  
  # builds subset of columns to calculate average over
  data_fin <- data_file
  
  # select lambda columns
  data_subset <- data_fin[, (grep(series_start, colnames(data_fin))):grep(series_end, colnames(data_fin))]
  
  # remove NAs from mean calculation using rowMeans
  data_fin$av_lambda <- apply(data_subset, 1, mean, na.rm = TRUE)
  data_fin <- model_format(data_fin, taxa)
  return(data_fin)
}

# calculate the average lambda and add language/class columns
series_start <- c(1983)
series_end <- c(2031)

# calculate the average lambda and add language/class columns
avg_lambdas <- list()
for(i in 1:length(merge_fin_lambda)){
  avg_lambdas[[i]] <- lapply(merge_fin_lambda[[i]], average_lambda, taxa = classes[[i]], series_start = series_start, series_end = series_end)
}

# assign final column for languae
for(i in 1:length(avg_lambdas)){
  for(j in 1:length(avg_lambdas[[i]])){
    avg_lambdas[[i]][[j]]$language <- languages[[j]]
  }
}

# bind all the lambda files into a single dataframe into a single dataframe
final_bound <- list()
for(i in 1:length(avg_lambdas)){
  final_bound[[i]] <- rbindlist(avg_lambdas[[i]])
}

# rbind together the final dataframes
final_bound <- rbindlist(final_bound) %>%
  mutate(taxonomic_class = factor(taxonomic_class)) %>%
  mutate(language = factor(language)) %>%
  mutate(q_wikidata = factor(q_wikidata))

# read in smoothed rates of change - final_bound from above
final_bound <- readRDS("data/class_wiki_indices/submission_2/mean_lambda_q_wikidata.rds")

## build model with language as random effect and sample from covariance matrix
# remove french wikipedia for overall model
no_final_bound_french <- final_bound %>%
  filter(language != "\\^fr_") %>% 
  droplevels()

model_1 <- lmer(av_lambda ~ taxonomic_class + (1|language), data = no_final_bound_french)
model_1_null <- lmer(av_lambda ~ 1 + (1|language), data = no_final_bound_french)
AIC(model_1, model_1_null)

summary(model_1)

# call in MASS here specifically for sampleing
iterate_covar_sai <- function(i, model, prediction_data){
  
  # extract fixed effects from covariance matrix 
  coefs <- MASS::mvrnorm(n = 1, mu = fixef(object = model), Sigma = vcov(object = model))
  mm <- model.matrix(terms(model), prediction_data)
  
  y <- mm %*% coefs
  
  # return the vector of adjusted values for that sample
  return(y)
}

no_french_prediction_data <- no_final_bound_french %>%
  dplyr::select(taxonomic_class, av_lambda, language) %>%
  mutate(av_lambda = 0) %>%
  unique()

no_french_preds.emp <- sapply(X = 1:1000, iterate_covar_sai, model_1, prediction_data = no_french_prediction_data)

# extract the median, upper interval, and lower interval for samples
no_french_preds.emp.summ <- data.frame(Median = apply(X = no_french_preds.emp, MARGIN = 1, FUN = median),
                             Upper = apply(X = no_french_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.975),
                             Lower = apply(X = no_french_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.025))

# plot the sampled effects from covariance matrix
taxa_plot <- cbind(no_french_prediction_data, no_french_preds.emp.summ) %>%
  dplyr::select(-av_lambda) %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  mutate(taxonomic_class = factor(taxonomic_class)) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, Median)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  geom_errorbar(aes(x = taxonomic_class, ymin = Lower, ymax = Upper), width = 0.1) +
  geom_point(aes(x = taxonomic_class, y = Median)) + 
  scale_y_continuous("Monthly rate of change", breaks = c(0, 0.0005, 0.001, 0.0015, 0.002), labels = c("0", "0.0005", "0.001", "0.0015", "0.002")) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("class-rate-of-change_language-random-effect_10000_95_no_french_2.png", scale = 0.8, dpi = 400)

## approach for just taxa to check approach of language
model_2 <- lm(av_lambda ~ taxonomic_class, data = final_bound)

predicted_values <- predict(model_2, final_bound, se.fit = TRUE)

final_bound_class <- final_bound
final_bound_class$predicted_values <- predicted_values$fit
final_bound_class$predicted_values_se <- predicted_values$se.fit

fin_frame_6 <- final_bound_class %>%
  dplyr::select(taxonomic_class, predicted_values, predicted_values_se) %>%
  unique()

fin_frame_6 %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, predicted_values)) %>%
  ggplot() + 
  geom_errorbar(aes(x = taxonomic_class, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se)))) +
  geom_point(aes(x = taxonomic_class, y = predicted_values)) +
  ylab("Random adjusted average lambda") +
  theme_bw() +
  theme(legend.position = "none")

## approach using language and class as fixed effects
model_3 <- lm(av_lambda ~ taxonomic_class * language, data = final_bound)
model_3_taxa <- lm(av_lambda ~ taxonomic_class, data = final_bound)
model_3_language<- lm(av_lambda ~ language, data = final_bound)
model_int <- lm(av_lambda ~ 1, data = final_bound)

# check AIC values
AIC(model_3, model_3_taxa, model_3_language, model_int)

summary(model_3)
anova(model_3)

# set up prediction data
predicted_values_interaction <- predict(model_3, final_bound, se.fit = TRUE)

# create new dataframe for interactions
final_bound_interaction <- final_bound
final_bound_interaction$predicted_values <- predicted_values_interaction$fit
final_bound_interaction$predicted_values_se <- predicted_values_interaction$se.fit

# select unique dataframe with predicted values
fin_frame_6 <- final_bound_interaction %>%
  dplyr::select(taxonomic_class, language, predicted_values, predicted_values_se) %>%
  unique()

# add labels for factors, sort by predicted value for language and class, and then plot
fin_frame_6 %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, -predicted_values, median)) %>%
  mutate(language = fct_reorder(language, -predicted_values, median)) %>%
  ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  geom_errorbar(aes(x = 1, colour = taxonomic_class, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se))), position=position_dodge(width = 0.5), width = 0.2) +
  geom_point(aes(x = 1, colour = taxonomic_class, y = predicted_values), position=position_dodge(width=0.5)) +
  ylab("Monthly change in Species Awareness Index (SAI)") +
  facet_wrap(~language) +
  scale_y_continuous(breaks = c(-0.015, -0.01, -0.005, 0, 0.005), labels = c("-0.015", "-0.010", "-0.005", "0", "0.005")) +
  scale_colour_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank())

# save the plot for interaction of language and class
ggsave("taxa_language_rate-of-change_3.png", scale = 1, dpi = 350)

# checking model assumptions
par(mfrow = c(2,2))
plot(model_3)

final_bound %>%
  ggplot() +
  geom_boxplot(aes(x = taxonomic_class, y = av_lambda)) +
  facet_wrap(~language)

## model with species (q_wikidata) as a random effect, and then sample from covariance matrix
model_4 <- lmer(av_lambda ~ taxonomic_class * language + (1|q_wikidata), data = final_bound)
summary(model_4)

prediction_data_inter_random <- final_bound %>%
  dplyr::select(taxonomic_class, av_lambda, language) %>%
  mutate(av_lambda = 0) %>%
  unique()

inter_random_preds.emp <- sapply(X = 1:10000, iterate_covar_sai, model_4, prediction_data = prediction_data_inter_random)

# extract the median, upper interval, and lower interval for samples
inter_random_preds.emp.summ <- data.frame(Median = apply(X = inter_random_preds.emp, MARGIN = 1, FUN = median),
                             Upper = apply(X = inter_random_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.975),
                             Lower = apply(X = inter_random_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.025))

# plot the sampled effects from covariance matrix
taxa_plot <- cbind(prediction_data_inter_random, inter_random_preds.emp.summ) %>%
  dplyr::select(-av_lambda) %>%
  mutate(taxonomic_class = factor(taxonomic_class)) %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, -Median, median)) %>%
  mutate(language = fct_reorder(language, -Median, median)) %>%  
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  geom_errorbar(aes(x = 1, ymin = Lower, ymax = Upper, colour = taxonomic_class), position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(aes(x = 1, y = Median, colour = taxonomic_class), position = position_dodge(width = 0.5), size = 1.5) +
  facet_wrap(~language) +
  scale_y_continuous("Random adjusted rate of change", breaks = c(-0.015, -0.01, -0.005, 0, 0.005), labels = c("-0.015", "-0.010", "-0.005", "0", "0.005")) +
  scale_colour_manual("Taxonomic class", values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank())

# save the plot for interaction of language and class
ggsave("taxa_language_rate-of-change_species-random_covariance-mat_2.png", scale = 1.1, dpi = 350)

## models for rate of change predicted as function of pollinator and traded
# first off, merge the FAO fish and traded species with the rates of change
# set up string of required wiki projects
wiki_projects <- c("eswiki", "frwiki", "dewiki", "jawiki", "itwiki", "arwiki", "ruwiki", "ptwiki", "zhwiki", "enwiki")

# read in smoothed rates of change - final_bound from above
rates_of_change <- readRDS("data/class_wiki_indices/submission_2/mean_lambda_q_wikidata.rds")

# assign new column to rate of change for wikipedia site
for(i in 1:length(wiki_projects)){
  rates_of_change$site[rates_of_change$language == languages[i]] <- wiki_projects[i]
}

# read in pollinator data
pollinat <- read.csv("data/COL_compiled_pollinators_add_conf.csv", stringsAsFactors = FALSE) %>%
  select(genus, Class, confidence, fact_conf, comb_conf) %>%
  mutate(Class = tolower(Class)) %>%
  filter(genus != "Tephrozosterops") %>%
  unique()

# read in traded vertebrate species, remove first three empty rows, and then add the fourth row as column names
traded_species <- read.csv("data/class_wiki_indices/submission_2/globally_traded_species_Scheffers.csv", stringsAsFactors = FALSE)

# read in the iucn_titles to match genus name and class for the pollinators - filter out tephrozosterops, which has duplicated wiki id among languages and lower pollination confidence
iucn_titles <- read.csv("data/class_wiki_indices/submission_2/all_iucn_titles.csv", stringsAsFactors = FALSE) %>%
  select(genus_name, class_name, order_name, q_wikidata) %>%
  unique() %>%
  mutate(class_name = tolower(class_name)) %>%
  mutate(order_name = tolower(order_name)) %>%
  filter(genus_name != "Tephrozosterops")

# read in the q_wikidata for traded species
traded_species_en <- read.csv("data/class_wiki_indices/submission_2/traded_species_wikidata.csv", stringsAsFactors = FALSE) %>%
  select(-ï..)
traded_species_rem <- read.csv("data/class_wiki_indices/submission_2/traded_species_wikidata_fr_edit.csv", stringsAsFactors = FALSE) %>%
  select(-manual_search, -X)

#
all_traded <- rbind(traded_species_en, traded_species_rem) %>%
  filter(qwiki_id != "") %>% 
  select(qwiki_id, ns) %>%
  unique()
  
# read in the fish species
fao_fishes <-  read.csv("data/class_wiki_indices/submission_2/ASFIS_sp/ASFIS_sp_2020.csv", stringsAsFactors = FALSE)

# read in the fish species with q_wikidata
fishes_en <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata.csv", stringsAsFactors = FALSE)
fishes_zh <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_zh.csv", stringsAsFactors = FALSE)
fishes_fr <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_fr.csv", stringsAsFactors = FALSE)
fishes_de <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_de.csv", stringsAsFactors = FALSE)
fishes_es <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_es.csv", stringsAsFactors = FALSE)
fishes_ru <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_ru.csv", stringsAsFactors = FALSE)
fishes_pt <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_pt.csv", stringsAsFactors = FALSE)
fishes_it <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_it.csv", stringsAsFactors = FALSE)
fishes_ar <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_ar.csv", stringsAsFactors = FALSE)
fishes_ja <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_ja.csv", stringsAsFactors = FALSE)

# bind together all the fished data
all_fishes <- rbind(fishes_en, fishes_zh, 
                    fishes_fr, fishes_de, 
                    fishes_es, fishes_ru, 
                    fishes_pt, fishes_it, 
                    fishes_ar, fishes_ja) %>%
  filter(qwiki_id != "") %>%
  select(qwiki_id, ns) %>%
  unique()

# merge the traded and fish species onto the datasets by q_wikidata - for some reason fao data contains some non-fish species, so filtering out
rates_fish <- left_join(rates_of_change, all_fishes, by = c("q_wikidata" = "qwiki_id")) %>%
  rename(used = ns)

# add new column for "used" for fish that are both fish and fished
rates_fish$used <- ifelse(!is.na(rates_fish$used) & rates_fish$taxonomic_class == "actinopterygii", "Y", "N")

# merge the rates_fish dataframe onto the traded animals
traded_rates <- left_join(rates_fish, all_traded, by = c("q_wikidata" = "qwiki_id"))

# amend column for "used" for species that are traded
traded_rates$used[traded_rates$ns == 0] <- "Y"

# set all the insects as NA because we don't know
traded_rates$used[traded_rates$taxonomic_class == "insecta"] <- NA

# bind the iucn_titles data onto the rates to retrieve the class and genus name
traded_rates <- left_join(traded_rates, iucn_titles, by = "q_wikidata")

# set any non squamate reptiles to NA
traded_rates$used[traded_rates$taxonomic_class == "reptilia" & traded_rates$order_name != "squamata"] <- NA

# join rates of change data onto the pollinator data, with full join to keep those that aren't pollinators
joined_pollinators <- left_join(traded_rates, pollinat, by = c("genus_name" = "genus", "taxonomic_class" = "Class")) %>%
  select(-genus_name) %>%
  unique()

# add column for whether that species is a pollinator, on basis of NAs in confidence column
joined_pollinators$pollinating[!is.na(joined_pollinators$confidence)] <- "Y"
joined_pollinators$pollinating[is.na(joined_pollinators$confidence)] <- "N"

# remove the extra pollination columns
joined_pollinators_poll <- joined_pollinators %>%
  select(-confidence, -fact_conf, -comb_conf, -class_name, -ns) %>%
  #filter(site != "frwiki") %>%
  filter(!taxonomic_class %in% c ("actinopterygii", "amphibia"))

### models predicting rate of change against pollinating/non-pollinating and traded/non-traded, with language random effect
poll_traded_model_1a <- lmerTest::lmer(av_lambda ~ pollinating * taxonomic_class + (1|language), data = joined_pollinators_poll)
poll_traded_model_1b <- lmerTest::lmer(av_lambda ~ pollinating + (1|language), data = joined_pollinators_poll)
poll_traded_model_1c <- lmerTest::lmer(av_lambda ~ taxonomic_class + (1|language), data = joined_pollinators_poll)
poll_traded_model_1d <- lmerTest::lmer(av_lambda ~ 1 + (1|language), data = joined_pollinators_poll)

summary(poll_traded_model_1a)
anova(poll_traded_model_1a)

AIC(poll_traded_model_1a, 
    poll_traded_model_1b, 
    poll_traded_model_1c,
    poll_traded_model_1d)

prediction_data_inter_random <- joined_pollinators_poll %>%
  dplyr::select(taxonomic_class, pollinating, av_lambda) %>%
  mutate(av_lambda = 0) %>%
  unique()

used_random_preds.emp <- sapply(X = 1:1000, iterate_covar_sai, poll_traded_model_1a, prediction_data = prediction_data_inter_random)

# extract the median, upper interval, and lower interval for samples
used_random_preds.emp.summ <- data.frame(Median = apply(X = used_random_preds.emp, MARGIN = 1, FUN = median),
                                          Upper = apply(X = used_random_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.975),
                                          Lower = apply(X = used_random_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.025))

taxa_plot <- cbind(prediction_data_inter_random, used_random_preds.emp.summ) %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("aves", "insecta", "mammalia", "reptilia"), 
                                  labels = c("Birds", "Insects", "Mammals", "Reptiles"))) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, -Median, median)) %>%
  mutate(pollinating = factor(pollinating, levels = c("Y", "N"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  xlab(NULL) +
  ylab("Average monthly change in SAI") +
  geom_errorbar(aes(x = taxonomic_class, ymin = Lower, ymax = Upper, colour = pollinating), position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(aes(x = taxonomic_class, y = Median, colour = pollinating), position = position_dodge(width = 0.5), size = 1.5) +
  scale_colour_manual("Pollinating", values = c("#000000", "#E69F00")) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002), labels = c("0", "0.001", "0.002")) +
  theme_bw() +
  theme(panel.grid = element_blank())

### models predicting rate of change against pollinating/non-pollinating and traded/non-traded
joined_pollinators_use <- joined_pollinators %>%
  #filter(site != "frwiki") %>%
  filter(taxonomic_class != "insecta")

poll_traded_model_2a <- lmerTest::lmer(av_lambda ~ used * taxonomic_class + (1|language), data = joined_pollinators_use)
poll_traded_model_2b <- lmerTest::lmer(av_lambda ~ used + (1|language), data = joined_pollinators_use)
poll_traded_model_2c <- lmerTest::lmer(av_lambda ~ taxonomic_class + (1|language), data = joined_pollinators_use)
poll_traded_model_2d <- lmerTest::lmer(av_lambda ~ 1 + (1|language), data = joined_pollinators_use)

summary(poll_traded_model_1)
anova(poll_traded_model_2)

AIC(poll_traded_model_2a, poll_traded_model_2b, poll_traded_model_2c)

prediction_data_inter_random <- joined_pollinators_use %>%
  dplyr::select(taxonomic_class, used, av_lambda) %>%
  mutate(av_lambda = 0) %>%
  unique() %>%
  slice(1:10)

used_random_preds.emp <- sapply(X = 1:1000, iterate_covar_sai, poll_traded_model_2, prediction_data = prediction_data_inter_random)

# extract the median, upper interval, and lower interval for samples
used_random_preds.emp.summ <- data.frame(Median = apply(X = used_random_preds.emp, MARGIN = 1, FUN = median),
                                         Upper = apply(X = used_random_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.975),
                                         Lower = apply(X = used_random_preds.emp, MARGIN = 1, FUN = quantile, probs = 0.025))

taxa_plot_use <- cbind(prediction_data_inter_random, used_random_preds.emp.summ) %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("actinopterygii", "amphibia", "aves", "mammalia", "reptilia"), 
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Mammals", "Reptiles"))) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, -Median, median)) %>%
  mutate(used = factor(used, levels = c("Y", "N"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  geom_errorbar(aes(x = taxonomic_class, ymin = Lower, ymax = Upper, colour = used), position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(aes(x = taxonomic_class, y = Median, colour = used), position = position_dodge(width = 0.5), size = 1.5) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002), labels = c("0", "0.001", "0.002")) +
  scale_colour_manual("Traded/harvested", values = c("#000000", "#E69F00")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank())
  
taxa_plot + taxa_plot_use + plot_layout(ncol = 1)

ggsave("use_pollinating.png", scale = 1.1, dpi = 350)

# plot broken down by three way interaction for trade, taxa, language
joined_pollinators_use <- joined_pollinators %>%
  filter(taxonomic_class != "insecta")

poll_traded_model_3 <- lm(av_lambda ~ used * taxonomic_class * language, data = joined_pollinators_use)
summary(poll_traded_model_3)

# set up prediction data
predicted_val <- predict(poll_traded_model_3, joined_pollinators_use, se.fit = TRUE)

# create new dataframe for interactions
final_bound_interaction <- joined_pollinators_use
final_bound_interaction$predicted_values <- predicted_val$fit
final_bound_interaction$predicted_values_se <- predicted_val$se.fit

# select unique dataframe with predicted values
fin_frame_6 <- final_bound_interaction %>%
  dplyr::select(taxonomic_class, language, used, predicted_values, predicted_values_se) %>%
  unique()

# dataframe for circling China fishes
china_fishes <- data.frame(x = "Ray finned fishes", language = "Chinese", y = 1.603572e-03) %>%
  mutate(language = factor(language, levels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian")))

# add labels for factors, sort by predicted value for language and class, and then plot
fin_frame_6 %>%
  filter(!is.na(predicted_values)) %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, -predicted_values, median)) %>%
  mutate(language = fct_reorder(language, -predicted_values, median)) %>%
  mutate(used = factor(used, levels = c("Y", "N"), labels = c("Yes", "No"))) %>%
  ggplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  geom_errorbar(aes(x = taxonomic_class, colour = used, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se))), position=position_dodge(width = 0.5), width = 0.2) +
  geom_point(aes(x = taxonomic_class, colour = used, y = predicted_values), position=position_dodge(width=0.5)) +
  geom_point(aes(x = x, y = y), data = china_fishes, shape = 1, size = 8, colour = "red") +
  ylab("Monthly change in Species Awareness Index (SAI)") +
  facet_wrap(~language, ncol = 5) +
  scale_y_continuous(breaks = c(-0.0025, 0, 0.0025, 0.005), labels = c("-0.0025", "0", "0.0025", "0.005")) +
  scale_colour_manual("Traded", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank())

ggsave("trade_language_taxa.png", scale = 1.1, dpi = 350)
