# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(rvest)
library(xml2)
library(forcats)

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

# merge each lambda file with the speciesSSet ID from view data
merge_lambda <- list()
merge_fin_lambda <- list()
for(i in 1:length(all_lambdas)){
  for(j in 1:length(all_lambdas[[i]])){
    merge_lambda[[j]] <- inner_join(iucn_views_poll[[j]][[i]], all_lambdas[[i]][[j]], by = "SpeciesSSet") %>%
      mutate(taxa = classes[i]) %>%
      mutate(language = languages[j]) %>%
      select(-taxa, -language)
    
    # merge each set of lambda files with the q_wikidata and add columns for class and language
    print(nrow(all_lambdas[[i]][[j]]) - nrow(merge_lambda[[j]]))
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

## build model with language as random effect and sample from covariance matrix
model_1 <- lmer(av_lambda ~ taxonomic_class + (1|language), data = final_bound)
model_1_null <- lmer(av_lambda ~ 1 + (1|language), data = final_bound)
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

prediction_data <- final_bound %>%
  dplyr::select(taxonomic_class, av_lambda, language) %>%
  mutate(av_lambda = 0) %>%
  unique()

preds.emp <- sapply(X = 1:10000, iterate_covar_sai, model_1, prediction_data = prediction_data)

# extract the median, upper interval, and lower interval for samples
preds.emp.summ <- data.frame(Median = apply(X = preds.emp, MARGIN = 1, FUN = median),
                             Upper = apply(X = preds.emp, MARGIN = 1, FUN = quantile, probs = 0.975),
                             Lower = apply(X = preds.emp, MARGIN = 1, FUN = quantile, probs = 0.025))

# plot the sampled effects from covariance matrix
taxa_plot <- cbind(prediction_data, preds.emp.summ) %>%
  dplyr::select(-av_lambda) %>%
  mutate(taxonomic_class = factor(taxonomic_class, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                                  labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  mutate(taxonomic_class = factor(taxonomic_class)) %>%
  mutate(taxonomic_class = fct_reorder(taxonomic_class, Median)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  geom_errorbar(aes(x = taxonomic_class, ymin = Lower, ymax = Upper), width = 0.1) +
  geom_point(aes(x = taxonomic_class, y = Median)) + 
  scale_y_continuous("Monthly rate of change", breaks = c(-0.0015, -0.001, -0.0005, 0, 0.0005, 0.001), labels = c("-0.0015", "-0.001", "-0.0005", "0", "0.0005", "0.001")) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("class-rate-of-change_language-random-effect_10000_95.png", scale = 0.8, dpi = 400)

## approach for just english language to check models - gives mostly same output as before
english_prediction <- final_bound #%>%
  #filter(language == "\\^en_")

model_2 <- lm(av_lambda ~ taxonomic_class, data = english_prediction)

predicted_values <- predict(model_2, english_prediction, se.fit = TRUE)

english_prediction$predicted_values <- predicted_values$fit
english_prediction$predicted_values_se <- predicted_values$se.fit

fin_frame_6 <- english_prediction %>%
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
summary(model_3)

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
  ylab("Random adjusted average lambda") +
  facet_wrap(~language) +
  scale_y_continuous(breaks = c(-0.015, -0.01, -0.005, 0, 0.005), labels = c("-0.015", "-0.010", "-0.005", "0", "0.005")) +
  scale_colour_manual("Taxonomic class", values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank())

# save the plot for interaction of language and class
ggsave("taxa_language_rate-of-change_2.png", scale = 1.1, dpi = 350)

## model with species (q_wikidata) as a random effect, and then sample from covariance matrix
model_4 <- lmer(av_lambda ~ taxonomic_class * language + (1|q_wikidata), data = final_bound)
summary(model_4)

prediction_data <- final_bound %>%
  dplyr::select(taxonomic_class, av_lambda, language) %>%
  mutate(av_lambda = 0) %>%
  unique()

preds.emp <- sapply(X = 1:10000, iterate_covar_sai, model_4, prediction_data = prediction_data)

# extract the median, upper interval, and lower interval for samples
preds.emp.summ <- data.frame(Median = apply(X = preds.emp, MARGIN = 1, FUN = median),
                             Upper = apply(X = preds.emp, MARGIN = 1, FUN = quantile, probs = 0.975),
                             Lower = apply(X = preds.emp, MARGIN = 1, FUN = quantile, probs = 0.025))

# plot the sampled effects from covariance matrix
taxa_plot <- cbind(prediction_data, preds.emp.summ) %>%
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

## retrieve whether the species is utilised through IUCN data
rredlist::rl_threats(name = "Panthera leo", key = "b9982f9d361dab635bdde922a08242bb84a5df79127d5288baca14bfeb6c7d8d")

rredlist::rl_threats(name = "Loxodonta africana", key = "b9982f9d361dab635bdde922a08242bb84a5df79127d5288baca14bfeb6c7d8d")


rredlist::rl_narrative(name = "Panthera leo", key = "b9982f9d361dab635bdde922a08242bb84a5df79127d5288baca14bfeb6c7d8d")

rredlist::rl_threats(id = 103636217, key = "b9982f9d361dab635bdde922a08242bb84a5df79127d5288baca14bfeb6c7d8d" )

rredlist::rl_search(id = 41681, key = "b9982f9d361dab635bdde922a08242bb84a5df79127d5288baca14bfeb6c7d8d")

# iucn id is in the url
read_html("https://www.iucnredlist.org/species/103636217/103636261")
