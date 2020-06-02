# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(boot)
library(RColorBrewer)
library(viridis)
library(forcats)
library(cowplot)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- "Z:/submission_2/lambda_files"

# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")

# read in the lambda files 
random_trend <- readRDS("Z:/submission_2/overall_10-random-languages.rds")

# read in the days per month trends
random_days <- list(list())
random_days[[1]] <- readRDS("Z:/submission_2/random_days_per_month_rate.rds")
species_days <- readRDS("Z:/submission_2/species_days_per_month_rate.rds")

# read in the rds for total monthly views and random monthly views to retrieve the lambda ids
total_monthly_views <- readRDS("Z:/submission_2/total_monthly_views_10-languages.rds")
random_monthly_views <- list(list())
random_monthly_views[[1]] <- readRDS("Z:/submission_2/total_monthly_views_random_10-languages.rds")

## format for the lpi function
# rescale each dataframe to start at 1970 and merge back with the views, then output lpi structure with original id
restructure_views <- function(x){
  data_fin <- x %>%
    select(article, q_wikidata, dec_date, av_views) %>%
    mutate(SpeciesSSet = as.character(as.numeric(as.factor(article)))) %>%
    filter(complete.cases(.)) %>%
    select(q_wikidata, SpeciesSSet, article) %>%
    unique() %>%
    mutate(SpeciesSSet = as.character(SpeciesSSet))
  return(data_fin)
}

rescale_monthly_views <- function(monthly_views){
  iucn_views_poll <- list()
  for(i in 1:length(monthly_views)){
    print(length(monthly_views))
    iucn_inner_list <-list()
    for(j in 1:length(monthly_views[[i]])){
      print(length(monthly_views[[i]]))
      iucn_inner_list[[j]] <- rescale_iucn(monthly_views[[i]][[j]])
      iucn_inner_list[[j]] <- select_comp(iucn_inner_list[[j]]) # select time series length
      iucn_inner_list[[j]] <- restructure_views(iucn_inner_list[[j]])
    }
    iucn_views_poll[[i]] <- iucn_inner_list
  }
  return(iucn_views_poll)
}

# string for pollinating classes, plus random
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, directory){
  
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
                               directory)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = list(character = "SpeciesSSet"))
})

## adjust the species trends and the random trends for the number of day per month trends
# merge each lambda file with the speciesSSet ID from view data
merge_monthly_days <- function(lambda_files, monthly_views, view_type){
  merge_lambda <- list()
  merge_fin_lambda <- list()
  if(view_type == "species"){
    for(i in 1:length(lambda_files)){
      for(j in 1:length(lambda_files[[i]])){
        merge_lambda[[j]] <- inner_join(lambda_files[[i]][[j]], monthly_views[[j]][[i]], by = "SpeciesSSet") %>%
          mutate(taxa = classes[i]) %>%
          mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
        print(nrow(lambda_files[[i]][[j]]) - nrow(merge_lambda[[j]]))
      }
      merge_fin_lambda[[i]] <- merge_lambda
    }
  }
  else{
    for(i in 1:length(lambda_files)){
      for(j in 1:length(lambda_files[[i]])){
        merge_lambda[[j]] <- inner_join(lambda_files[[i]][[j]], monthly_views[[i]][[j]], by = "SpeciesSSet") %>%
          mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
        print(nrow(lambda_files[[i]][[j]]) - nrow(merge_lambda[[j]]))
      }
      merge_fin_lambda[[i]] <- merge_lambda
    }
  }
  return(merge_fin_lambda)
}

# adjust each set of lambdas for the rate of change of days for that article
adjust_day_rate <- function(lambda_files, day_rate, view_type){
  whole_day_adjusted <- list()
  if(view_type == "species"){
    for(i in 1:length(lambda_files)){
      total_lambda_merge <- list()
        for(j in 1:length(lambda_files[[i]])){
          total_lambda_merge[[j]] <- reshape2::melt(lambda_files[[i]][[j]], id = c("V1", "SpeciesSSet", "Freq", "q_wikidata", "article", "language", "taxa")) %>%
            mutate(variable = as.character((variable)))
          day_rate[[j]][[i]]$dec_date <- as.character(day_rate[[j]][[i]]$dec_date)
          total_lambda_merge[[j]] <- inner_join(total_lambda_merge[[j]], day_rate[[j]][[i]], by = c("q_wikidata", "article", "variable" = "dec_date"))
          total_lambda_merge[[j]]$adjusted_rate <- total_lambda_merge[[j]]$value - total_lambda_merge[[j]]$rate
        }
      whole_day_adjusted[[i]] <- total_lambda_merge
    }
  }
  else{
    for(i in 1:length(lambda_files)){
      total_lambda_merge <- list()
      for(j in 1:length(lambda_files[[i]])){
        total_lambda_merge[[j]] <- reshape2::melt(lambda_files[[i]][[j]], id = c("V1", "SpeciesSSet", "Freq", "q_wikidata", "article", "language")) %>%
          mutate(variable = as.character((variable)))
        day_rate[[i]][[j]]$dec_date <- as.character(day_rate[[i]][[j]]$dec_date)
        total_lambda_merge[[j]] <- inner_join(total_lambda_merge[[j]], day_rate[[i]][[j]], by = c("q_wikidata", "article", "variable" = "dec_date"))
        total_lambda_merge[[j]]$adjusted_rate <- total_lambda_merge[[j]]$value - total_lambda_merge[[j]]$rate
      }
      whole_day_adjusted[[i]] <- total_lambda_merge
    }
  }
  return(whole_day_adjusted)
}

# recast data adjusted for number of days into lambda file format of next step
recast_lambda <- function(adjusted_lambda){
  for(i in 1:length(adjusted_lambda)){
    for(j in 1:length(adjusted_lambda[[i]])){
      adjusted_lambda[[i]][[j]] <- adjusted_lambda[[i]][[j]] %>% 
        select(V1, SpeciesSSet, Freq, variable, adjusted_rate)
      adjusted_lambda[[i]][[j]] <- reshape2::dcast(adjusted_lambda[[i]][[j]], V1 + SpeciesSSet + Freq ~ variable, value.var = "adjusted_rate")
    }
  }
  return(adjusted_lambda)
}

merge_fin_lambda <- adjust_day_rate(lambda_files = merge_monthly_days(lambda_files = language_views, 
                                                                      monthly_views = rescale_monthly_views(total_monthly_views),
                                                                      view_type = "species"), 
                                    day_rate = species_days,
                                    view_type = "species") %>% 
  recast_lambda()

# read in all the lambda files for the random data for each language
random_views <- list(list())
random_directories <- view_directories(classes = "random", directory)
system.time(for(i in 1:length(random_directories[[1]])){
  random_views[[1]][[i]] <- fread(random_directories[[1]][i], encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = list(character = "SpeciesSSet"))
})

random_fin_lambda <- adjust_day_rate(lambda_files = merge_monthly_days(lambda_files = random_views, 
                                                                       monthly_views = rescale_monthly_views(random_monthly_views),
                                                                       view_type = "random"), 
                                     day_rate = random_days,
                                     view_type = "random") %>% 
  recast_lambda()

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  
  # remove na rows
  lambdas_new <- lambdas[complete.cases(lambdas), ]
  
  # select columns from lambda file to calculate mean, and build a cumprod trend
  lambda_data <- lambdas_new[, 5:ncol(lambdas_new)]
  this_lambdas <- lambda_data[ind, ]
  mean_ann_lambda <- colMeans(this_lambdas, na.rm = TRUE)
  trend <- cumprod(10^c(0, mean_ann_lambda))
  return(trend)
}

# function for boostrapping the create_lpi function for each lambda, and generating a 95 % confidence interval
run_each_group <- function(lambda_files, random_trends_adjusted){
  
  # Bootstrap these to get confidence intervals
  dbi.boot <- boot(lambda_files, create_lpi, R = 1000)
  
  # Construct dataframe and get mean and 95% intervals
  boot_res <- data.frame(LPI = dbi.boot$t0)
  boot_res$Year <- random_trends_adjusted$Year[1:(nrow(random_trends_adjusted))]
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.975), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.025), na.rm = TRUE)
  return(boot_res)
}

# run the boostrapping of trends for each lambda, and adjust for the random of that language
random_trends_adjusted <- list()
for(i in 1:length(random_fin_lambda[[1]])){
  random_trends_adjusted[[i]] <- run_each_group(random_fin_lambda[[1]][[i]], random_trend[[i]]) %>%
      mutate(language = languages[i])
}

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trends_adjusted)){
  random_trends_adjusted[[i]]$date <- as.numeric(1977:2033)
  random_trends_adjusted[[i]]$Year <- (random_trends_adjusted[[i]]$date - 1970)/12 + 2015
  random_trends_adjusted[[i]]$Year <- as.character(random_trends_adjusted[[i]]$Year)
  random_trends_adjusted[[i]]$lamda <- c(0, diff(log10(random_trends_adjusted[[1]]$LPI[1:57])))
  random_trends_adjusted[[i]]$date <- paste("X", random_trends_adjusted[[i]]$date, sep = "")
}

# bind together and plot the random trends
rbindlist(random_trends_adjusted) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI, group = language)) +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, group = language), alpha = 0.3) +
  facet_wrap(~language) +
  theme_bw()

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(merge_fin_lambda)){
  for(j in 1:length(random_trends_adjusted)){
    data_file <- language_views[[i]][[j]]
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trends_adjusted[[j]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

# run the boostrapping of trends for each lambda, and adjust for the random of that language
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(all_lambdas)){
  for(j in 1:length(all_lambdas[[i]])){
    lpi_trends_adjusted[[j]] <- run_each_group(all_lambdas[[i]][[j]], random_trends_adjusted[[j]]) %>%
      mutate(language = random_trends_adjusted[[j]]$language)
    
  }
  
  # bind together the trends for that language
  bound_trends[[i]] <- rbindlist(lpi_trends_adjusted) %>%
    mutate(taxa = classes[i])
}

# bind together the trend for all languages
fin_bound_trends <- rbindlist(bound_trends)

# plot all the class level trends
fin_bound_trends %>%
  mutate(Year = as.numeric(Year)) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = language), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI, colour = language)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~taxa, scales = "free_y") +
  ylab("SAI") +
  xlab(NULL) +
  theme_bw()

ggsave("random-adjusted_day-adjusted__class_SAI_free_1000_95.png", scale = 1.3, dpi = 350)

# convert series back to lambda, and then take sets varying the start date up by one
# figure for overall changes of different groupings
# first calculate average lambda for each series
language_frame <- list()
for(i in 1:56){
  language_frame[[i]] <- fin_bound_trends %>%
    group_by(language, taxa) %>%
    filter(row_number() %in% c(i:57)) %>%
    mutate(lambda = c(0, diff(log10(LPI)))) %>%
    mutate(conf_diff = 1 - (mean(LPI_upr-LPI_lwr))) %>% 
    mutate(average_lambda = mean(lambda)) %>% 
    ungroup() %>% 
    filter(taxa != "random") %>%
    select(language, taxa, conf_diff, average_lambda) %>%
    unique() %>%
    mutate(factor_rate = factor(ifelse(average_lambda > 0, "increasing", "decreasing"))) %>% 
    mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low"))) %>%
    mutate(series_start = i)
}

# count the number increasing and decreasing time series for each taxa/language combination
series_start_var <- rbindlist(language_frame) %>%
  group_by(taxa, language) %>%
  count(factor_rate) %>%
  ungroup() %>%
  arrange(taxa, language) %>%
  reshape2::dcast(taxa + language ~ factor_rate)

series_start_var[is.na(series_start_var)] <- 0
series_start_var$change_diff <- series_start_var$increasing - series_start_var$decreasing

# count number increasing and decreasing
series_start_var <- series_start_var %>%
  mutate(up_down = ifelse(change_diff > 0, "increasing", "decreasing")) 

# build plot for language and certainty 
rate_plot_series_var <- series_start_var %>%
  mutate(up_down = factor(up_down, levels = c("increasing", "decreasing"), labels = c("Increase", "Decrease"))) %>%
  mutate(taxa = factor(taxa, levels = c("insecta", "actinopterygii", "amphibia", "mammalia", "aves", "reptilia"),
                       labels = c("Insecta", "Actinopterygii", "Amphibia", "Mammalia", "Aves", "Reptilia"))) %>% 
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                          labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  ggplot() +
  ggtitle("Total monthly view trends") +
  geom_tile(aes(x = language, y = taxa, fill = up_down), colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        legend.position = "none") 

# build dataframe for setting out the legend
d <- expand.grid(x=1:2,y=1:2)
d <- merge(d,data.frame(x=1:2,xlabel=c("X low", "X high")),by="x")
d <- merge(d,data.frame(y=1:2,ylabel=c("Y low", "Y high")),by="y")
d$alpha_val <- d$x

# build the legend with the appropriate colours for change and certainty
g.legend <- ggplot(d, aes(x,y,fill = ylabel)) +
  geom_tile(colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.4, yend = 0.6 ), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.6, yend = 2.4), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  theme_void() +
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10),
        axis.title.y = element_text(angle = 90),
        axis.title.x = element_text(hjust = 0.6, vjust = -0.8),
        axis.text.x = element_text()) +
  labs(title=NULL,x= "Certainty", y = "Change") +
  theme(axis.title=element_text(color="black"))

# combine the plot and the legend
ggdraw() +
  draw_plot(rate_plot, -0.13, width = 1, height = 1, scale = 0.75) +
  draw_plot(g.legend, width = 0.2, height = 0.3, scale = 0.95, hjust = -3.65, vjust = -1.96)

### rest to be reset to how it was before for whole series

# calculate number of factors for rates and confidence for each language
sort_rate_lang <- language_frame %>%
  group_by(language) %>%
  count(factor_rate) %>%
  ungroup() %>%
  filter(factor_rate == "decreasing") %>%
  select(language, n)

# add en at the bottom for engligh with no low confidence groupings
sort_conf_lang <- language_frame %>%
  group_by(language) %>%
  count(factor_conf) %>%
  ungroup()  %>%
  filter(factor_conf == "low") %>%
  select(language, n) %>%
  add_row(language = "\\^en_", n = 0)

# join together number of factors and sort on rate and confidence
joined_order_lang <- inner_join(sort_rate_lang, sort_conf_lang, by = "language") %>%
  arrange(desc(n.x), n.y)  %>% 
  mutate(language = factor(language, levels = language)) %>% pull(language)

# calculate number of factors for rates and confidence for each taxa
sort_rate_taxa <- language_frame %>%
  group_by(taxa) %>%
  count(factor_rate) %>%
  filter(factor_rate == "decreasing") %>%
  select(taxa, n) %>%
  arrange(desc(n)) %>%
  ungroup()

sort_conf_taxa <- language_frame %>%
  group_by(taxa) %>%
  count(factor_conf) %>%
  filter(factor_conf == "low") %>%
  select(taxa, n) %>%
  arrange(n) %>%
  ungroup()

# join together number of factors and sort on rate and confidence
joined_order_taxa <- inner_join(sort_rate_taxa, sort_conf_taxa, by = "taxa") %>%
  arrange(desc(n.x), n.y)  %>% 
  mutate(taxa = factor(taxa, levels = taxa)) %>% pull(taxa)

# build plot for language and certainty 
rate_plot <- fin_bound_trends %>%
  group_by(language, taxa) %>%
  mutate(lambda = c(0, diff(log10(LPI)))) %>% 
  filter(lambda != 0) %>%
  mutate(conf_diff = 1 - (mean(LPI_upr-LPI_lwr))) %>%
  mutate(mean_lambda = mean(lambda)) %>%
  mutate(certainty = abs(mean_lambda) * conf_diff) %>%
  ungroup() %>%
  select(mean_lambda, taxa, language, conf_diff) %>%
  unique() %>%
  filter(taxa != "random") %>% 
  mutate(factor_rate = factor(ifelse(mean_lambda > 0, "increasing", "decreasing"))) %>% 
  mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low"))) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing", "decreasing"), labels = c("Increase", "Decrease"))) %>%
  mutate(factor_conf = factor(factor_conf, levels = c("low", "high"), labels = c("Low",  "High"))) %>%
  mutate(alpha_val = as.numeric(factor_conf)) %>%
  mutate(taxa = factor(taxa, levels = joined_order_taxa,
                       labels = c("Insecta", "Actinopterygii", "Amphibia", "Mammalia", "Aves", "Reptilia"))) %>% 
  mutate(language = factor(language, levels = joined_order_lang,
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(plot_order = (as.numeric(factor_rate) + as.numeric(factor_conf))) %>%
  ggplot() +
  ggtitle("Total monthly view trends") +
  geom_tile(aes(x = language, y = taxa, fill = factor_rate, alpha = alpha_val), colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        legend.position = "none") 

# build dataframe for setting out the legend
d <- expand.grid(x=1:2,y=1:2)
d <- merge(d,data.frame(x=1:2,xlabel=c("X low", "X high")),by="x")
d <- merge(d,data.frame(y=1:2,ylabel=c("Y low", "Y high")),by="y")
d$alpha_val <- d$x

# build the legend with the appropriate colours for change and certainty
g.legend <- ggplot(d, aes(x,y,fill=ylabel, alpha = alpha_val))+
  geom_tile(colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.4, yend = 0.6 ), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.6, yend = 2.4), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  theme_void() +
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10),
        axis.title.y = element_text(angle = 90),
        axis.title.x = element_text(hjust = 0.6, vjust = -0.8),
        axis.text.x = element_text()) +
  labs(title=NULL,x= "Certainty", y = "Change") +
  theme(axis.title=element_text(color="black"))

# combine the plot and the legend
ggdraw() +
  draw_plot(rate_plot, -0.13, width = 1, height = 1, scale = 0.75) +
  draw_plot(g.legend, width = 0.2, height = 0.3, scale = 0.95, hjust = -3.65, vjust = -1.96)

# save the plot with legend and plot combined for change and certainty
ggsave("average_change_uncertainty_comb_2.png", scale = 1.1, dpi = 400)