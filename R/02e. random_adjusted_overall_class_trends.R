# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(boot)
library(forcats)
library(cowplot)
library(viridis)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/")

# read in the lambda files 
random_trend <- readRDS("Z:/submission_2/overall_10-random-languages.rds")

# read in the rds for total monthly views to retrieve the lambda ids
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/user_trends/total_monthly_views_10-languages.rds"))

# read in the days per month trends
random_days <- list(list())
random_days[[1]] <- readRDS("Z:/submission_2/random_days_per_month_rate.rds")
species_days <- readRDS("Z:/submission_2/species_days_per_month_rate.rds")

# read in the rds for total monthly views and random monthly views to retrieve the lambda ids
total_monthly_views <- readRDS("Z:/submission_2/total_monthly_views_10-languages.rds")
random_monthly_views <- list(list())
random_monthly_views[[1]] <- readRDS("Z:/submission_2/total_monthly_views_random_10-languages.rds")

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

# read in the string of languages and taxa - original order sorted alphabetically for files read in
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

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
                                     view_type = "random") %>% recast_lambda()

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas), start_column) {
  
  # remove na rows
  lambdas_new <- lambdas[complete.cases(lambdas), ]
  
  # select columns from lambda file to calculate mean, and build a cumprod trend
  lambda_data <- lambdas_new[, start_column:ncol(lambdas_new)]
  this_lambdas <- lambda_data[ind, ]
  mean_ann_lambda <- colMeans(this_lambdas, na.rm = TRUE)
  trend <- cumprod(10^c(0, mean_ann_lambda))
  return(trend)
}

# function for boostrapping the create_lpi function for each lambda, and generating a 95 % confidence interval
run_each_group <- function(lambda_files, random_trends_adjusted, start_column){
  
  # Bootstrap these to get confidence intervals
  dbi.boot <- boot(lambda_files, create_lpi, R = 50, start_column = start_column)
  
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
  random_trends_adjusted[[i]] <- run_each_group(lambda_files = random_fin_lambda[[1]][[i]], random_trends_adjusted = random_trend[[i]], start_column = 5) %>%
    mutate(language = languages[i])
}

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trends_adjusted)){
  random_trends_adjusted[[i]]$date <- as.numeric(1977:2033)
  random_trends_adjusted[[i]]$Year <- (random_trends_adjusted[[i]]$date - 1970)/12 + 2015
  random_trends_adjusted[[i]]$Year <- as.character(random_trends_adjusted[[i]]$Year)
  random_trends_adjusted[[i]]$lamda <- c(0, diff(log10(random_trends_adjusted[[i]]$LPI[1:57])))
  random_trends_adjusted[[i]]$date <- paste("X", random_trends_adjusted[[i]]$date, sep = "")
}

# bind together and plot the random trends
rbindlist(random_trends_adjusted) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI, group = language)) +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, group = language), alpha = 0.3) +
  facet_wrap(~language) +
  theme_bw()

# run the function with 10 languages, specifying the directory
user_files <- view_directories(classes,
                               directory)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE, colClasses = list(character = "SpeciesSSet"))
})

merge_fin_lambda <- adjust_day_rate(lambda_files = merge_monthly_days(lambda_files = language_views, 
                                                                      monthly_views = rescale_monthly_views(total_monthly_views),
                                                                      view_type = "species"), 
                                    day_rate = species_days,
                                    view_type = "species") %>% recast_lambda()

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(merge_fin_lambda)){
  for(j in 1:length(merge_fin_lambda[[i]])){
    data_file <- merge_fin_lambda[[i]][[j]]
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trends_adjusted[[j]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

# merge each lambda file with the speciesSSet ID from view data
merge_lambda <- list()
iucn_views_poll <- rescale_monthly_views(total_monthly_views)
merge_fin_lambda <- list()
for(i in 1:length(all_lambdas)){
  for(j in 1:length(all_lambdas[[i]])){
    merge_lambda[[j]] <- inner_join(all_lambdas[[i]][[j]], iucn_views_poll[[j]][[i]], by = "SpeciesSSet") %>%
      mutate(taxa = classes[i]) %>%
      mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
    print(nrow(all_lambdas[[i]][[j]]) - nrow(merge_lambda[[j]]))
  }
  merge_fin_lambda[[i]] <- merge_lambda
}

# function for binding all the lambdas together and calculate average for each q_wikidata
wiki_average <- function(data_file){
  data_fin <- data_file %>%
    reshape2::melt(id = c("q_wikidata", "SpeciesSSet", "Freq", "V1", "language", "taxa", "article")) %>%
    mutate(variable = as.character(variable)) %>%
    group_by(q_wikidata, variable, taxa) %>%
    summarise(mean_val = mean(value)) %>%
    ungroup()
  return(data_fin)
}

# rbindlist all lambda together and calculate averge for each species across languages
merge_species <- list()
for(i in 1:length(merge_fin_lambda)){
  merge_species[[i]] <- rbindlist(merge_fin_lambda[[i]])
}

# merge all the lambda files, and calc average across each q_wikidata
merge_species <- rbindlist(merge_species) %>% 
  wiki_average()

# reshape lambda files back into year rows, and then split into separate taxonomic classes
cast_lambda <- reshape2::dcast(merge_species, q_wikidata + taxa ~ variable)
all_lambdas <- split(cast_lambda, cast_lambda$taxa)

# run the boostrapping of trends for each lambda, and adjust for the random of that language
lpi_trends_adjusted <- list()
for(i in 1:length(all_lambdas)){
  lpi_trends_adjusted[[i]] <- run_each_group(all_lambdas[[i]], random_trends_adjusted[[1]], start_column = 4) %>%
    mutate(taxa = classes[i])
}

# bind together the trends for that language
bound_trends <- rbindlist(lpi_trends_adjusted)

language_frame <- list()
for(i in 1:56){
  language_frame[[i]] <- bound_trends %>%
    group_by(taxa) %>%
    filter(row_number() %in% c(i:57)) %>%
    mutate(lambda = c(0, diff(log10(LPI)))) %>%
    mutate(conf_diff = 1 - (mean(LPI_upr-LPI_lwr))) %>% 
    mutate(average_lambda = mean(lambda)) %>% 
    ungroup() %>% 
    filter(taxa != "random") %>% print(head()) %>%
    select(taxa, conf_diff, average_lambda, Year, LPI, LPI_upr, LPI_lwr) %>%
    unique() %>%
    mutate(factor_rate = factor(ifelse(average_lambda > 0, "increasing", "decreasing"))) %>% 
    mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low"))) %>%
    mutate(series_start = i)
}

# count the number increasing and decreasing time series for each taxa/language combination
series_start_var <- rbindlist(language_frame) %>% 
  mutate(Year = as.character(Year)) %>%
  select(taxa, factor_rate, series_start) %>%
  unique() %>%
  bind_rows(data.frame("taxa" = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                       "factor_rate" = c(rep(NA, 6)),
                       "series_start" = 57)) %>%
  arrange(taxa, series_start) %>%
  select(series_start, factor_rate)

# plot all the class level trends
bound_trends %>% cbind(series_start_var) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing", "decreasing"), labels = c("Average increase", "Average decrease"))) %>%
  mutate(taxa = factor(taxa, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                       labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  ggplot() +
  geom_point(aes(x = Year, y = LPI, colour = factor_rate), size = 1.25) +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_colour_manual("Benchmark month", na.translate = F, values = c("#009E73", "#D55E00")) +
  facet_wrap(~taxa) +
  ylab("SAI") +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("random_adjusted_all-class_SAI_start-point.png", scale = 1.1, dpi = 350)

