library(data.table)
library(dplyr)
library(ggplot2)
library(mgcv)

# *** all of above points solved by adjusting for rate of chage of number of days
# do the pages that you pick up vary between accesses of the API?
# what's the variation in the number of days per months? 
# rate of change for number of days in each month? 
# need to smooth the number of days and how this changes

# read in additional functions
source("R/00. functions.R")

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
directory <- "Z:/submission_2/user_trends/"
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(languages, directory, view_files){
  
  # bring in all the files in that directory
  view_files <- list.files(directory)
  
  # set up empty list for files for each language
  user_files <- list()
  
  # set up each of the file directories
  for(i in 1:length(languages)){
    user_files[[i]] <- list.files(directory, pattern = languages[i])
    user_files[[i]] <- paste0(directory, "", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files)
}

# run the function with 10 languages, specifying the directory
user_files <- view_directories(languages,
                               directory)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# remove extra error columns from chinese dataframe - extra dataframe to avoid overwrite
language_views_edit <- language_views
language_views_edit[[9]][[1]] <- language_views_edit[[9]][[1]] %>%
  dplyr::select(-title, -V2)

# set up outer list and inner list, and then iterate through each language and each class within languages
# count number of days per month, and then change days per month to rate of change
days_all <- list()
for(i in 1:length(language_views_edit)){
  days <- list()
  for(j in 1:length(language_views_edit[[i]])){
    
    # count the number of days per month
    language_views_edit[[i]][[j]]$year <- substr(language_views_edit[[i]][[j]]$timestamp, start = 1, stop = 4)
    language_views_edit[[i]][[j]]$month <- substr(language_views_edit[[i]][[j]]$timestamp, start = 5, stop = 6)
    language_views_edit[[i]][[j]]$day <- substr(language_views_edit[[i]][[j]]$timestamp, start = 7, stop = 8)
    
    # count the number of days per month, and then calculate a rate of change for those days per month
    days[[j]] <- language_views_edit[[i]][[j]] %>%
      group_by(q_wikidata, article, year) %>%
      count(month) %>%
      ungroup() %>%
      group_by(article) %>%
      mutate(rate = c(0, diff(log10(n)))) %>%
      ungroup() %>%
      mutate(date_col = paste(year, month, "01", sep = "/")) %>% 
      mutate(date_col = as.Date(date_col, format = "%Y/%m/%d")) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(month = as.numeric(month)) %>%
      mutate(dec_date = 1970 + (year - 2015)*12 + month)
  }
  days_all[[i]] <- days
}

# save as rds file to rds
saveRDS(days_all, "J:/submission_2/species_days_per_month_rate.rds")

## smmooth the rate of change for the index of days with loess regression
rate_change <- readRDS("Z:/submission_2/species_days_per_month_rate.rds")

# set up three empty lists
language_bound <- list()

# iterate through through each langauge, then each class, and then each article for each class/language
for(i in 1:length(rate_change)){
  print(i)
  bound_subframes <- list()
  for(j in 1:length(rate_change[[i]])){
    article_subframe <- list()
    
    rate_change[[i]][[j]] <- select_comp(rate_change[[i]][[j]])
    
    unique_articles <- unique(rate_change[[i]][[j]]$q_wikidata)
    print(j)
    for(k in 1:length(unique_articles)){

      # filter for each unique article, create a variable for the length of the series
      article_subframe[[k]] <- rate_change[[i]][[j]] %>%
        filter(q_wikidata == unique_articles[k])
    
      x_range <- 1:length(article_subframe[[k]]$date_col)
      
      jittered_n <- jitter(article_subframe[[k]]$n, factor = 0.1)
      
      # run a loess regression to smooth the values for each article, and assigned to a smoothed values column
      y.loess <- loess(jittered_n~x_range, span = 0.25)
      article_subframe[[k]]$smoothed_values <- predict(y.loess, data.frame(x_range))
      
    }
    bound_subframes[[j]] <- rbindlist(article_subframe)
    
  }
  
  language_bound[[i]] <- bound_subframes
}

# convert the smoothed values into a rate of change, and then convert that into an index
calculate_index <- function(data_file){
  data_fin <- data_file %>%
    group_by(article) %>%
    mutate(smoothed_rate = c(0, diff(log10(smoothed_values)))) %>%
    mutate(smoothed_index = cumprod(10^c(smoothed_rate))) %>%
    ungroup() %>%
    mutate(article = as.factor(article)) %>%
    select(-smoothed_values, -smoothed_index, -n, -rate)
  return(data_fin)
}

# run the function for converting each set of smoothed values into an index
smoothed_day_rate <- list()
for(i in 1:length(rate_change)){
  smoothed_day_rate[[i]] <- lapply(language_bound[[i]], calculate_index)
}

# plot one smoothed index as an example
smoothed_day_rate[[1]][[1]] %>%
  filter(q_wikidata == "Q1039090") %>%
  mutate(index = cumprod(10^c(rate))) %>% 
  ggplot() +
    geom_line(aes(x = date_col, y = smoothed_index), colour = "red") +
    geom_line(aes(x = date_col, y = index), colour = "black")

# save the new smoothed trends
saveRDS(smoothed_day_rate, "smoothed_day_rate_species.rds")
