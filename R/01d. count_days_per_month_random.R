## script for counting the number of views in analysis, and write total monthly views to rds
library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)
library(parallel)

# read in additional functions
source("R/00. functions.R")

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
directory <- "J:/submission_2/user_trends/random_views/"
random_sets <- c("6000", "5500")

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

# select random unique articles from each random subset
remove_dups <- function(data_file){
  article_set_1 <- data_file %>% 
    pull(q_wikidata) %>%
    unique() %>%
    as.character()
  return(article_set_1)
}

# run remove duplicates over each language
random_no_dup <- list()
for(i in 1:length(language_views)){
  random_no_dup[[i]] <- lapply(language_views[[i]], remove_dups)
}

# check for any articles that occur in the first random download and the second - no pages in both first and second download
for(i in 1:length(random_no_dup)){
  unique_articles <- intersect(random_no_dup[[i]][[1]], random_no_dup[[i]][[2]])
  print(length(unique_articles))
}

####

# bind each pair of random views together
language_views_monthly_bound <- list()
for(i in 1:length(language_views)){
  language_views_monthly_bound[[i]] <- rbindlist(language_views[[i]])
}

# set up outer list and inner list, and then iterate through each language and each class within languages
# count number of days per month, and then change days per month to rate of change
days_all <- list()
for(i in 1:length(language_views_monthly_bound)){

  # count the number of days per month
  language_views_monthly_bound[[i]]$year <- substr(language_views_monthly_bound[[i]]$timestamp, start = 1, stop = 4)
  language_views_monthly_bound[[i]]$month <- substr(language_views_monthly_bound[[i]]$timestamp, start = 5, stop = 6)
  language_views_monthly_bound[[i]]$day <- substr(language_views_monthly_bound[[i]]$timestamp, start = 7, stop = 8)
    
  # count the number of days per month, and then calculate a rate of change for those days per month
  days_all[[i]] <- language_views_monthly_bound[[i]] %>%
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

# add names for languages
names(language_views_monthly) <- languages
for(i in 1:length(language_views_monthly)){
  names(language_views_monthly[[i]]) <- random_sets
}

# save as rds file to rds
saveRDS(days_all, "J:/submission_2/random_days_per_month_rate.rds")
