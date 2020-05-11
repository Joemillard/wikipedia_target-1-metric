# set up the packages required
library(ggplot2)
library(rlpi)
library(dplyr)
library(data.table)

# read in additional functions
source("R/00. functions.R")

# read in the rds for total monthly views
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/total_monthly_views_random_10-languages.rds"))

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# aggregate views to annual level for all articles with complete time series(57 months), and filter for those with complete year
sum_annual <- function(data_file){
  data_fin <- data_file %>%
    group_by(year, article, q_wikidata) %>%
    tally(av_views) %>%
    ungroup() %>%
    filter(year %in% c(2016, 2017, 2018, 2019))
  return(data_fin)
}

# reformat data for lpi
reformat_annual <- function(data_file) {
  data_fin <- data_file %>%
    mutate(dec_date = 1970 + (as.numeric(year) - 2016)) %>%
    select(article, dec_date, n) %>%
    mutate(ID = as.character(as.numeric(as.factor(article)))) %>%
    rename(Binomial = article) %>%
    rename(year = dec_date) %>%
    rename(popvalue = n) %>%
    mutate(Binomial = ID) %>%
    select(Binomial, ID, year, popvalue)
  return(data_fin)
}

# count number of articles in each random grouping
count_articles <- function(data_file){
  data_file %>% 
    select(q_wikidata) %>%
    unique() %>%
    tally() %>%
    print()
}

## format for the lpi function
# select complete time series (57 months), calculate the annual total views, and rescale each dataframe to start at 1970 for lpi
## format for the lpi function
# rescale each dataframe to start at 1970 and merge back with the views
iucn_views_poll <- lapply(total_monthly_views, select_comp) # format for lpi
iucn_views_poll <- lapply(iucn_views_poll, sum_annual) # select time series length
iucn_views_poll <- lapply(iucn_views_poll, reformat_annual) # format for lpi

# set up the infile with equal weightings for each class in each language, and write to txts
for(i in 1:length(languages)){
  infile_df <- list()
  write.table(iucn_views_poll[[i]], paste(languages[i], "random_data.txt", sep = "_"), row.names = FALSE)
  infile_df[[i]] <- data.frame(FileName = paste(languages[i], "random_data.txt", sep = "_"), Group = 1, Weighting = 1)
  write.table(infile_df[[i]], paste(languages[i], "random_all_infile_conf.txt", sep = "_"), row.names = FALSE)
}

# run lpi trends for each language
lpi_trends <- list()
for(i in 1:length(languages)){
  lpi_trends[[i]] <- LPIMain(paste(languages[i], "random_all_infile_conf.txt", sep = "_"), REF_YEAR = 1970, PLOT_MAX = 1973)
}

# save the rds for annual random trends
saveRDS(lpi_trends, "annual_overall_10-random-languages.rds")
