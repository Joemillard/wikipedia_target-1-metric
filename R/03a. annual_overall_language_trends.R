# set up the packages required
library(ggplot2)
library(rlpi)
library(dplyr)
library(data.table)

# read in additional functions
source("R/00. functions.R")

# read in the rds for total monthly views
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/total_monthly_views_10-languages.rds"))

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

## format for the lpi function
# select complete time series (57 months), calculate the annual total views, and rescale each dataframe to start at 1970 for lpi
iucn_views_poll <- list()
for(i in 1:length(total_monthly_views)){
  iucn_views_poll[[i]] <- lapply(total_monthly_views[[i]], select_comp) # select time series length
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], sum_annual) # sum for each year
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], reformat_annual) # reformat for the rlpi code, series running 1970-1973
}

# set up the infile with equal weightings for each class in each language, and write to txts
for(i in 1:length(languages)){
  infile_df <- list()
  for(j in 1:length(classes)){
    write.table(iucn_views_poll[[i]][[j]], paste(languages[i], classes[j], "annual_data.txt", sep = "_"), row.names = FALSE)
    infile_df[[j]] <- data.frame(FileName = paste(languages[i], classes[j], "annual_data.txt", sep = "_"), Group = j, Weighting = 1)
  }
  infile_df <- rbindlist(infile_df)
  write.table(infile_df, paste(languages[i], "annual_pages_all_infile_conf.txt", sep = "_"), row.names = FALSE)
}

# run lpi trends for each language
lpi_trends <- list()
for(i in 1:length(languages)){
  lpi_trends[[i]] <- LPIMain(paste(languages[i], "annual_pages_all_infile_conf.txt", sep = "_"), REF_YEAR = 1970, PLOT_MAX = 1973, goParallel = TRUE)
}

saveRDS(lpi_trends, "annual_overall_10-languages_6-class-groups_equal-weight.rds")



