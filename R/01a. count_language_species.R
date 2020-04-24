## script for calculating number of species (and number of complete time series) in each language and plotting
library(dplyr)

# read in the rds for total monthly views
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/total_monthly_views_10-languages.rds"))

# function for counting the number of species in each class/language grouping
count_total_species <- function(data_file){
  data_fin <- data_file %>%
    select(article) %>%
    unique() %>%
    tally()
  return(data_fin)
}

total_species <- list()
for(i in 1:length(total_monthly_views)){
  total_species[[i]] <- lapply(total_monthly_views[[i]], count_total_species)
}

count_complete_species <- function(data_file){
  data_file %>%
    group_
}