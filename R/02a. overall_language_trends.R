## script for calculating the trend for each language, adjusted for random, and then aggregated for all languages
# will need to initially weight all classes equally in in the infile, and then weight by relative species richness of each class

# set up the packages required
library(ggplot2)
library(rlpi)
library(dplyr)
library(data.table)

# read in additional functions
source("R/00. functions.R")

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the rds for total monthly views
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/total_monthly_views_10-languages.rds"))

## format for the lpi function
# rescale each dataframe to start at 1970 and merge back with the views
iucn_views_poll <- list()
for(i in 1:length(total_monthly_views)){
  iucn_views_poll[[i]] <- lapply(total_monthly_views[[i]], rescale_iucn)
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], select_comp) # select time series length
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], structure_lpi_overall) # format for lpi
}

# set up the infile with equal weightings for each class in each language, and write to txts
for(i in 1:length(languages)){
  infile_df <- list()
  for(j in 1:length(classes)){
    write.table(iucn_views_poll[[i]][[j]], paste(languages[i], classes[j], "data.txt", sep = "_"), row.names = FALSE)
    infile_df[[j]] <- data.frame(FileName = paste(languages[i], classes[j], "data.txt", sep = "_"), Group = j, Weighting = 1)
  }
  infile_df <- rbindlist(infile_df)
  write.table(infile_df, paste(languages[i], "pages_all_infile_conf.txt", sep = "_"), row.names = FALSE)
}

# run lpi trends for each language
lpi_trends <- list()
for(i in 1:length(languages)){
  lpi_trends[[i]] <- LPIMain(paste(languages[i], "pages_all_infile_conf.txt", sep = "_"), REF_YEAR = 1977, PLOT_MAX = 2033, goParallel = TRUE)
}

saveRDS(lpi_trends, "overall_10-languages_6-class-groups_equal-weight.rds")
