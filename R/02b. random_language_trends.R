## script for calculating the trend for each language, adjusted for random, and then aggregated for all languages
# will need to initially weight all classes equally in in the infile, and then weight by relative species richness of each class

# set up the packages required
library(ggplot2)
library(rlpi)
library(dplyr)
library(data.table)
library(boot)

# read in additional functions
source("R/00. functions.R")

# set up vector for languages, classes, and directory
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
               
# read in the rds for total monthly views
total_monthly_views <- readRDS("Z:/submission_2/average_daily_views_random_10-languages.rds")

# set up vectors of wiki project class to remove any animal species from the random data
wiki_proj <- paste(c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en"), "wiki", sep = "")
taxa_groups <- c("ACTINOPTERYGII", "AMPHIBIA", "AVES", "INSECTA", "MAMMALIA", "REPTILIA")

# read in the biodiversity pages
biodiversity_pages <- read.csv(here::here("data/all_iucn_titles.csv"), encoding = "UTF-8") %>%
  filter(site %in% wiki_proj) %>%
  filter(class_name %in% taxa_groups) %>%
  select(title, site, class_name) %>%
  unique() %>%
  mutate(site = factor(site, levels = wiki_proj)) %>%
  arrange(site)

# filter the biodiversity pages for the set of languages we're using, then split up, and sort by random languages list
split_biodiversity_pages <- split(biodiversity_pages, biodiversity_pages$site)

# filter all pages from random that are species pages
filter_species <- function(data_file, split_biodiversity_pages){
  data_fin <- anti_join(data_file, split_biodiversity_pages, by = c("article" = "title")) %>%
    rename("wikipedia_id" = "q_wikidata")
  return(data_fin)
}

# merge the random species title with the all species list for each language to remove species from random - antijoin to remove those in both
for(i in 1:length(total_monthly_views)){
  total_monthly_views[[i]] <- filter_species(data_file = total_monthly_views[[i]], 
                                      split_biodiversity_pages = split_biodiversity_pages[[i]]) %>%
    select(article, wikipedia_id, year, month, av_views, date)
}

## format for the lpi function
# rescale each dataframe to start at 1970 and merge back with the views
#iucn_views_poll <- list()
iucn_views_poll <- lapply(total_monthly_views, rescale_iucn)
iucn_views_poll <- lapply(iucn_views_poll, select_comp) # select time series length
iucn_views_poll <- lapply(iucn_views_poll, structure_lpi_overall) # format for lpi

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
  lpi_trends[[i]] <- LPIMain(paste(languages[i], "random_all_infile_conf.txt", sep = "_"), REF_YEAR = 1977, PLOT_MAX = 2033, goParallel = TRUE)
}

# save the rds file for the year values
saveRDS(lpi_trends, "Z:/submission_2/overall_daily-views_10-random-languages_no-species.rds")
random_trend <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_no-species.rds")

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

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, languages, directory){
  
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
run_each_group <- function(lambda_files, random_trend){
  
  # Bootstrap these to get confidence intervals
  dbi.boot <- boot(lambda_files, create_lpi, R = 1000)
  
  # Construct dataframe and get mean and 95% intervals
  boot_res <- data.frame(LPI = dbi.boot$t0)
  boot_res$Year <- random_trend$Year[1:(nrow(random_trend))]
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.975), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.025), na.rm = TRUE)
  return(boot_res)
}

## read in the lambda files to build the average random trend for each language
# run the function with 10 languages, specifying the directory
user_files <- view_directories(classes = "random",
                               languages = languages,
                               directory = here::here("data/lambdas/no_species_random"))

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# run the boostrapping of trends for each lambda, and adjust for the random of that language
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(language_views[[1]])){
    lpi_trends_adjusted[[i]] <- run_each_group(language_views[[1]][[i]], random_trend[[i]]) %>%
      mutate(language = random_trend[[i]]$language) %>%
      select(LPI, LPI_lwr, LPI_upr) %>%
      rename("LPI_final" = "LPI") %>%
      rename("CI_low" = "LPI_lwr") %>%
      rename("CI_high" = "LPI_upr")
    
    rownames(lpi_trends_adjusted[[i]]) <- 1977:2033
}
  
# resave the average lambda for random views, according to bootstrap method used throughout
saveRDS(lpi_trends_adjusted, "Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")
