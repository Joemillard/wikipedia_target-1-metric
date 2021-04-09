## script for counting the number of random views in analysis, and write total monthly views to rds
library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)
library(parallel)

# read in additional functions
source("R/00. functions.R")

# set up vector for languages, classes, and directory - random views read in in two chunks
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
directory <- "Z:/submission_2/user_trends/random_views/"
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

# calculate total views for all languages
total_views <- function(data_file){
  view_total <- 0
  for(i in 1:length(data_file)){
    for(j in 1:length(data_file[[i]])){
      view_total <- view_total + sum(data_file[[i]][[j]]$views, na.rm = TRUE)
    }
  }
  print(view_total)
}

# run function for total views
total_views(language_views) # 2830289115 (2.83 billion)

# calculate total views for each language
group_views <- function(data_file){
  language_total <- c(rep(0, 10))
  for(i in 1:length(language_views)){
    for(j in 1:length(language_views[[i]])){
      language_total[[i]] <- language_total[[i]] + sum(language_views[[i]][[j]]$views, na.rm = TRUE)
    }
  }
  
  # build dataframe for views for each language and return it
  language_total <- data.frame("language" = languages, "views" = language_total)
  return(language_total)
}

# run function for each language views and build bar plot
plot_views <- group_views(language_views) %>%
  mutate(language = factor(language, levels = languages, 
                           labels = c("Spanish", "French", "German", "Japanese", "Italian", 
                                      "Arabic", "Russian", "Portuguese", "Chinese", "English"))) %>%
  mutate(language = fct_reorder(language, -views)) %>%
  ggplot() +
  geom_bar(aes(x = language, y = views), stat = "identity") + 
  geom_text(aes(x = language, y = views + 23000000, label = (round(views/1000000, digits = 2)))) +
  xlab("Wiki project (language)") +
  ylab("Total user views (millions)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800000000), 
                     breaks = c(0, 200000000, 400000000, 600000000, 800000000), labels = c("0", "200", "400", "600", "800")) +
  theme_bw() +
  theme(panel.grid = element_blank())

# save the plot
ggsave("outputs/all_random-views_languages.png", dpi = 350, scale = 1)

## script to calculate total monthly views and write to rds
# **** check corrected for only pages with complete trends ****
# filter NAs from timestamp
NA_timestamp <- function(data_file){
  data_fin <- data_file %>%
    filter(!is.na(timestamp))
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

# filter NA rows (timestamps) from each set of views and count new number of articles in each grouping
language_views_monthly <- list()
for(i in 1:length(language_views)){
  language_views_monthly[[i]] <- lapply(language_views[[i]], NA_timestamp)
  lapply(language_views_monthly[[i]], count_articles)
}

# calculate total monthly views for each set of views and count new number of articles in each grouping
for(i in 1:length(language_views_monthly)){
  language_views_monthly[[i]] <- lapply(language_views_monthly[[i]], run_dat, av_all = FALSE)
  lapply(language_views_monthly[[i]], count_articles)
}

# add names for languages and class to each element of the list
names(language_views_monthly) <- languages
for(i in 1:length(language_views_monthly)){
  names(language_views_monthly[[i]]) <- random_sets
}

# bind each pair of random views together, and check that the number of rows is the same after uniquing the bound version
language_views_monthly_bound <- list()
for(i in 1:length(language_views_monthly)){
  language_views_monthly_bound[[i]] <- rbindlist(language_views_monthly[[i]])
  print(nrow(language_views_monthly_bound[[i]]))
  language_views_monthly_bound[[i]] %>% 
    select(q_wikidata) %>% 
    unique() %>%
    tally() %>% 
    print()
}

# save total monthly views as an rds
saveRDS(language_views_monthly_bound, "Z:/submission_2/total_monthly_views_random_10-languages.rds")
saveRDS(language_views_monthly_bound, "Z:/submission_2/average_daily_views_random_10-languages.rds")

# read back in the total monthly views - note need to remove those appearing in the species trends removed
# removing pages occuring in random trend
total_random_views <- readRDS("Z:/submission_2/total_views/total_monthly_views_random_10-languages.rds")

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
total_random_views_2 <- list()
for(i in 1:length(total_random_views)){
  total_random_views_2[[i]] <- filter_species(data_file = total_random_views[[i]], 
                                             split_biodiversity_pages = split_biodiversity_pages[[i]]) %>%
    select(article, wikipedia_id, year, month, av_views, date)
}

# calculate the number of views without the random pages that are in the species trend
calc_views <- function(data_file){
  data_fin <- data_file %>%
    summarise(total_views = sum(av_views))
  return(data_fin)
}

# build new plot for the number of views after removing the random pages that are in the species trend
views_remove_species <- lapply(total_random_views_2, calc_views) %>%
  rbindlist() %>% sum()
  mutate(language = languages)

# run function for each language views and build bar plot
plot_views <- views_remove_species %>%
  mutate(language = factor(language, levels = languages, 
                           labels = c("Spanish", "French", "German", "Japanese", "Italian", 
                                      "Arabic", "Russian", "Portuguese", "Chinese", "English"))) %>%
  mutate(language = fct_reorder(language, -total_views)) %>%
  ggplot() +
  geom_bar(aes(x = language, y = total_views), stat = "identity") + 
  geom_text(aes(x = language, y = total_views + 23000000, label = (round(total_views/1000000, digits = 2)))) +
  xlab("Wiki project (language)") +
  ylab("Total user views (millions)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800000000), 
                     breaks = c(0, 200000000, 400000000, 600000000, 800000000), labels = c("0", "200", "400", "600", "800")) +
  theme_bw() +
  theme(panel.grid = element_blank())

# save plot for species removed from random trend
ggsave("outputs/random_views_removed-species.png", dpi = 350, scale = 1)

# plot for number of complete series for the random trends
## plot for complete time series
# count number of months of views per article
count_months <- function(data_file){
  data_fin <- data_file %>%
    group_by(article) %>%
    tally()
  
  return(data_fin)
}

# run function to count number of months of views per article
count_number_months <- lapply(total_random_views_2, count_months)

# count number of complete series (i.e. with 57 months) for each language/class combination, and concert to proportion
series_frame <- list()
for(i in 1:length(count_number_months)){
  counter <- 0 # set up the counter
  total_number <- length(count_number_months[[i]]$n) # calculate number of species for that class, and count through each species
  for(k in 1:length(count_number_months[[i]]$n)){
    if(count_number_months[[i]]$n[k] == 57){
      counter <- counter + 1 # if complete series, add one to counter
    }
  }
  proportion <- counter / total_number # calculate the proportion of complete series for that class
  series_frame[[i]] <- data.frame("languages"= languages[i], counter, total_number, proportion) # print the class, number of complete series, the total number of articles, and proportion complete
}

# plot the complete series for random pages
all_series_frame <- rbindlist(series_frame) %>% 
  mutate(languages = factor(languages, levels = languages, labels = c("Spanish", "French", "German", "Japanese", "Italian", 
                                                 "Arabic", "Russian", "Portuguese", "Chinese", "English"))) %>%
  mutate(languages = fct_reorder(languages, -counter)) %>%
  ggplot() +
    geom_bar(aes(x = languages, y = total_number), position = "identity", stat = "identity") +
    geom_bar(aes(x = languages, y = counter, fill = "Complete series (57 months)"), position = "identity", stat = "identity") +
    scale_y_continuous("Total random pages", expand = c(0, 0), limits = c(0 , 12000)) +
    scale_fill_manual("", values = c("red")) +
    xlab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
      panel.grid = element_blank(), 
      axis.title.y = element_text(size = 13, vjust = 0.9))

ggsave("outputs/random_complete_series.png", scale = 1, dpi = 350)
