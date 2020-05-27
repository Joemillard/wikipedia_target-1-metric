## script for counting the number of views in analysis, and write total monthly views to rds
# **** check corrected for only pages with complete trends ****
library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)
library(parallel)

# read in additional functions
source("R/00. functions.R")

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
directory <- "J:/submission_2/user_trends/"
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
total_views(language_views_edit) # 2227539617 (2.23 billion)

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
plot_views <- group_views(language_views_edit) %>%
  mutate(language = factor(language, levels = languages, 
                           labels = c("Spanish", "French", "German", "Japanese", "Italian", 
                                      "Arabic", "Russian", "Portuguese", "Chinese", "English"))) %>%
  mutate(language = fct_reorder(language, -views)) %>%
  ggplot() +
    geom_bar(aes(x = language, y = views), stat = "identity") + 
    geom_text(aes(x = language, y = views + 28000000, label = (round(views/1000000, digits = 2)))) +
    xlab("Wiki project (language)") +
    ylab("Total user views (millions)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1200000000), 
                       breaks = c(0, 300000000, 600000000, 900000000, 1200000000), labels = c("0", "300", "600", "900", "1200")) +
    theme_bw() +
    theme(panel.grid = element_blank())

# save the plot
ggsave("outputs/species_views_languages.png", dpi = 350, scale = 1)

## script to calculate total monthly views and write to rds
# filter NAs from timestamp
NA_timestamp <- function(data_file){
  data_fin <- data_file %>%
    filter(!is.na(timestamp))
  return(data_fin)
}

# filter NA rows (timestamps) from each set of views 
language_views_monthly <- list()
for(i in 1:length(language_views_edit)){
  language_views_monthly[[i]] <- lapply(language_views_edit[[i]], NA_timestamp)
}

# calculate total monthly views for each set of views
for(i in 1:length(language_views_monthly)){
  language_views_monthly[[i]] <- lapply(language_views_monthly[[i]], run_dat, av_all = FALSE)
}

# add names for languages and class to each element of the list
names(language_views_monthly) <- languages
for(i in 1:length(language_views_monthly)){
  names(language_views_monthly[[i]]) <- classes
}

# save total monthly views as an rds
saveRDS(language_views_monthly, "J:/submission_2/total_monthly_views_10-languages.rds")




#




















# read in class level lambdas
bird <- read.csv("data/birds_data_conf_lambda.csv", stringsAsFactors = FALSE)
insect <- read.csv("data/insects_data_conf_lambda.csv", stringsAsFactors = FALSE)
mammal <- read.csv("data/mammals_data_conf_lambda.csv", stringsAsFactors = FALSE)

# taxonomic classes as list
class_lambdas <- list(bird, insect, mammal)

# count number of days for each user trend
bird_views <- read.csv("data/bird_user_trends.csv", stringsAsFactors = FALSE)
mammal_views <- read.csv("data/mammal_user_trends.csv", stringsAsFactors = FALSE)
insect_views <- read.csv("data/insect_user_trends.csv", stringsAsFactors = FALSE)
random_monthly_trends_init <- read.csv("data/random_user_trends.csv", stringsAsFactors = FALSE)

length(unique(bird_views$timestamp))

# read in pollinator data from lambdas
bird_N <- read.csv("data/bird_N_data_lambda.csv", stringsAsFactors = FALSE)
bird_Y <- read.csv("data/bird_Y_data_lambda.csv", stringsAsFactors = FALSE)
mammal_N <- read.csv("data/mammal_N_data_lambda.csv", stringsAsFactors = FALSE)
mammal_Y <- read.csv("data/mammal_Y_data_lambda.csv", stringsAsFactors = FALSE)
insect_N <- read.csv("data/insect_N_data_lambda.csv", stringsAsFactors = FALSE)
insect_Y <- read.csv("data/insect_Y_data_lambda.csv", stringsAsFactors = FALSE)

lambdas <- list(bird_N, bird_Y, mammal_N, mammal_Y, insect_N, insect_Y)

print_complete_series <- function(data){
  data <- data[complete.cases(data),]
  print(nrow(data))
}

lapply(lambdas, print_complete_series)
lapply(class_lambdas, print_complete_series)

r_views <- sum(random_monthly_trends_init$views, na.rm = TRUE)
b_views <- sum(bird_views$views)
m_views <- sum(mammal_views$views)
i_views <- sum(insect_views$views)

b_views + m_views + i_views + r_views

bird_spec <- length(unique(bird_views$article))
mammal_spec <- length(unique(mammal_views$article))
insect_spec <- length(unique(insect_views$article))
rand_spec <- length(unique(random_monthly_trends_init$article))

bird_spec + mammal_spec + insect_spec + rand_spec

## plot for number of pollinating species
pollinators <- data.frame("class" = rep(c("Birds", "Insects", "Mammals"), 2), 
                          "pollinating" = c(rep(c("Y"), 3), rep(c("N"), 3)),
                          "count" = c(1289, 497, 655, 7147, 1202, 3828))

# plot for pollinators with wiki data and complete time series
pollinators %>%
  mutate(pollinating = factor(pollinating, levels = c("Y", "N"), labels = c("Yes", "No"))) %>%
  ggplot() +
    geom_bar(aes(x = NA, fill = pollinating, y = count), stat = "identity") +
    facet_wrap(~class) +
    scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
    ylab("Total species") +
    xlab("") +
    theme_bw() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 8900)) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("pollinator_plot.png", scale = 1, dpi = 350)
