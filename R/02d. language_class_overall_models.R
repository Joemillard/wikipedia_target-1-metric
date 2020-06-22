# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/average_lambda")

# read in the string of languages - original order sorted alphabetically for files read in - CHECK THAT THIS SHOULD BE SORTED
languages <- sort(c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_"))

# read in the lambda files 
random_trend <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")

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

# bind together and plot the random trends
random_trend_figure <- rbindlist(random_trend) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_line(aes(x = Year, y = LPI_final, group = language)) +
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = language), alpha = 0.3) +
  scale_y_continuous("Random index", breaks = c(0.6, 1, 1.4, 1.8)) +
  scale_x_continuous(NULL, breaks = c(2016, 2017, 2018, 2019, 2020), labels = c(2016, 2017, 2018, 2019, 2020)) +
  facet_wrap(~language) +
  theme_bw() +
  theme(panel.grid = element_blank())

# string for pollinating classes
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, directory){
  
  # bring in all the files in that directory
  view_files <- list.files(directory)
  
  # set up empty list for files for each language
  user_files <- list()
  
  # set up each of the file directories
  for(i in 1:length(classes)){
    user_files[[i]] <- list.files(directory, pattern = classes[i])
    user_files[[i]] <- paste0(directory, "/", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files)
}

# run the function with 10 languages, specifying the directory
user_files <- view_directories(classes,
                               directory)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(language_views)){
  for(j in 1:length(random_trend)){
    data_file <- language_views[[i]][[j]]
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend[[j]]$lamda))
  }
  all_lambdas[[i]] <- adj_lambdas
}

# assign new column for class and filter out the extra lambda rows
model_format <- function(data_file, taxa, language){
  data_fin <- data_file %>%
    select(av_lambda, V1, Freq, SpeciesSSet) %>%
    mutate(taxonomic_class = taxa)
  return(data_fin)
}

# calculate average for each row and reformat with language and class included
average_lambda <- function(data_file, taxa){
  
  # builds subset of columns to calculate average over
  data_fin <- data_file
  data_subset <- data_file[, 5:ncol(data_fin)]
  data_fin$lambda_summed <- rowSums(data_subset)
  
  # calculate sum and average lambda and convert speciesset to character
  data_fin$av_lambda <- data_fin$lambda_summed / ncol(data_subset)
  data_fin$SpeciesSSet <- as.character(data_fin$SpeciesSSet)
  data_fin <- model_format(data_fin, taxa)
  return(data_fin)
}

# calculate the average lambda and add language/class columns
avg_lambdas <- list()
for(i in 1:length(all_lambdas)){
  avg_lambdas[[i]] <- lapply(all_lambdas[[i]], average_lambda, taxa = classes[[i]])
}

# assign final column for languae
for(i in 1:length(avg_lambdas)){
  for(j in 1:length(avg_lambdas[[i]])){
    avg_lambdas[[i]][[j]]$language <- languages[[j]]
  }
}

# bind all the lambda files into a single dataframe into a single dataframe
final_bound <- list()
for(i in 1:length(avg_lambdas)){
  final_bound[[i]] <- rbindlist(avg_lambdas[[i]])
}

# rbind together the final dataframes
final_bound <- rbindlist(final_bound)

# build the models - to work on!!
model_1 <- lm(av_lambda ~ taxonomic_class * language, data = final_bound)
summary(model_1)



predicted_values <- predict(model_1, final_bound, se.fit = TRUE)

final_bound$predicted_values <- predicted_values$fit
final_bound$predicted_values_se <- predicted_values$se.fit

fin_frame_6 <- final_bound %>%
  dplyr::select(taxonomic_class, predicted_values, predicted_values_se) %>%
  unique()

lambda_overall <- fin_frame_6 %>%
  mutate(class = factor(taxonomic_class, levels = c("insects", "amphibians", "actinopterygii", "birds", "mammals", "reptiles"), labels = c("Insects", "Amphibians", "Ray-finned fishes", "Birds", "Mammals", "Reptiles"))) %>%
  ggplot() + 
  geom_errorbar(aes(x = taxonomic_class, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se))), width = 0.3) +
  geom_point(aes(x = taxonomic_class, y = predicted_values)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  ylab("SAI average monthly lambda") +
  xlab(NULL) +
  theme_bw() +
  theme(legend.position = "none")
