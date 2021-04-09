# potentially add script to separate between smoothed and non-smoothed lambdas
# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(boot)
library(RColorBrewer)
library(viridis)
library(forcats)
library(cowplot)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/lambdas/species")

# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")

# read in the lambda files 
random_trend <- readRDS("data/lambdas/no_species_random/overall_daily-views_10-random-languages_from_lambda_no-species.rds")

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
rbindlist(random_trend) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI_final, group = language)) +
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = language), alpha = 0.3) +
  facet_wrap(~language) +
  theme_bw()

# string for pollinating classes, plus random
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random_data")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, directory){
  
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
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend[[j]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

#### additional smoothing of the random adjusted indices

# smooth the adjusted random lambda for each species
# iterate through all the articles of that class/language
smooth_series <- function(X){
  
  # create index
  index <- cumprod(10^c(0, X))

  # smooth the index
  x_range <- 1:length(index)
  y.loess <- loess(index~x_range, span = 0.30)
  data_fin <- predict(y.loess, data.frame(x_range))
  return(data_fin)
}

# convert the index back to lambda
create_lambda <- function(X){
  lambda <- c(1, diff(log10(X)))
  return(lambda)
}

# convert back to index, run the smooth for random adjusted lambda, and then convert back the lamda
smooth_all_groups <- function(data_file){
  
  # set up an empty list for smoothed values
  smoothed_indices <- list()
  
  # smooth the series for each row (species)
  for(i in 1:nrow(data_file)){
    smoothed_indices[[i]] <- smooth_series(X = as.numeric(as.vector(data_file[i, 5:ncol(data_file)])))
    smoothed_indices[[i]] <- create_lambda(smoothed_indices[[i]])
  }

  smoothed_lambda <- as.data.frame(do.call(rbind, smoothed_indices))
  
  # add back in the original column names
  colnames(smoothed_lambda) <- colnames(data_file)[4:ncol(data_file)]
  
  # bind the adjusted smoothed lambda back onto the first four columns
  smoothed_lambda <- cbind(data_file[,1:3], smoothed_lambda)

  return(smoothed_lambda)

}

# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}

###

# reassign correct names for each element in list
names(smoothed_adjusted_lamda) <- (c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random_data"))
for(i in 1:length(smoothed_adjusted_lamda)){
  names(smoothed_adjusted_lamda[[i]]) <- c("es", "fr", "de", "ja", "it", "ar", "ru", "pt", "zh", "en")
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

# run the boostrapping of trends for each lambda
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  for(j in 1:length(smoothed_adjusted_lamda[[i]])){
    lpi_trends_adjusted[[j]] <- run_each_group(smoothed_adjusted_lamda[[i]][[j]], random_trend[[j]]) %>%
      mutate(language = random_trend[[j]]$language)
    
  }
  
  # bind together the trends for that language
  bound_trends[[i]] <- rbindlist(lpi_trends_adjusted) %>%
    mutate(taxa = classes[i])
}

# bind together the trend for all languages
fin_bound_trends <- rbindlist(bound_trends)

formatter <- function(...){
  function(x) format(round(x, 1), ...)
}

# plot all the class level trends
fin_bound_trends %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(taxa != "random_data") %>%
  mutate(taxa = factor(taxa, levels = c("reptilia", "actinopterygii", "mammalia", "aves", "insecta", "amphibia"),
                       labels = c("Reptiles", "Ray finned fishes", "Mammals", "Birds", "Insects", "Amphibians"))) %>%
  mutate(language = factor(language, levels = c("\\^ar_", "\\^zh_", "\\^en_", "\\^fr_", "\\^de_", "\\^it_", "\\^ja_", "\\^pt_", "\\^ru_", "\\^es_"),
                           labels = c("Arabic", "Chinese", "English", "French", "German", "Italian", "Japanese", "Portuguese", "Russian", "Spanish"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = taxa), alpha = 0.4) +
  geom_line(aes(x = Year, y = LPI, colour = taxa)) +
  geom_hline(yintercept = 1, linetype = "88", size = 0.3, alpha = 0.3) +
  scale_fill_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
  scale_colour_manual("Taxonomic class", values = c("black", "#FF7F00", "#377EB8", "#4DAF4A", "#F781BF", "#A65628")) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5), labels = function(x) ifelse(x == 0, "0", x)) +
  facet_grid(language~taxa) +
  ylab("Species Awareness Index (SAI)") +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 13, vjust = 2),
        strip.text.y = element_text(size = 11, angle = 45),
        strip.text.x = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11))

ggsave("outputs/random_adjusted_class_SAI_free_1000_95_no-random-species_smoothed.png", scale = 1.5, dpi = 350)