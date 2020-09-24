# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(boot)
library(forcats)
library(cowplot)
library(patchwork)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/average_lambda")

# read in the species unique to each language
unique_species <- read.csv(here::here("data/class_wiki_indices/submission_2/one_language_species.csv"))

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, directory, languages){
  
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
    user_files[[i]] <- user_files[[i]][user_files[[i]] %in% file_order]
    user_files[[i]] <- user_files[[i]][order(match(user_files[[i]], file_order))]
    user_files_dir[[i]] <- paste0(directory, "/", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files_dir)
}

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

# function for binding all the lambdas together and calculate average for each q_wikidata
wiki_average <- function(data_file){
  data_fin <- data_file %>%
    reshape2::melt(id = c("q_wikidata", "SpeciesSSet", "Freq", "V1", "language", "taxa", "article")) %>%
    mutate(variable = as.character(variable)) %>%
    group_by(q_wikidata, variable, taxa) %>%
    summarise(mean_val = mean(value)) %>%
    ungroup()
  return(data_fin)
}

# function for a user weighted average
wiki_average_weight <- function(data_file){
  data_fin <- data_file %>%
    reshape2::melt(id = c("q_wikidata", "SpeciesSSet", "Freq", "V1", "language", "taxa", "article", "prop")) %>%
    mutate(variable = as.character(variable)) %>%
    group_by(q_wikidata, variable, taxa) %>%
    summarise(mean_val = weighted.mean(value, prop)) %>%
    ungroup()
  return(data_fin)
}

jamGeomean <- function(x, na.rm = TRUE, ...){
  ## Purpose is to calculate geometric mean while allowing for
  ## positive and negative values
  x2 <- mean(log2(1 + abs(x)) * sign(x));
  sign(x2) * (2 ^ abs(x2) - 1);
}

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  
  # remove na rows
  lambdas_new <- lambdas[complete.cases(lambdas), ]
  
  # select columns from lambda file to calculate mean, and build a cumprod trend
  lambda_data <- lambdas_new[, 4:ncol(lambdas_new)]
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

# rerun the above overall index with a weighted proportion for users
# first calculate the weighted proportions and add to dataframe
# interent users from https://www.internetworldstats.com/stats7.htm
# and from https://www.statista.com/statistics/828259/predicted-internet-user-penetration-rate-in-italy/ and https://www.istat.it/ for italy
# italy = 60.24 million * 0.7193 (2020 internet penetration)

# internet users by each langauge, equivalent to 73.5 of total users
internet_users <- data.frame("Language" = c("\\^en_",
                                            "\\^zh_",
                                            "\\^es_",
                                            "\\^ar_",
                                            "\\^pt_",
                                            "\\^fr_",
                                            "\\^ja_",
                                            "\\^ru_",
                                            "\\^de_",
                                            "\\^it_"),
                             "Users" = c(1186451052,
                                         888453068,
                                         363684593,
                                         237418349,
                                         171750818,
                                         151733611,
                                         118626672,
                                         116353942,
                                         92525427,
                                         43330632))

# total internet users
total_users <- 4585578718

# proportion of total users for our 10 languages - include in manuscript
sum(internet_users$Users)/total_users # 73.5%

# convert the number of users to proportion
internet_users$users_total_10 <- sum(internet_users$Users)
internet_users$prop <- internet_users$Users/internet_users$users_total_10

# remove extra columsn with exception of proportion
internet_users <- internet_users %>%
  select(Language, prop)

# set up language and class vectors
languages_orig <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")
lpi_trends_adjusted <- list()

for(l in 1:length(languages_orig)){
  
  # read in the rds for total monthly views to retrieve the lambda ids
  average_monthly_views <- readRDS("Z:/submission_2/daily_average_views_10-languages.rds")
  average_monthly_views[[l]] <- NULL # remove ones wikipedia in stepwise manner

  ## format for the lpi function
  # rescale each dataframe to start at 1970 and merge back with the views, then output lpi structure with original id
  iucn_views_poll <- list()
  for(i in 1:length(average_monthly_views)){
    iucn_views_poll[[i]] <- lapply(average_monthly_views[[i]], rescale_iucn)
    iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], select_comp) # select time series length
    iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], function(x){
      data_fin <- x %>%
        select(article, q_wikidata, dec_date, av_views) %>%
        mutate(SpeciesSSet = as.character(as.numeric(as.factor(article)))) %>%
        filter(complete.cases(.)) %>%
        select(q_wikidata, SpeciesSSet, article) %>%
        unique() %>%
        mutate(SpeciesSSet = as.character(SpeciesSSet))
      return(data_fin)
    })
  }

  # read in the string of languages and taxa - original order sorted alphabetically for files read in - exclude french wikipedia
  languages <- languages_orig[-l]

  # read in the lambda files 
  random_trend <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")
  random_trend[[l]] <- NULL

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

  # run the function with 10 languages, specifying the directory
  user_files <- view_directories(classes,
                               directory,
                               languages)

  # read in all the files in groups for each language
  language_views <- list()
  system.time(for(i in 1:length(user_files)){
    language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
  })

  # adjust the lambdas for each species for each language with random, and conert speciesset to character for merging
  adj_lambdas <- list()
  all_lambdas <- list()
  data_file <- list()
  for(i in 1:length(language_views)){
    for(j in 1:length(random_trend)){
      data_file <- language_views[[i]][[j]] %>%
        mutate(SpeciesSSet = as.character(SpeciesSSet))
      adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend[[j]]$lamda, FUN = "-"))
    }
    all_lambdas[[i]] <- adj_lambdas
  }

  #### additional smoothing of the random adjusted indices
  # run the smoothing of lamdas over each class/language combination
  smoothed_adjusted_lamda <- list()
  for(i in 1:length(all_lambdas)){
    smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
    print(i)
  }
  ####

  # merge each lambda file with the speciesSSet ID from view data
  merge_lambda <- list()
  merge_fin_lambda <- list()
  for(i in 1:length(smoothed_adjusted_lamda)){
    for(j in 1:length(smoothed_adjusted_lamda[[i]])){
      merge_lambda[[j]] <- inner_join(smoothed_adjusted_lamda[[i]][[j]], iucn_views_poll[[j]][[i]], by = "SpeciesSSet") %>%
        mutate(taxa = classes[i]) %>%
        mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
      print(nrow(smoothed_adjusted_lamda[[i]][[j]]) - nrow(merge_lambda[[j]]))
      merge_lambda[[j]] <- merge_lambda[[j]] %>%
        filter(!q_wikidata %in% unique_species$q_wikidata)
      
    }
    merge_fin_lambda[[i]] <- merge_lambda
  }

  # rbindlist all lambda together and calculate averge for each species across languages
  merge_species <- list()
  for(i in 1:length(merge_fin_lambda)){
    merge_species[[i]] <- rbindlist(merge_fin_lambda[[i]])
  }

  # merge all the lambda files, and calc average across each q_wikidata
  merge_species <- rbindlist(merge_species) %>% 
    inner_join(internet_users, by = c("language" = "Language")) %>%
    wiki_average_weight()

  # reshape lambda files back into year rows, and then split into separate taxonomic classes
  all_lambdas <- reshape2::dcast(merge_species, q_wikidata + taxa ~ variable)

  # run the boostrapping of trends for all lambda, and adjust for the random of that language
  lpi_trends_adjusted[[l]] <- run_each_group(all_lambdas, random_trend = random_trend[[1]])
}

# add the language jack-knifed for each grouping
bound_trends <- list()
for(i in 1:length(lpi_trends_adjusted)){
  bound_trends[[i]] <- lpi_trends_adjusted[[i]] %>%
    mutate(language_jack = languages_orig[i])
}

# collapse together the average lambda at each start point for ecah class, add last row for value 57, and then stick LPI values back on
jack_knifed_class <- rbindlist(bound_trends) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(language_jack = factor(language_jack, levels = c("\\^ar_", "\\^zh_", "\\^en_", "\\^fr_", "\\^de_", "\\^it_", "\\^ja_", "\\^pt_", "\\^ru_", "\\^es_"),
                                labels = c("Arabic", "Chinese", "English", "French", "German", "Italian", "Japanese", "Portuguese", "Russian", "Spanish"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = language_jack), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI, colour = language_jack)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_fill_manual("Excluded language", values = c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) +
  scale_colour_manual("Excluded language", values = c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) +
  scale_y_continuous(breaks = c(1.05, 1, 0.95, 0.9, 0.85, 0.8), labels = c("1.05","1", "0.95", "0.9", "0.85", "0.8")) +
  ylab("User-weighted Species Awareness Index (SAI)") +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank())

# save the jack-knifed plot for supp material
ggsave("overall_index_jack_1000_95_equal-weight_no-unique-spec.png", scale = 0.9, dpi = 350)

## after jack-knifing, remove the language/languages that have a big influence on the overall index

# read in the string of languages and taxa - original order sorted alphabetically for files read in - exclude french wikipedia
languages <- c("\\^es_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in daily average to retrieve q_wikidata id
average_monthly_views <- readRDS("Z:/submission_2/daily_average_views_10-languages.rds")
average_monthly_views[[2]] <- NULL # remove french wikipedia as overly influences index

iucn_views_poll <- list()
for(i in 1:length(average_monthly_views)){
  iucn_views_poll[[i]] <- lapply(average_monthly_views[[i]], rescale_iucn)
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], select_comp) # select time series length
  iucn_views_poll[[i]] <- lapply(iucn_views_poll[[i]], function(x){
    data_fin <- x %>%
      select(article, q_wikidata, dec_date, av_views) %>%
      mutate(SpeciesSSet = as.character(as.numeric(as.factor(article)))) %>%
      filter(complete.cases(.)) %>%
      select(q_wikidata, SpeciesSSet, article) %>%
      unique() %>%
      mutate(SpeciesSSet = as.character(SpeciesSSet))
    return(data_fin)
  })
}

# read in the lambda files 
random_trend <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")
random_trend[[2]] <- NULL

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

# run the function with 10 languages, specifying the directory
user_files <- view_directories(classes,
                               directory,
                               languages)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# adjust the lambdas for each species for each language with random, and conert speciesset to character for merging
adj_lambdas <- list()
all_lambdas <- list()
data_file <- list()
for(i in 1:length(language_views)){
  for(j in 1:length(random_trend)){
    data_file <- language_views[[i]][[j]] %>%
      mutate(SpeciesSSet = as.character(SpeciesSSet))
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend[[j]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

#### additional smoothing of the random adjusted indices
# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}
####

# merge each lambda file with the speciesSSet ID from view data
merge_lambda <- list()
merge_fin_lambda <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  for(j in 1:length(smoothed_adjusted_lamda[[i]])){
    merge_lambda[[j]] <- inner_join(smoothed_adjusted_lamda[[i]][[j]], iucn_views_poll[[j]][[i]], by = "SpeciesSSet") %>%
      mutate(taxa = classes[i]) %>%
      mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
    print(nrow(smoothed_adjusted_lamda[[i]][[j]]) - nrow(merge_lambda[[j]]))
    merge_lambda[[j]] <- merge_lambda[[j]] %>%
      filter(!q_wikidata %in% unique_species$q_wikidata)
  }
  merge_fin_lambda[[i]] <- merge_lambda
}

# rbindlist all lambda together and calculate averge for each species across languages
merge_species <- list()
for(i in 1:length(merge_fin_lambda)){
  merge_species[[i]] <- rbindlist(merge_fin_lambda[[i]])
}

# merge all the lambda files, and calc average across each q_wikidata
merge_species_bound <- rbindlist(merge_species) %>% 
  wiki_average()

# reshape lambda files back into year rows, and then split into separate taxonomic classes
all_lambdas <- reshape2::dcast(merge_species_bound, q_wikidata + taxa ~ variable)

# run the boostrapping of trends for all lambda
lpi_trends_adjusted <- run_each_group(all_lambdas, random_trend = random_trend[[1]])

# calculate average lambda, starting at each month, and assign factor for whether average is increasing or decreasing
language_frame <- list()
for(i in 1:56){
  language_frame[[i]] <- lpi_trends_adjusted %>%
    filter(row_number() %in% c(i:57)) %>%
    mutate(lambda = c(0, diff(log10(LPI)))) %>%
    mutate(average_lambda = mean(lambda)) %>% 
    select(average_lambda, Year, LPI, LPI_upr, LPI_lwr) %>%
    unique() %>%
    mutate(factor_rate = factor(ifelse(average_lambda >= 0, "increasing/stable", "decreasing"))) %>% 
    mutate(series_start = i)
}
  
# collapse together the average lambda at each start point for ecah class, add last row for value 57, and then stick LPI values back on
all_class_no_french <- rbindlist(language_frame) %>%
  select(series_start, average_lambda, factor_rate) %>%
  unique() %>% 
  bind_rows(data.frame("average_lambda" = NA,
                        "factor_rate" = NA,
                        "series_start" = 57)) %>%
  mutate(Year = random_trend[[1]]$Year) %>%
  mutate(LPI = lpi_trends_adjusted$LPI) %>%
  mutate(LPI_upr = lpi_trends_adjusted$LPI_upr) %>%
  mutate(LPI_lwr = lpi_trends_adjusted$LPI_lwr) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing/stable", "decreasing"), labels = c("Increasing or stable", "Decreasing"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  ylab("Unweighted Species Awareness Index (SAI)") +
  scale_y_continuous(breaks = c(1.08, 1.04, 1, 0.96), labels = c("1.08", "1.04","1", "0.96")) +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 12, vjust = 2))

# read in the string of languages and taxa - original order sorted alphabetically for files read in - exclude french wikipedia
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in daily average to retrieve q_wikidata id
average_monthly_views_weight <- readRDS("Z:/submission_2/daily_average_views_10-languages.rds")

iucn_views_poll_weight <- list()
for(i in 1:length(average_monthly_views_weight)){
  iucn_views_poll_weight[[i]] <- lapply(average_monthly_views_weight[[i]], rescale_iucn)
  iucn_views_poll_weight[[i]] <- lapply(iucn_views_poll_weight[[i]], select_comp) # select time series length
  iucn_views_poll_weight[[i]] <- lapply(iucn_views_poll_weight[[i]], function(x){
    data_fin <- x %>%
      select(article, q_wikidata, dec_date, av_views) %>%
      mutate(SpeciesSSet = as.character(as.numeric(as.factor(article)))) %>%
      filter(complete.cases(.)) %>%
      select(q_wikidata, SpeciesSSet, article) %>%
      unique() %>%
      mutate(SpeciesSSet = as.character(SpeciesSSet))
    return(data_fin)
  })
}

# read in the lambda files 
random_trend_weight <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend_weight)){
  random_trend_weight[[i]]$date <- as.numeric(rownames(random_trend_weight[[i]]))
  random_trend_weight[[i]]$Year <- (random_trend_weight[[i]]$date - 1970)/12 + 2015
  random_trend_weight[[i]]$Year <- as.character(random_trend_weight[[i]]$Year)
  
  # calculate lambda for random
  random_trend_weight[[i]] <- random_trend_weight[[i]] %>%
    filter(date %in% c(1977:2033))
  random_trend_weight[[i]]$lamda = c(0, diff(log10(random_trend_weight[[i]]$LPI_final[1:57])))
  random_trend_weight[[i]]$date <- paste("X", random_trend_weight[[i]]$date, sep = "")
  random_trend_weight[[i]]$language <- languages[i]
}

# run the function with 10 languages, specifying the directory
user_files_weight <- view_directories(classes,
                               directory,
                               languages)

# read in all the files in groups for each language
language_views_weight <- list()
system.time(for(i in 1:length(user_files_weight)){
  language_views_weight[[i]] <- lapply(user_files_weight[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# adjust the lambdas for each species for each language with random, and conert speciesset to character for merging
adj_lambdas_weight <- list()
all_lambdas_weight <- list()
data_file <- list()
for(i in 1:length(language_views_weight)){
  for(j in 1:length(random_trend_weight)){
    data_file <- language_views_weight[[i]][[j]] %>%
      mutate(SpeciesSSet = as.character(SpeciesSSet))
    adj_lambdas_weight[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend_weight[[j]]$lamda, FUN = "-"))
  }
  all_lambdas_weight[[i]] <- adj_lambdas_weight
}

#### additional smoothing of the random adjusted indices
# run the smoothing of lamdas over each class/language combination
smoothed_adjusted_lamda_weight <- list()
for(i in 1:length(all_lambdas_weight)){
  smoothed_adjusted_lamda_weight[[i]] <- lapply(all_lambdas_weight[[i]], smooth_all_groups)
  print(i)
}
####

# merge each lambda file with the speciesSSet ID from view data
merge_lambda_weight <- list()
merge_fin_lambda_weight <- list()
for(i in 1:length(smoothed_adjusted_lamda_weight)){
  for(j in 1:length(smoothed_adjusted_lamda_weight[[i]])){
    merge_lambda_weight[[j]] <- inner_join(smoothed_adjusted_lamda_weight[[i]][[j]], iucn_views_poll_weight[[j]][[i]], by = "SpeciesSSet") %>%
      mutate(taxa = classes[i]) %>%
      mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
    print(nrow(smoothed_adjusted_lamda_weight[[i]][[j]]) - nrow(merge_lambda_weight[[j]]))
    merge_lambda_weight[[j]] <- merge_lambda_weight[[j]] %>%
      filter(!q_wikidata %in% unique_species$q_wikidata)
  }
  merge_fin_lambda_weight[[i]] <- merge_lambda_weight
}

# rbindlist all lambda together and calculate averge for each species across languages
merge_species_weight <- list()
for(i in 1:length(merge_fin_lambda_weight)){
  merge_species_weight[[i]] <- rbindlist(merge_fin_lambda_weight[[i]])
}

# merge all the lambda files, and calc average across each q_wikidata
merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
  inner_join(internet_users, by = c("language" = "Language")) %>%
  wiki_average_weight()

# reshape lambda files back into year rows, and then split into separate taxonomic classes
all_lambdas_weight <- reshape2::dcast(merge_species_bound_weight, q_wikidata + taxa ~ variable)

# run the boostrapping of trends for all lambda
lpi_trends_adjusted_weight <- run_each_group(all_lambdas_weight, random_trend = random_trend_weight[[1]])

# calculate average lambda, starting at each month, and assign factor for whether average is increasing or decreasing
language_frame_weight <- list()
for(i in 1:56){
  language_frame_weight[[i]] <- lpi_trends_adjusted_weight %>%
    filter(row_number() %in% c(i:57)) %>%
    mutate(lambda = c(0, diff(log10(LPI)))) %>%
    mutate(average_lambda = mean(lambda)) %>% 
    select(average_lambda, Year, LPI, LPI_upr, LPI_lwr) %>%
    unique() %>%
    mutate(factor_rate = factor(ifelse(average_lambda >= 0, "increasing/stable", "decreasing"))) %>% 
    mutate(series_start = i)
}

# build the plot for the user weighted average
all_class_weighted <- rbindlist(language_frame_weight) %>%
  select(series_start, average_lambda, factor_rate) %>%
  unique() %>% 
  bind_rows(data.frame("average_lambda" = NA,
                       "factor_rate" = NA,
                       "series_start" = 57)) %>%
  mutate(Year = random_trend_weight[[1]]$Year) %>%
  mutate(LPI = lpi_trends_adjusted_weight$LPI) %>%
  mutate(LPI_upr = lpi_trends_adjusted_weight$LPI_upr) %>%
  mutate(LPI_lwr = lpi_trends_adjusted_weight$LPI_lwr) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing/stable", "decreasing"), labels = c("Increasing or stable", "Decreasing"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  ylab("User-weighted Species Awareness Index (SAI)") +
  scale_y_continuous(breaks = c(1, 0.96, 0.92, 0.88), labels = c("1", "0.96","0.92", "0.88")) +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 12, vjust = 2))

# combine plots for the french excluded and user weighted trends
all_class_no_french + all_class_weighted + plot_layout(ncol = 1)

# save the overall trend for the main text
ggsave("overall_index_1000_95_no-french_weighted.png", scale = 1.4, dpi = 350)

# plot of basic trends for use in discussion figure, with rescaling
all_class_no_french_diagram <- rbindlist(language_frame) %>%
  select(series_start, average_lambda, factor_rate) %>%
  unique() %>% 
  bind_rows(data.frame("average_lambda" = NA,
                       "factor_rate" = NA,
                       "series_start" = 57)) %>%
  mutate(Year = random_trend[[1]]$Year) %>%
  mutate(LPI = lpi_trends_adjusted$LPI) %>%
  mutate(LPI_upr = lpi_trends_adjusted$LPI_upr) %>%
  mutate(LPI_lwr = lpi_trends_adjusted$LPI_lwr) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing/stable", "decreasing"), labels = c("Increasing or stable", "Decreasing"))) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  ylab(NULL) +
  scale_y_continuous(breaks = c(1), labels = c("0")) +
  xlab("") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 25),
        panel.border = element_rect(size = 3))

# plot of basic trends for use in discussion figure, with rescaling
all_class_no_french_diagram_rescale <- rbindlist(language_frame) %>%
  select(series_start, average_lambda, factor_rate) %>%
  unique() %>% 
  bind_rows(data.frame("average_lambda" = NA,
                       "factor_rate" = NA,
                       "series_start" = 57)) %>%
  mutate(Year = random_trend[[1]]$Year) %>%
  mutate(LPI = lpi_trends_adjusted$LPI) %>%
  mutate(LPI_upr = lpi_trends_adjusted$LPI_upr) %>%
  mutate(LPI_lwr = lpi_trends_adjusted$LPI_lwr) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing/stable", "decreasing"), labels = c("Increasing or stable", "Decreasing")))

diagram_rescale <- all_class_no_french_diagram_rescale %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI), size = 3) +
  ylab(NULL) +
  scale_y_continuous(breaks = c(min(all_class_no_french_diagram_rescale$LPI), max(all_class_no_french_diagram_rescale$LPI)), labels = c("0", "100")) +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 25),
        axis.ticks.x = element_blank(),
        panel.border = element_rect(size = 3))

# combine the plots for diagram
all_class_no_french_diagram + diagram_rescale + plot_layout(ncol = 1)

# save the combined diagram plot
ggsave("rescale_diagram.png", scale = 1, dpi = 350)
