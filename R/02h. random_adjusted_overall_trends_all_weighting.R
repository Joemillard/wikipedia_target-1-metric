# script for each weighting in 2 panel figure

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
directory <- here::here("data/lambdas/species")

# read in the number of times each species comes up in each language and create proportion column
unique_species <- read.csv(here::here("data/species_number_per_language.csv")) %>%
  mutate(spec_weight = n/10)

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

sum_users <- function(user_frame){

  # convert the number of users to proportion
  user_frame$users_total_10 <- sum(user_frame$Users)
  user_frame$prop <- user_frame$Users/user_frame$users_total_10

  # remove extra columsn with exception of proportion
  user_frame <- user_frame %>%
    select(Language, prop)
  
  return(user_frame)

}

# internet users by each langauge, equivalent to 73.5 of total users
# total internet users
total_users <- 4585578718
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

# proportion of total users for our 10 languages - include in manuscript
sum(internet_users$Users)/total_users # 73.5%

# users of wikipedia
wiki_users <- data.frame("Language" = c("\\^en_",
                                        "\\^de_",
                                        "\\^es_",
                                        "\\^ru_",
                                        "\\^ja_",
                                        "\\^fr_",
                                        "\\^it_",
                                        "\\^pt_",
                                        "\\^zh_",
                                        "\\^ar_"),
                         "Users" = c(1081510000,
                                     233180000,
                                     199680000,
                                     193150000,
                                     155270000,
                                     137000000,
                                     86170000,
                                     63130000,
                                     53520000,
                                     24920000))

# create dataframes for internet user and wiki user proportions
internet_users <- sum_users(internet_users)
wiki_users <- sum_users(wiki_users)

# read in the string of languages and taxa - original order sorted alphabetically for files read in - keep french language for user weighting
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in daily average to retrieve q_wikidata id
average_monthly_views_weight <- readRDS("data/average_views/daily_average_views_10-languages.rds")

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
random_trend_weight <- readRDS("data/lambdas/no_species_random/overall_daily-views_10-random-languages_from_lambda_no-species.rds")

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

# function for the way in way the smoothed lambdas should be weighted
# merge each lambda file with the speciesSSet ID from view data
# print tibble at each merge to check haven't added rows
weight_index <- function(smoothed_adjusted_lamda_weight, sample_weight, us_wi_weight){

  merge_lambda_weight <- list()
  merge_fin_lambda_weight <- list()
  for(i in 1:length(smoothed_adjusted_lamda_weight)){
    for(j in 1:length(smoothed_adjusted_lamda_weight[[i]])){
      merge_lambda_weight[[j]] <- inner_join(smoothed_adjusted_lamda_weight[[i]][[j]], iucn_views_poll_weight[[j]][[i]], by = "SpeciesSSet") %>%
        mutate(taxa = classes[i]) %>%
        mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
      print(nrow(smoothed_adjusted_lamda_weight[[i]][[j]]) - nrow(merge_lambda_weight[[j]]))
    
    }
    merge_fin_lambda_weight[[i]] <- merge_lambda_weight
  }

  # rbindlist all lambda together and calculate averge for each species across languages
  merge_species_weight <- list()
  for(i in 1:length(merge_fin_lambda_weight)){
    merge_species_weight[[i]] <- rbindlist(merge_fin_lambda_weight[[i]])
  }

  ## weighting by sample for each of none, user, and wiki
  # no user/wiki and sample weight
  if(sample_weight == TRUE){
    if(us_wi_weight == "none"){
    merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
      wiki_average() %>% print() %>%
      left_join(unique_species, by = c("q_wikidata" = "q_wikidata")) %>% print() %>%
      mutate(adj_mean_val = mean_val * spec_weight) %>% 
      select(q_wikidata, variable, taxa, adj_mean_val)
    }
  
    # user and sample weight
    if(us_wi_weight == "user"){
    merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
      left_join(internet_users, by = c("language" = "Language")) %>%
      wiki_average_weight() %>% print() %>%
      left_join(unique_species, by = c("q_wikidata" = "q_wikidata")) %>% print() %>%
      mutate(adj_mean_val = mean_val * spec_weight) %>% 
      select(q_wikidata, variable, taxa, adj_mean_val)
    }
    
    # wiki and sample weight
    if(us_wi_weight == "wiki"){
    merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
      left_join(wiki_users, by = c("language" = "Language")) %>%
      wiki_average_weight() %>% print() %>%
      left_join(unique_species, by = c("q_wikidata" = "q_wikidata")) %>% print() %>%
      mutate(adj_mean_val = mean_val * spec_weight) %>% 
      select(q_wikidata, variable, taxa, adj_mean_val)
    }
    
    # after adjusting sample weights, reassign any value of month 1977 back as 1
    merge_species_bound_weight$adj_mean_val[merge_species_bound_weight$variable == "1977"] <- 1
    
  }
  
    ## un-weighted by sample for each of none, user, and wiki
    # no user/wiki and sample un-weighted
  if(sample_weight == FALSE){
    if(us_wi_weight == "none"){
    merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
      wiki_average()
    }
    
    # user and un-weighted sample
    if(us_wi_weight == "user"){
      merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
        left_join(internet_users, by = c("language" = "Language")) %>% print() %>%
        wiki_average_weight() %>% print()
    }
    
    # wiki and un-weighted sample
    if(us_wi_weight == "wiki"){
    merge_species_bound_weight <- rbindlist(merge_species_weight) %>% 
      left_join(wiki_users, by = c("language" = "Language")) %>% print() %>%
      wiki_average_weight() %>% print()
    }
    
  }

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
  
  return(language_frame_weight)
}

# calculate the weighted the index for each of 6 combinations for sample and none sampled indices
# select the first element of list for whole series
sample_NA <- weight_index(smoothed_adjusted_lamda_weight, sample_weight = TRUE, us_wi_weight = "none")[[1]] %>%
  mutate(sample_weight = "sampled") %>%
  mutate(user_weight = "none")

sample_user <- weight_index(smoothed_adjusted_lamda_weight, sample_weight = TRUE, us_wi_weight = "user")[[1]] %>%
  mutate(sample_weight = "sampled") %>%
  mutate(user_weight = "user")

sample_wiki <- weight_index(smoothed_adjusted_lamda_weight, sample_weight = TRUE, us_wi_weight = "wiki")[[1]] %>%
  mutate(sample_weight = "sampled") %>%
  mutate(user_weight = "wiki")

no_sample_NA <- weight_index(smoothed_adjusted_lamda_weight, sample_weight = FALSE, us_wi_weight = "none")[[1]] %>%
  mutate(sample_weight = "un-sampled") %>%
  mutate(user_weight = "none")

no_sample_user <- weight_index(smoothed_adjusted_lamda_weight, sample_weight = FALSE, us_wi_weight = "user")[[1]] %>%
  mutate(sample_weight = "un-sampled") %>%
  mutate(user_weight = "user")

no_sample_wiki <- weight_index(smoothed_adjusted_lamda_weight, sample_weight = FALSE, us_wi_weight = "wiki")[[1]] %>%
  mutate(sample_weight = "un-sampled") %>%
  mutate(user_weight = "wiki")

# bind together all the options for weighting
bound_weighted_trends <- rbind(sample_NA, sample_user, sample_wiki, no_sample_NA, no_sample_user, no_sample_wiki)

# make plot for 6 combinations for weightings
bound_weighted_trends %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(sample_weight = factor(sample_weight, levels = c("sampled", "un-sampled"), labels = c("Sample weighting", "No sample weighting"))) %>%
  mutate(user_weight = factor(user_weight , levels = c("user", "wiki", "none"), labels = c("Internet", "Wikipedia", "No user weighting"))) %>%
  ggplot() +
    geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = user_weight, group = user_weight), alpha = 0.3) +
    geom_line(aes(x = Year, y = LPI, colour = user_weight, group = user_weight)) +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
    scale_fill_manual("User weighting", values = c("green", "blue", "black")) +
    scale_colour_manual("User weighting", values = c("green", "blue", "black")) +
  
    facet_wrap(~sample_weight, scales = "free_y") +
    ylab("Species Awareness Index (SAI)") +
    xlab(NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
      axis.text = element_text(size = 12),
      strip.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 12, vjust = 2))

# save the overall trend for the main text
ggsave("overall_index_1000_95_6_weight_types.png", scale = 1.2, dpi = 350)
