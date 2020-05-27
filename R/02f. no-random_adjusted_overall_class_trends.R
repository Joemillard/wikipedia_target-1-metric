# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(boot)
library(forcats)
library(cowplot)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/")

# read in the rds for total monthly views to retrieve the lambda ids
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/user_trends/total_monthly_views_10-languages.rds"))

## format for the lpi function
# rescale each dataframe to start at 1970 and merge back with the views, then output lpi structure with original id
iucn_views_poll <- list()
for(i in 1:length(total_monthly_views)){
  iucn_views_poll[[i]] <- lapply(total_monthly_views[[i]], rescale_iucn)
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

# read in the string of languages and taxa - original order sorted alphabetically for files read in
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the lambda files 
random_trend <- readRDS("overall_10-random-languages.rds")

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

# run code once with random adjustment, and a second time without the random
bound_trends_list <- list()
adjustment <- c("random", "non-random")
for(k in 1:2){
  if(k == 1){
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
  }
  else{
    # adjust the lambdas for each species for each language with random, and convert speciesset to character for merging
    adj_lambdas <- list()
    all_lambdas <- list()
    data_file <- list()
    for(i in 1:length(language_views)){
      for(j in 1:length(random_trend)){
        adj_lambdas[[j]] <- language_views[[i]][[j]] %>%
          mutate(SpeciesSSet = as.character(SpeciesSSet))
      }
      all_lambdas[[i]] <- adj_lambdas
    }
  }
  
  # merge each lambda file with the speciesSSet ID from view data
  merge_lambda <- list()
  merge_fin_lambda <- list()
  for(i in 1:length(language_views)){
    for(j in 1:length(language_views[[i]])){
      merge_lambda[[j]] <- inner_join(all_lambdas[[i]][[j]], iucn_views_poll[[j]][[i]], by = "SpeciesSSet") %>%
        mutate(taxa = classes[i]) %>%
        mutate(language = languages[j]) # merge each set of lambda files with the q_wikidata and add columns for class and language
      print(nrow(language_views[[i]][[j]]) - nrow(merge_lambda[[j]])) # take initial frame from the merged one to check they're the same no. of rows
    }
    merge_fin_lambda[[i]] <- merge_lambda
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
  
  # rbindlist all lambda together and calculate averge for each species across languages
  merge_species <- list()
  for(i in 1:length(merge_fin_lambda)){
    merge_species[[i]] <- rbindlist(merge_fin_lambda[[i]])
  }
  
  # merge all the lambda files, and calc average across each q_wikidata
  merge_species <- rbindlist(merge_species) %>% 
    wiki_average()
  
  # reshape lambda files back into year rows, and then split into separate taxonomic classes
  cast_lambda <- reshape2::dcast(merge_species, q_wikidata + taxa ~ variable)
  all_lambdas <- split(cast_lambda, cast_lambda$taxa)
  
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
  
  # run the boostrapping of trends for each set of random_adjusted lambda, taking the year column from the random trend file
  lpi_trends_adjusted <- list()
  for(i in 1:length(all_lambdas)){
    lpi_trends_adjusted[[i]] <- run_each_group(all_lambdas[[i]], random_trend[[1]]) %>%
      mutate(taxa = classes[i])
  }
  
  # bind together the trends for that language
  bound_trends_list[[k]] <- rbindlist(lpi_trends_adjusted) %>% 
    mutate(adjustment_col = adjustment[k])
  
}

# bind together the trends for both random and non-random adjustment
bound_trends_list <- rbindlist(bound_trends_list)

# plot all the class level trends
bound_trends_list %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(taxa = factor(taxa, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia"),
                       labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = adjustment_col), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI, colour = adjustment_col)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~taxa) +
  ylab("SAI") +
  xlab(NULL) +
  theme_bw()

ggsave("random_adjusted_all-class_SAI_1000_95_no_random.png", scale = 1.2, dpi = 350)
