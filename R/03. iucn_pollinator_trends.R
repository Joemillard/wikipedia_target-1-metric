## script for constructing trends on ecological system
library(dplyr)
library(data.table)
library(rlpi)
library(ggplot2)
library(forcats)
library(boot)

# source the functions R script
source("R/00. functions.R")

# read in view data for daily views
bird_views <- fread("Z:/submission_2/user_trends/en_aves__user_trends.csv", stringsAsFactors = FALSE)
mammal_views <- fread("Z:/submission_2/user_trends/en_mammalia__user_trends.csv", stringsAsFactors = FALSE)
insect_views <- fread("Z:/submission_2/user_trends/en_insecta__user_trends.csv", stringsAsFactors = FALSE)

# aggregate the views for each of birds, mammals, and insects by month
bird_av <- run_dat(bird_views, av_all = FALSE)
mammal_av <- run_dat(mammal_views, av_all = FALSE)
insect_av <- run_dat(insect_views, av_all = FALSE)

# merge the view data and change article to no space
iucn_views <- rbind(bird_av, mammal_av, insect_av)
iucn_views$article <- gsub("_", " ", iucn_views$article)

# read in pollinator data and count orders have pollinators and subset out those without pollinators
iucn_pollinators <- readRDS("data/IUCN_pollinators.rds")

# subset out duplicated pollinators - identify all those with a Y, make unique, then subset these from the N
iucn_pollinators <- lapply(iucn_pollinators, subset_dup)

# filter out any taxonomic orders without any yesses
pollinating_orders <- lapply(iucn_pollinators, filter_pollinator)
iucn_pollinators <- iucn_pollinators[c(4, 6, 7)]

# join the pollinator data onto the view data, for each of 6 subsets (birds, Y/N; insects, Y/N; mammals, Y/N)
iucn_views_poll <- list()
for(i in 1:length(iucn_pollinators)){
  iucn_views_poll[[i]] <- inner_join(iucn_views, iucn_pollinators[[i]], by = c("article")) %>%
    select(article, year, month, pollinating, av_views) %>%
    arrange(article, pollinating, year, month) %>%
    unique() %>%
    data.frame()
}

### put each dataframe into the lpi structure and run lpi
# rescale each dataframe to start at 1970 and merge back with the views
iucn_pollinators_comp <- lapply(iucn_views_poll, rescale_iucn)

# create data.frame for those with 53 rows (full time series)
# iucn_pollinators_comp <- lapply(iucn_views_poll, select_comp)

# save pollinator list as rds
saveRDS(iucn_pollinators_comp, "data/iucn_pollinators_comp.rds")

# separate lists for Y/N pollinating by class, and create list of 6 objects
lpi_structured_Y <- lapply(iucn_pollinators_comp, structure_lpi, poll = "Y")
lpi_structured_N <- lapply(iucn_pollinators_comp, structure_lpi, poll = "N")
lpi_structured <- c(lpi_structured_Y, lpi_structured_N) 

# list of classes by pollinating
groupings <- c("bird_Y", "insect_Y", "mammal_Y", "bird_N", "insect_N", "mammal_N")
class_group <- c("bird", "insect", "mammal","bird", "insect", "mammal")
pollinat_YN <- c("Y", "Y", "Y", "N", "N", "N")

# write each file to table
for(i in 1:length(groupings)){
  write.table(lpi_structured[[i]], paste(groupings[i], "data.txt", sep = "_"), row.names = FALSE)
  infile_df <- data.frame(FileName = paste(groupings[i], "data.txt", sep = "_"), Group = 1, Weighting = 1)
  write.table(infile_df, paste(groupings[i], "pages_all_infile.txt", sep = "_"), row.names = FALSE)
}

lpi_trends <- list()
for(i in 1:length(groupings)){
  lpi_trends[[i]] <- LPIMain(paste(groupings[i], "pages_all_infile.txt", sep = "_"), REF_YEAR = 1977, PLOT_MAX = 2033)
}

# save rds for pollinator trends
saveRDS(lpi_trends, "data/lpi_trends_pollinator_en_sub_2.rds")


### make plot for trends
lpi_trends <- readRDS("data/lpi_trends_pollinator_en_sub_2.rds")

# add column for class and pollinating
for(i in 1:length(class_group)){
  lpi_trends[[i]]$class <- class_group[i]
  lpi_trends[[i]]$pollinat <- pollinat_YN[i]
  lpi_trends[[i]]$groupings <- pollinat_YN[i]
  lpi_trends[[i]]$date <- as.numeric(rownames(lpi_trends[[i]]))
  lpi_trends[[i]]$Year <- (lpi_trends[[i]]$date - 1970)/12 + 2015
  #rownames(lpi_trends[[i]]) <- lpi_trends[[i]]$Year 
}

# make plot of trends over time for each grouping
lpi_trends %>%
  rbindlist %>%
  filter(LPI_final != -99) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_line(aes(x = Year, y = LPI_final, group = groupings, colour = pollinat), size = 1) + 
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = groupings, fill = pollinat), alpha = 0.2) +
  facet_wrap(~class, scales = "free_x") +
  theme_bw() + 
  theme(panel.grid = element_blank())

### random monthly trend
random_monthly_trends_init <- fread("Z:/submission_2/user_trends/en_random__user_trends.csv", stringsAsFactors = FALSE)

# remove NA random pages
random_monthly_trends <- random_monthly_trends_init %>%
  filter(!is.na(timestamp))
         
# aggregate the views for the random trend
random_monthly_trends <- run_dat(random_monthly_trends, av_all = FALSE)

# subset the random pages for just 2000 pages
unique_random <- unique(random_monthly_trends$article)[0:5000]
random_monthly_trends <- random_monthly_trends %>%
  mutate(year = as.numeric(year)) %>%
  mutate(month = as.numeric(month)) %>%
  filter(article %in% unique_random)
  
# select only complete time series
# random_monthly_trends_init <- select_comp(random_monthly_trends_init)

# subtract 2015 to get first year as '0', then multiply by 12 so each original year represents 12 'years', add months and add 1970 for baseline
random_monthly_trends_date <- plyr::ddply(random_monthly_trends, c("article", "year", "month"), summarise, dec_date = (1970 + (year - 2015)*12 + month))

# Merge back into view data
random_monthly_trends <- merge(random_monthly_trends_date, random_monthly_trends, by=c("article", "year", "month"), all = T)

# Create unique ID for each page/species using factor
random_monthly_trends$id <- as.numeric(as.factor(random_monthly_trends$article))

# Select and order columns
random_monthly_trends <- random_monthly_trends[c("article", "id", "dec_date", "av_views")]

# Forcing columns into format for rlpi package
colnames(random_monthly_trends) <- c("Binomial", "ID", "year", "popvalue")
random_monthly_trends$Binomial <- random_monthly_trends$ID

# *** Removing NAs in data (because of error 'Error in { : task 1 failed - "missing value where TRUE/FALSE needed"')
# *** Should add to rlpi the error check for NAs in all columns (apart from pop value)
random_monthly_trends <- random_monthly_trends[complete.cases(random_monthly_trends), ]

write.table(random_monthly_trends, "random_trend_data.txt", row.names = FALSE)
infile_df <- data.frame(FileName="random_trend_data.txt", Group = 1, Weighting = 1)
write.table(infile_df, "random_pages_all_infile.txt", row.names = F)

# run rlpi and save as rds
random_wiki_lpi <- LPIMain("random_pages_all_infile.txt", REF_YEAR = 1977, PLOT_MAX = 2033, basedir = ".")
saveRDS(random_wiki_lpi, "data/lpi_trend_random_en_sub_2.rds")
random_wiki_lpi <- readRDS("data/lpi_trend_random_en_sub_2.rds")

# adjust the year column
random_wiki_lpi$date <- as.numeric(rownames(random_wiki_lpi))
random_wiki_lpi$Year <- (random_wiki_lpi$date - 1970)/12 + 2015
random_wiki_lpi$Year <- as.character(random_wiki_lpi$Year)

# bind the random values back onto main dataframe and calculate adjusted lpi
lpi_trends <- lapply(lpi_trends, join_random)

#### Robin calculating lambdas
lpi_trends_corr <- lpi_trends
#
for (i in 1:length(lpi_trends_corr)) {
  group_index = lpi_trends_corr[[i]]

  index_values = group_index$LPI_final.x
  lambdas = diff(log10(index_values[1:57]))

  random_index = group_index$LPI_final.y
  r_lambdas = diff(log10(random_index[1:57]))

  corrected_lambdas = lambdas - r_lambdas

  corrected_index = cumprod(10^c(0, corrected_lambdas))

  lpi_trends_corr[[i]]$LPI_final.x[1:57] = corrected_index

}

# Load lambda file of interest
bird_lambdas_N <- read.csv("bird_N_data_lambda.csv", row.names = 1)
bird_lambdas_Y <- read.csv("bird_Y_data_lambda.csv", row.names = 1)
insect_lambdas_N <- read.csv("insect_N_data_lambda.csv", row.names = 1)
insect_lambdas_Y <- read.csv("insect_Y_data_lambda.csv", row.names = 1)
mammal_lambdas_N <- read.csv("mammal_N_data_lambda.csv", row.names = 1)
mammal_lambdas_Y <- read.csv("mammal_Y_data_lambda.csv", row.names = 1)

# lambda groupings
# create list of data and vectors for assigning new column
lambda_data <- list(bird_lambdas_N, bird_lambdas_Y, insect_lambdas_N, insect_lambdas_Y, mammal_lambdas_N, mammal_lambdas_Y)
class_group_lambda <- c("bird", "bird", "insect", "insect", "mammal", "mammal")
pollinat_NY <- c("N", "Y", "N", "Y", "N", "Y")

# remove NAs from each of lambda files
remove_NAs <- function(data){
  data_new <- data[complete.cases(data), ]
}

lambda_data <- lapply(lambda_data, remove_NAs)

### script for calculating the confidence interval for each grouping

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  this_lambdas = lambdas[ind, ]
  
  mean_ann_lambda = colMeans(this_lambdas, na.rm = TRUE)
  
  trend = cumprod(10^c(0, mean_ann_lambda))
  return(trend)
}

run_each_group <- function(data){

  # Random adjusted species trends
  adj_lambdas = sweep(data[4:ncol(data)], 2, r_lambdas, FUN = "-")

  # Bootstrap these to get confidence intervals
  dbi.boot = boot(adj_lambdas, create_lpi, R = 10000)

  # Construct dataframe and get 95% intervals
  boot_res = data.frame(LPI = dbi.boot$t0)
  boot_res$Year = random_wiki_lpi$Year[1:(nrow(random_wiki_lpi)-1)]
  boot_res$LPI_upr = apply(dbi.boot$t, 2, quantile, probs = c(0.95), na.rm = TRUE) 
  boot_res$LPI_lwr = apply(dbi.boot$t, 2, quantile, probs = c(0.05), na.rm = TRUE)
  
  return(boot_res)

}

lpi_trends_adjusted <- lapply(lambda_data, run_each_group)

# add column for class and pollinating
for(i in 1:length(class_group_lambda)){
  lpi_trends_adjusted[[i]]$class <- class_group_lambda[i]
  lpi_trends_adjusted[[i]]$pollinat <- pollinat_NY[i]
}

lpi_confidence_int <- rbindlist(lpi_trends_adjusted)

### make plot of trends over time for each grouping, adjusted for random
lpi_trends_corr %>%
  rbindlist %>% 
  inner_join(lpi_confidence_int, by = c("Year", "class", "pollinat")) %>%
  filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(pollinat = factor(pollinat, levels = c("Y", "N"), labels = c("Yes", "No"))) %>% 
  mutate(class = factor(class, levels = c("bird", "insect", "mammal"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() +
  geom_point(aes(x = Year, y = LPI_final.x, colour = pollinat)) + 
  geom_ribbon(aes(x = Year, fill = pollinat, ymin = LPI_upr, ymax = LPI_lwr), alpha = 0.37) +
  geom_line(aes(x = Year, y = LPI_final.x, colour = pollinat)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  facet_wrap(~class, scales = "free_x") +
  scale_colour_manual(name = "Pollinating", values = c("black", "red")) +
  scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
  theme_bw() +
  ylab("Random adjusted index")

ggsave("pollinating_trends_comp_conf_2.png", scale = 1.1, dpi = 350)
  