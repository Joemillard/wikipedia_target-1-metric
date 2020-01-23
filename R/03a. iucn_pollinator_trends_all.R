## script for constructing trends on ecological system
library(dplyr)
library(data.table)
library(rlpi)
library(ggplot2)
library(forcats)
library(rvest)
library(tm)
library(patchwork)
library(ggrepel)
library(boot)

# source the functions R script
source("R/00. functions.R")

# read in view data for daily views
bird_views <- read.csv("data/bird_user_trends.csv", stringsAsFactors = FALSE)
mammal_views <- read.csv("data/mammal_user_trends.csv", stringsAsFactors = FALSE)
insect_views <- read.csv("data/insect_user_trends.csv", stringsAsFactors = FALSE)

# aggregate the views for each of birds, mammals, and insects by month
bird_av <- run_dat(bird_views, av_all = FALSE)
mammal_av <- run_dat(mammal_views, av_all = FALSE)
insect_av <- run_dat(insect_views, av_all = FALSE)

# create list of view object
iucn_views_poll <- list(bird_av, mammal_av, insect_av)

### put each dataframe into the lpi structure and run lpi
# rescale each dataframe to start at 1970 and merge back with the views
iucn_views_poll <- lapply(iucn_views_poll, rescale_iucn)

# create data.frame for those with 53 rows (full time series)
iucn_pollinators_comp <- lapply(iucn_views_poll, select_comp)

# reformat data as compatible with lpi function
iucn_pollinators_comp <- lapply(iucn_pollinators_comp, structure_lpi_overall)

# groupings for overall trend
groupings <- c("birds", "mammals", "insects")

# write each file to table
for(i in 1:length(groupings)){
  write.table(iucn_pollinators_comp[[i]], paste(groupings[i], "data.txt", sep = "_"), row.names = FALSE)
  infile_df <- data.frame(FileName = paste(groupings[i], "data.txt", sep = "_"), Group = 1, Weighting = 1)
  write.table(infile_df, paste(groupings[i], "pages_all_infile.txt", sep = "_"), row.names = FALSE)
}

lpi_trends <- list()
for(i in 1:length(groupings)){
  lpi_trends[[i]] <- LPIMain(paste(groupings[i], "pages_all_infile.txt", sep = "_"), REF_YEAR = 1977, PLOT_MAX = 2029)
}

# save rds for pollinator trends
saveRDS(lpi_trends, "data/lpi_trends_pollinator_comp-series_all.rds")

### make plot for trends
lpi_trends <- readRDS("data/lpi_trends_pollinator_comp-series_all.rds")

# add column for class and pollinating
for(i in 1:length(groupings)){
  lpi_trends[[i]]$class <- groupings[i]
  lpi_trends[[i]]$date <- as.numeric(rownames(lpi_trends[[i]]))
  lpi_trends[[i]]$Year <- (lpi_trends[[i]]$date - 1970)/12 + 2015
}

# make plot of trends over time for each grouping
lpi_trends %>%
  rbindlist %>%
  filter(LPI_final != -99) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_line(aes(x = Year, y = LPI_final, group = class), size = 1) + 
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = class), alpha = 0.2) +
  facet_wrap(~class, scales = "free_x") +
  theme_bw() + 
  theme(panel.grid = element_blank())

### random monthly trend
random_wiki_lpi <- readRDS("data/lpi_trend_random_3.rds")

# adjust the year column
random_wiki_lpi$date <- as.numeric(rownames(random_wiki_lpi))
random_wiki_lpi$Year <- (random_wiki_lpi$date - 1970)/12 + 2015
random_wiki_lpi$Year <- as.character(random_wiki_lpi$Year)

# bind the random values back onto main dataframe and calculate adjusted lpi
lpi_trends <- lapply(lpi_trends, join_random)

#### Robin calculating lambdas
lpi_trends_corr = lpi_trends

for (i in 1:length(lpi_trends_corr)) {
  group_index = lpi_trends_corr[[i]]
  
  index_values = group_index$LPI_final.x
  lambdas = diff(log10(index_values[1:53]))
  
  random_index = group_index$LPI_final.y
  r_lambdas = diff(log10(random_index[1:53]))
  
  corrected_lambdas = lambdas - r_lambdas
  
  corrected_index = cumprod(10^c(0, corrected_lambdas))
  
  lpi_trends_corr[[i]]$LPI_final.x[1:53] = corrected_index
  
}

### make plot of trends over time for each grouping, adjusted for random
overall_trends <- lpi_trends_corr %>%
  rbindlist %>% 
  filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(Year)) %>%
  #mutate(pollinat = factor(pollinat, levels = c("Y", "N"), labels = c("Yes", "No"))) %>% 
  mutate(class = factor(class, levels = c("birds", "insects", "mammals"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() +
  geom_point(aes(x = Year, y = LPI_final.x, colour = class)) + 
  geom_line(aes(x = Year, y = LPI_final.x, colour = class)) +
  #geom_smooth(aes(x = Year, y = adjusted_lpi, group = groupings, colour = pollinat, fill = pollinat), lm = "loess") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  #facet_wrap(~class, scales = "free_x") +
  scale_colour_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  #scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
  theme_bw() +
  ggtitle("B") +
  ylab("Random adjusted index")

ggsave("pollinating_trends_comp_all.png", scale = 1.1, dpi = 350)

### confidence interval with bootstrapping
# function for calculating lpi and adjusting
calc_lpi_adj <- function(data){
  
    # randomly select 100 random species
    iucn_pollinators_comp <- data
    
    set.seed(10)
    
    boostrap_conf <- list()
  
    for(i in 1:length(groupings)){
      #bound_lpi <- list()
      iucn_pollinators_comp_new <- list()
      fin_adjusted_lpi <- list()
      
      unique_species <- unique(iucn_pollinators_comp[[i]]$ID)
      
      for(j in 1:50){
        
        row_indexes <- sample(unique_species, size = 50)
        
        iucn_pollinators_comp_new[[j]] <- iucn_pollinators_comp[[i]] %>% filter(ID %in% row_indexes)
        
        write.table(iucn_pollinators_comp_new[[j]], paste(groupings[i], "data.txt", sep = "_"), row.names = FALSE)
        infile_df <- data.frame(FileName = paste(groupings[i], "data.txt", sep = "_"), Group = 1, Weighting = 1)
        write.table(infile_df, paste(groupings[i], "pages_all_infile.txt", sep = "_"), row.names = FALSE)
    
        lpi_trends <- list()
        lpi_trends_corr <- list()
        
        lpi_trends[[j]] <- LPIMain(paste(groupings[i], "pages_all_infile.txt", sep = "_"), REF_YEAR = 1977, PLOT_MAX = 2029)
        
        # add column for class and pollinating
        lpi_trends[[j]]$class <- groupings[i]
        lpi_trends[[j]]$date <- as.numeric(rownames(lpi_trends[[j]]))
        lpi_trends[[j]]$Year <- (lpi_trends[[j]]$date - 1970)/12 + 2015
        
        lpi_trends[[j]] <- join_random(lpi_trends[[j]])
      
        group_index = lpi_trends[[j]]
        
        index_values = group_index$LPI_final.x
        lambdas = diff(log10(index_values[1:53]))
        
        random_index = group_index$LPI_final.y
        r_lambdas = diff(log10(random_index[1:53]))
        
        corrected_lambdas = lambdas - r_lambdas
        
        corrected_index = cumprod(10^c(0, corrected_lambdas))
        
        lpi_trends[[j]]$adjusted_lpi[1:53] = corrected_index
        
        #print(lpi_trends[[j]])
      
        fin_adjusted_lpi[[j]] <- data.frame("Year" = lpi_trends[[j]]$Year, "LPI" = lpi_trends[[j]]$adjusted_lpi)
        
        #print(fin_adjusted_lpi)
        
      }
      
      bound_lpi <- rbindlist(fin_adjusted_lpi)
      
      print(bound_lpi)
      
      boostrap_conf[[i]] <- bound_lpi %>% mutate(class = groupings[i])
      
    }
    
    boostrap_conf <- data.table::rbindlist(boostrap_conf)
    
    #print(boostrap_conf)
    
    return(boostrap_conf)
} 

fin <- calc_lpi_adj(iucn_pollinators_comp)

# calculate confidence interval - which means values do I use?
fin_conf <- fin %>% 
  dplyr::group_by(class, Year) %>%
  dplyr::summarise(conf = 1.96 * (sd(LPI) / sqrt(length(LPI))))

# new plot with confidence interval
overall_trends_conf <- lpi_trends_corr %>%
  rbindlist %>% 
  inner_join(fin_conf, by = c("Year", "class")) %>%
  filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(Year)) %>%
  #mutate(pollinat = factor(pollinat, levels = c("Y", "N"), labels = c("Yes", "No"))) %>% 
  mutate(class = factor(class, levels = c("birds", "insects", "mammals"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_final.x - conf, ymax = LPI_final.x + conf, fill = class), alpha = 0.5) +
  geom_point(aes(x = Year, y = LPI_final.x, colour = class)) + 
  geom_line(aes(x = Year, y = LPI_final.x, colour = class)) +
  #geom_smooth(aes(x = Year, y = adjusted_lpi, group = groupings, colour = pollinat, fill = pollinat), lm = "loess") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  #facet_wrap(~class, scales = "free_x") +
  scale_colour_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  scale_fill_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  #scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
  theme_bw() +
  ggtitle("B") +
  ylab("Random adjusted index")

## plot of insect change with key publications
altmetric <- data.frame(x = c(2019.16666666667, 2018.83333333333, 2017.83333333333), y = c(0.7390624, 0.7628576, 0.7816953), size = c(5466, 2810, 6316), text = c("Sanchez-Bayo & Wyckhuys", "Lister & Garcia", "Hallmann et al"))

overall_trends_insects <- lpi_trends_corr %>%
  rbindlist %>% 
  filter(class == "insects") %>%
  filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(class = factor(class, levels = c("insects"), labels = c("Insects"))) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI_final.x)) +
  geom_label_repel(aes(x = x, y = y, label = text), fill= "black", colour = "white", data = altmetric, size = 3, point.padding = 0.3) +
  geom_point(aes(x = x, y = y, size = size), data = altmetric, fill = NA) + 
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  scale_size_continuous(name = "Altmetric") +
  theme_bw() +
  ylab("Random adjusted insect index") +
  xlab("Year")

ggsave("pollinating_trends_comp_insects.png", scale = 1, dpi = 350)

## modelling of lambda values in relation to pollinating, class, system
# read in lambda files
bird_lambda <- read.csv("birds_data_lambda.csv", stringsAsFactors = FALSE)
insect_lambda <- read.csv("insects_data_lambda.csv", stringsAsFactors = FALSE)
mammal_lambda <- read.csv("mammals_data_lambda.csv", stringsAsFactors = FALSE)
random_wiki_lpi <- readRDS("data/lpi_trend_random_3.rds")

# adjust each of the lambda values for random
# adjust the year column
random_wiki_lpi$date <- as.numeric(rownames(random_wiki_lpi))
random_wiki_lpi$Year <- (random_wiki_lpi$date - 1970)/12 + 2015
random_wiki_lpi$Year <- as.character(random_wiki_lpi$Year)

# calculate lambda for random
random_wiki_lpi <- random_wiki_lpi %>%
  filter(date %in% c(1977:2029))
random_wiki_lpi$lamda = c(0, diff(log10(random_wiki_lpi$LPI_final[1:53])))
random_wiki_lpi$date <- paste("X", random_wiki_lpi$date, sep = "")

# pollinators with redlist id
bird_iucn_id <- read.csv("data/iucn_AVES_monthly_views_user.csv", stringsAsFactors = FALSE)
insect_iucn_id <- read.csv("data/iucn_INSECTA_monthly_views_user.csv", stringsAsFactors = FALSE)
mammal_iucn_id <- read.csv("data/iucn_MAMMALIA_monthly_views_user.csv", stringsAsFactors = FALSE) 

# combine taxa groupings as list
pollinating_taxa <- list(bird_iucn_id, insect_iucn_id, mammal_iucn_id)

# unique the articles, and reformat the article column
all_taxa_ids <- lapply(pollinating_taxa, select_col) %>% 
  rbindlist() %>%
  mutate(article = gsub(" ", "_", article))

# redlist for iucn red list level
redlist <- read.csv("data/redlist_data_2019_10_11_15_05_45.csv", stringsAsFactors = FALSE) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  dplyr::select(species, category, scientific_name, taxonid)

### set up the final dataframe
# create list of data and vectors for assigning new column
class_group <- c("bird", "insect", "mammal")
lambdas <- list(bird_lambda, insect_lambda, mammal_lambda)

# adjust lambda values for the random
data <- lapply(lambdas, adjust_lambda)

# run loop to add columns for pollinating and class
for(i in 1:length(data)){
  data[[i]]$class <- class_group[i]
}

all_lambda <- rbindlist(data)

# calculate the average lambda for each row, excluding 1977
# create vector for all years
all_lambda$lambda_summed <- rowSums(all_lambda[, c(5:56)])

# calculate sum and average lambda and convert speciesset to character
all_lambda$av_lambda <- all_lambda$lambda_summed / 52
all_lambda$SpeciesSSet <- as.character(all_lambda$SpeciesSSet)

### models for predicting lambda and pollination relatedness -- drop all_total from the model, and check if have similar result
# similarity value in this model
predict_lambda_all <- lm(av_lambda ~ class, data = all_lambda)

summary(predict_lambda_all)

predict_lambda_step <- step(predict_lambda_all, direction = "both", trace = TRUE)

summary(predict_lambda_step)
plot(predict_lambda_step)
anova(predict_lambda_step)

predicted_values <- predict(predict_lambda_step, all_lambda, se.fit = TRUE)

all_lambda$predicted_values <- predicted_values$fit
all_lambda$predicted_values_se <- predicted_values$se.fit

fin_frame_6 <- all_lambda %>%
  dplyr::select(class, predicted_values, predicted_values_se) %>%
  unique()

lambda_overall <- fin_frame_6 %>%
  mutate(class = factor(class, levels = c("bird", "insect", "mammal"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() + 
  geom_errorbar(aes(x = class, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se)), colour = class)) +
  geom_point(aes(x = class, y = predicted_values, colour = class)) +
  #facet_grid(~class) +
  ylab("Random adjusted average lambda") +
  xlab("Class") +
  ggtitle("A") +
  scale_y_continuous(breaks = c(-0.001, -0.0005, 0, 0.0005), labels = c("-0.001", "-0.0005", "0", "0.0005")) +
  scale_colour_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  theme_bw() +
  theme(legend.position = "none")

lambda_overall + overall_trends + patchwork::plot_layout(ncol = 2)

ggsave("overall_trends_figure_2.png", dpi = 350, scale = 1.1)
