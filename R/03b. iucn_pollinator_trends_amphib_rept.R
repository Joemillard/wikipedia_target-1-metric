## script for constructing trends on ecological system
library(dplyr)
library(data.table)
library(rlpi)
library(ggplot2)
library(forcats)
library(patchwork)
library(ggrepel)
library(boot)

# source the functions R script
source("R/00. functions.R")

## read in the lambda files
amphib <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_AMPHIBIA_popdata_user_all_lambda.csv", row.names = 1)
aves <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_AVES_popdata_user_all_lambda.csv", row.names = 1)
insects <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_INSECTA_popdata_user_all_lambda.csv", row.names = 1)
mammals <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_MAMMALIA_popdata_user_all_lambda.csv", row.names = 1)
reptiles <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_REPTILIA_popdata_user_all_lambda.csv", row.names = 1)
actin <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_ACTINOPTERYGII_popdata_user_all_lambda.csv", row.names = 1)

# create list of data and vectors for assigning new column
lambda_data <- list(amphib, aves, insects, mammals, reptiles, actin)

# remove extra columns anc omplete cases
remove_cols <- function(data) {
  data_new <- data %>%
    select(-X1971, -X1972, -X1973, -X1974, -X1975, -X1976, -X1977) %>%
    rename(X1977 = X1970)
  
  data_new <- data_new[complete.cases(data_new), ]
    
}

# adjust the lambdas from robin for each of the subsets with random values
adjust_lambda_robin <- function(x){
  data <- melt(x, id = c("SpeciesSSet", "Freq"))
  data <- inner_join(data, random_wiki_lpi, by = c("variable" = "date"))
  data$adjusted_lambda <- data$value - data$lamda
  data <- data %>%
    dplyr::select(SpeciesSSet, Freq, variable, adjusted_lambda)
  
  data <- dcast(data, SpeciesSSet + Freq ~ variable)
  return(data)
}

lambda_data <- lapply(lambda_data, remove_cols)

### random monthly trend
random_wiki_lpi <- readRDS("data/lpi_trend_random_3.rds")

# derive the lambdas from the random index
r_lambdas <- diff(log10(random_wiki_lpi$LPI_final[1:53]))

# lambda groupings
class_group_lambda <- c("amphibians", "birds", "insects", "mammals", "reptiles", "actinopterygii")

### script for calculating the confidence interval for each grouping
# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  this_lambdas = lambdas[ind, ]
  
  mean_ann_lambda = colMeans(this_lambdas)
  
  trend = cumprod(10^c(0, mean_ann_lambda))
  return(trend)
}

run_each_group <- function(data){
  
  # Random adjusted species trends
  adj_lambdas <- sweep(data[4:ncol(data)], 2, r_lambdas)
  
  # Bootstrap these to get confidence intervals
  dbi.boot = boot(adj_lambdas, create_lpi, R = 10000)

  # Construct dataframe and get 95% intervals
  boot_res = data.frame(LPI = dbi.boot$t0)
  boot_res$Year = random_wiki_lpi$Year[1:(nrow(random_wiki_lpi)-1)]
  boot_res$LPI_upr = apply(dbi.boot$t, 2, quantile, probs = c(0.95)) 
  boot_res$LPI_lwr = apply(dbi.boot$t, 2, quantile, probs = c(0.05))

  return(boot_res)
  
}

lpi_trends_adjusted <- lapply(lambda_data, run_each_group)

# add column for class and pollinating
for(i in 1:length(class_group_lambda)){
  lpi_trends_adjusted[[i]]$class <- class_group_lambda[i]
}

lpi_confidence_int <- rbindlist(lpi_trends_adjusted)

lpi_confidence_int <- lpi_confidence_int %>%
  group_by(class) %>%
  mutate(Year = 1977:2029) %>%
  mutate(Year = (Year - 1970)/12 + 2015) %>%
  ungroup()
  
### make plot of trends over time for each grouping, adjusted for random
overall_trends <- lpi_confidence_int %>%
  mutate(class = factor(class, levels = c("actinopterygii", "amphibians", "birds", "insects", "mammals", "reptiles"), labels = c("Actinopterygii", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  #filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(Year)) %>%
  ggplot() +
  #geom_point(aes(x = Year, y = LPI)) + 
  geom_line(aes(x = Year, y = LPI)) +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.37) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  facet_wrap(~class, scales = "free_x") +
  #scale_colour_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  #scale_fill_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  theme_bw() +
  ylab("Random adjusted index")

ggsave("all_taxa_2.png", scale = 1, dpi = 350)

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

# adjust lambda values for the random
data <- lapply(lambda_data, adjust_lambda_robin)

# run loop to add columns for pollinating and class
for(i in 1:length(data)){
  data[[i]]$class <- class_group_lambda[i]
}

all_lambda <- rbindlist(data)

# calculate the average lambda for each row, excluding 1977
# create vector for all years
all_lambda$lambda_summed <- rowSums(all_lambda[, c(4:55)])

# calculate sum and average lambda and convert speciesset to character
all_lambda$av_lambda <- all_lambda$lambda_summed / 52
all_lambda$SpeciesSSet <- as.character(all_lambda$SpeciesSSet)

### models for predicting lambda and pollination relatedness -- drop all_total from the model, and check if have similar result
# similarity value in this model
predict_lambda_all <- lm(av_lambda ~ class, data = all_lambda)

predict_lambda_step <- step(predict_lambda_all, direction = "both", trace = TRUE)

#summary(predict_lambda_step)
#plot(predict_lambda_step)
#anova(predict_lambda_step)

predicted_values <- predict(predict_lambda_step, all_lambda, se.fit = TRUE)

all_lambda$predicted_values <- predicted_values$fit
all_lambda$predicted_values_se <- predicted_values$se.fit

fin_frame_6 <- all_lambda %>%
  dplyr::select(class, predicted_values, predicted_values_se) %>%
  unique()

lambda_overall <- fin_frame_6 %>%
  mutate(class = factor(class, levels = c("insects", "amphibians", "actinopterygii", "birds", "mammals", "reptiles"), labels = c("Insects", "Amphibians", "Actinopterygii", "Birds", "Mammals", "Reptiles"))) %>%
  ggplot() + 
  geom_errorbar(aes(x = class, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se)))) +
  geom_point(aes(x = class, y = predicted_values)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "grey") +
  #facet_grid(~class) +
  ylab("Random adjusted average lambda") +
  xlab("Class") +
  #scale_y_continuous(breaks = c(-0.001, -0.0005, 0, 0.0005), labels = c("-0.001", "-0.0005", "0", "0.0005")) +
  #scale_colour_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("average_lambda_6_classes.png", dpi = 350, scale = 1)
