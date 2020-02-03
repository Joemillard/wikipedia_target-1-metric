## adjust with robin's lambda
## script for constructing trends on ecological system
library(dplyr)
library(data.table)
library(rlpi)
library(ggplot2)
library(forcats)
library(patchwork)
library(ggrepel)
library(boot)

# read in the lambda files
amphib <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_AMPHIBIA_popdata_user_all_lambda.csv", row.names = 1)
aves <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_AVES_popdata_user_all_lambda.csv", row.names = 1)
insects <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_INSECTA_popdata_user_all_lambda.csv", row.names = 1)
mammals <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_MAMMALIA_popdata_user_all_lambda.csv", row.names = 1)
reptiles <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_REPTILIA_popdata_user_all_lambda.csv", row.names = 1)
actin <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_ACTINOPTERYGII_popdata_user_all_lambda.csv", row.names = 1)

# read in the random lambda file
random <- read.csv("data/robin_lambdas/class_wiki_indices/iucn_RANDOM_popdata_user_all_lambda.csv", row.names = 1)

# create list of data and vectors for assigning new column
lambda_data_or <- list(amphib, aves, insects, mammals, reptiles, actin)
lambda_data <- list(amphib, aves, insects, mammals, reptiles, actin)

class_group_lambda <- c("amphibians", "birds", "insects", "mammals", "reptiles", "actinopterygii")

# remove extra columns and incomplete cases
remove_cols <- function(data) {
  data_new <- data %>%
    select(-X1971, -X1972, -X1973, -X1974, -X1975, -X1976, -X1977) %>%
    rename(X1977 = X1970)
  
  data_new <- data_new[complete.cases(data_new), ]
  
}

lambda_data <- lapply(lambda_data, remove_cols)

# remove columns from random and calculate average for each column
r_lambdas <- random %>% 
  remove_cols() %>%
  select(-SpeciesSSet, -Freq, -X1977) %>%
  colMeans() %>%
  as.numeric()
  
# adjust each of the lambdas for the random 
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
  dbi.boot = boot(adj_lambdas, create_lpi, R = 100)
  
  # Construct dataframe and get 95% intervals
  boot_res = data.frame(LPI = dbi.boot$t0)
  #boot_res$Year = random_wiki_lpi$Year[1:(nrow(random_wiki_lpi)-1)]
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
overall_trends_adj <- lpi_confidence_int %>%
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

ggsave("robin_random.png", dpi = 350, scale = 1)

## unadjusted index
# remove columns from random and calculate average for each column
mean_lambda <- function(data) {
  data_new <- data %>% 
    remove_cols() %>%
    select(-SpeciesSSet, -Freq, -X1977) %>%
    colMeans() %>%
    as.numeric()
}

lambda_data_or <- lapply(lambda_data_or, mean_lambda)

# calculate the unadjusted index for each grouping
index_lambda <- function(data){
  trend = data.frame("trend" = cumprod(10^c(0, data)))
  trend$year <- 1:nrow(trend)
  return(trend)
}

unadjusted_indices <- lapply(lambda_data_or, index_lambda)

# add column for class and pollinating
for(i in 1:length(unadjusted_indices)){
  unadjusted_indices[[i]]$class <- class_group_lambda[i]
}

unadjusted_indices <- rbindlist(unadjusted_indices)

### make plot of trends over time for each grouping, adjusted for random
overall_trends <- unadjusted_indices %>%
  mutate(class = factor(class, levels = c("actinopterygii", "amphibians", "birds", "insects", "mammals", "reptiles"), labels = c("Actinopterygii", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles"))) %>%
  #filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(year)) %>%
  ggplot() +
  #geom_point(aes(x = Year, y = LPI)) + 
  geom_line(aes(x = year, y = trend)) +
  #geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr), alpha = 0.37) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  facet_wrap(~class, scales = "free_x") +
  #scale_colour_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  #scale_fill_manual(name = "Taxonomic class", values = c("#009E73", "#CC79A7", "#999999")) +
  theme_bw() +
  ylab("Random adjusted index")
