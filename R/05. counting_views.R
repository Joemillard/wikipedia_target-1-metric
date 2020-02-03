## script for counting the number of views and pollinators in analysis
library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)

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

# [;pt gpt ]
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
