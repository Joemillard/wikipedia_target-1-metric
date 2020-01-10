## modelling of lambda values in relation to pollinating, class, system
library(data.table)
library(dplyr)
library(lme4)
library(ggplot2)
library(MASS)
library(GGally)
# library(plyr) - # hashed out to avoid bringing in and clashing with dplyr, used further down specifically for revalue

# source the functions R script
source("R/00. functions.R")

# read in lambda files
insect_y_lambda <- read.csv("data/insect_Y_data_lambda.csv", stringsAsFactors = FALSE)
insect_n_lambda <- read.csv("data/insect_N_data_lambda.csv", stringsAsFactors = FALSE)
bird_y_lambda <- read.csv("data/bird_Y_data_lambda.csv", stringsAsFactors = FALSE)
bird_n_lambda <- read.csv("data/bird_N_data_lambda.csv", stringsAsFactors = FALSE)
mammal_y_lambda <- read.csv("data/mammal_Y_data_lambda.csv", stringsAsFactors = FALSE)
mammal_n_lambda <- read.csv("data/mammal_N_data_lambda.csv", stringsAsFactors = FALSE)
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

# read in pollinator data and count orders have pollinators and subset out those without pollinators
# built in iucn_polliantor trends
iucn_pollinators <- readRDS("data/iucn_pollinators_comp.rds")

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
data <- list(insect_y_lambda, insect_n_lambda, bird_y_lambda, bird_n_lambda, mammal_y_lambda, mammal_n_lambda)
class_group <- c("insect", "insect","bird", "bird", "mammal", "mammal")
pollinat_YN <- c("Y", "N", "Y", "N", "Y", "N")

# adjust lambda values for the random
data <- lapply(data, adjust_lambda)

# run loop to add columns for pollinating and class
for(i in 1:length(data)){
  data[[i]]$class <- class_group[i]
  data[[i]]$pollinating <- pollinat_YN[i]
}

all_lambda <- rbindlist(data)

# calculate the average lambda for each row, excluding 1977
# create vector for all years
all_lambda$lambda_summed <- rowSums(all_lambda[, c(5:56)])

# calculate sum and average lambda and convert speciesset to character
all_lambda$av_lambda <- all_lambda$lambda_summed / 52
all_lambda$SpeciesSSet <- as.character(all_lambda$SpeciesSSet)

### bind pollination data onto the lambda lpi data
# select just for new year column, rename columns, and remove NAs
retrieve_lpi_id <- function(iucn_no_dup, poll){
  iucn_no_dup <- iucn_no_dup %>%
    filter(pollinating %in% poll) %>%
    dplyr::select(article, dec_date, total_views) %>%
    mutate(ID = as.character(as.numeric(as.factor(article)))) %>%
    dplyr::select(article, ID)
}

# retrieve lpi ids for each 
lpi_id_Y <- lapply(iucn_pollinators, retrieve_lpi_id, poll = "Y")
lpi_id_N <- lapply(iucn_pollinators, retrieve_lpi_id, poll = "N")
lpi_structured <- c(lpi_id_Y, lpi_id_N)

# set up vectors to assign to columns
classes <- c("bird", "insect", "mammal", "bird", "insect", "mammal")
pollinat <- c("Y", "Y", "Y", "N", "N", "N")

for(i in 1:length(lpi_structured)){
  lpi_structured[[i]]$class <- classes[i]
  lpi_structured[[i]]$pollinating <- pollinat[i]
}

lpi_structured <- rbindlist(lpi_structured) %>%
  unique()
  
# bind lpi ids to the lambdas and mutate article to join onto sim
joined_pollinator <- inner_join(all_lambda, lpi_structured, by = c("SpeciesSSet" = "ID", "class", "pollinating")) %>%
  mutate(article = gsub(" ", "_", article))

# bind taxonid onto final data frame
fin_frame_2 <- inner_join(joined_pollinator, all_taxa_ids, by = c("article" = "article"))

# bind extinction category onto final dataframe
fin_frame_5 <- inner_join(fin_frame_2, redlist, by = "taxonid")

# change factors for NT and LC
fin_frame_5$category <- plyr::revalue(fin_frame_5$category, c("LR/nt" = "NT", "LR/cd"="NT", "LR/lc" = "LC"))

### models for predicting lambda and pollination relatedness -- drop all_total from the model, and check if have similar result
# similarity value in this model
predict_lambda_1 <- lm(av_lambda ~ class * pollinating, data = fin_frame_5)

# binomial model test
#fin_frame_6 = fin_frame_5
#lambdas = fin_frame_6[, 5:56]
fin_frame_5$inc = fin_frame_5$av_lambda > 0
fin_frame_5$inc = fin_frame_5$av_lambda <= 0

predict_lambda_glm <- glm(inc ~ class * pollinating.x, data = fin_frame_5, family = "binomial")
####
predic_lambda_glm_step <- step(predict_lambda_glm, direction = "both", trace = TRUE)
summary(predic_lambda_glm_step)

summary(predict_lambda_1)
plot(predict_lambda_1)
#summary(predict_lambda_2)
anova(predict_lambda_1)
#anova(predict_lambda_2)

predict_lambda_step <- step(predict_lambda_1, direction = "both", trace = TRUE)

#predict_lambda_step_2 <- stepAIC(predict_lambda_2, direction = "both", 
                               #trace = FALSE)

summary(predict_lambda_step)
plot(predict_lambda_step)
#summary(predict_lambda_step_2)
anova(predict_lambda_step)
#anova(predict_lambda_step_2)

predicted_values <- predict(predict_lambda_step, fin_frame_5, se.fit = TRUE)

fin_frame_5$predicted_values <- predicted_values$fit
fin_frame_5$predicted_values_se <- predicted_values$se.fit

fin_frame_6 <- fin_frame_5 %>%
  dplyr::select(class, pollinating, predicted_values, predicted_values_se) %>%
  unique()

fin_frame_6 %>%
  mutate(pollinating = factor(pollinating, levels = c("Y", "N"), labels = c("Yes", "No"))) %>%
  mutate(class = factor(class, levels = c("bird", "insect", "mammal"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() + 
  geom_errorbar(aes(x = pollinating, y = predicted_values, ymin = (predicted_values - (1.96 * predicted_values_se)), ymax = (predicted_values + (1.96 * predicted_values_se)), colour = pollinating)) +
  geom_point(aes(x = pollinating, y = predicted_values, colour = pollinating)) +
    facet_grid(~class) +
    ylab("Random adjusted average lambda") +
    xlab("Pollinating") +
    scale_y_continuous(breaks = c(-0.001, 0, 0.001), labels = c(-0.001, 0, 0.001)) +
    scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
    scale_colour_manual(name = "Pollinating", values = c("black", "red")) +
    theme_bw() +
    theme(legend.position = "none")
  
ggsave("lambda_class_pollinating_pred_4.png", dpi = 350, scale = 1)
################

# with full models
model_7 <- lm(av_lambda ~ pollinating.x * class * log10(no_characters) * log10(value_rescale), data = fin_frame_5)
model_8 <- lm(av_lambda ~ pollinating.x * class * log10(value_rescale) * log10(no_characters) * log10(all_total), data = fin_frame_5)

step.model_2 <- stepAIC(model_8, direction = "both", 
                      trace = FALSE)

step.model_3 <- stepAIC(model_7, direction = "both", 
                        trace = FALSE)

summary(model_7)
anova(model_7)
anova(model_8)
anova(step.model_2)
summary(step.model_2)
summary(step.model_3)
anova(step.model_3)

plot(step.model_3)

fin_frame_insect <- fin_frame_5 %>%
  filter(class == "insect")

insect_model <- lm(av_lambda ~ log10(value_rescale), data = fin_frame_insect)
