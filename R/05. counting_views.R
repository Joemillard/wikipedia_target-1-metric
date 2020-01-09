## script for counting the number of views and pollinators in analysis
library(dplyr)
library(data.table)
library(forcats)

bird_views <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/bird_user_trends.csv", stringsAsFactors = FALSE)
mammal_views <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/mammal_user_trends.csv", stringsAsFactors = FALSE)
insect_views <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/insect_user_trends.csv", stringsAsFactors = FALSE)
random_monthly_trends_init <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/random_user_trends.csv", stringsAsFactors = FALSE)

# read in pollinator data and count orders have pollinators and subset out those without pollinators
iucn_pollinators <- readRDS("IUCN_pollinators.rds")

# subset out duplicated pollinators - identify all those with a Y, make unique, then subset these from the N
subset_dup <- function(x) {
  x_2 <- x %>%
    filter(pollinating == "Y") %>%
    select(article) %>%
    unique()
  
  x_3 <- x %>%
    filter(pollinating == "N", !article %in% x_2$article) %>%
    select(-name) %>%
    group_by(article) %>%
    unique() %>%
    ungroup()
  
  x_4 <- x %>%
    filter(pollinating == "Y") %>%
    select(-name)
  
  x_5 <- rbind(x_3, x_4)
  
  return(x_5)
}

iucn_pollinators <- lapply(iucn_pollinators, subset_dup)

# filter out any taxonomic orders without any yesses
filter_pollinator <- function(pollinators){
  pollinators <- pollinators %>%
    filter(pollinating == "Y")
  
  return(pollinators)
}

# pollinators in 4 (birds - 1301), 6 (insects - 551), 7 (mammals - 665), and 8 (reptiles - 96)
pollinating_orders <- lapply(iucn_pollinators, filter_pollinator)
iucn_pollinators <- iucn_pollinators[c(4, 6, 7)]

# plot of order level distribution of pollinators - to work on 18/11/19 (RERUN FOR complete timeseries subset)
order_pollinators <- rbindlist(iucn_pollinators) %>%
  droplevels() %>%
  mutate(pollinating = factor(pollinating, levels = c("N", "Y"))) %>%
  group_by(class_name, order_name) %>%  
  tally() %>% 
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(order_name = fct_reorder(order_name, -n)) %>%
  mutate(sorted = 1:78)

order_pollinator_counts <- rbindlist(iucn_pollinators) %>%
  droplevels() %>%
  mutate(pollinating = factor(pollinating, levels = c("N", "Y"))) %>%
  group_by(class_name, pollinating) %>%  
  tally() %>% 
  ungroup()

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
