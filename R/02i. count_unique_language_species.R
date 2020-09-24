# count the number of species unique to each language - checking whether French language has may unique species

# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(forcats)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
average_monthly_views <- readRDS("Z:/submission_2/daily_average_views_10-languages.rds")

# extract names of the average view list and assign to a vector
languages <- names(average_monthly_views)

# select unique species for each language
sel_unique_wiki <- function(data_file){
  data_fin <- data_file %>%
    select(q_wikidata) %>%
    unique()
  
  return(data_fin)
}

# select unique species for each language, and then bind together the classes within each language
all_species <- list()
for(i in 1:length(average_monthly_views)) {
  all_species[[i]] <- lapply(average_monthly_views[[i]], sel_unique_wiki) %>%
    rbindlist()
}

# assign a column for language according to names of list from above
for(i in 1:length(all_species)) {
  all_species[[i]]$language <- languages[i]
}

# bind together the dataframe for all languages
bound_all_spec <- rbindlist(all_species)

# identify all species which appear in only one language
bound_unique_spec <- bound_all_spec %>%
  group_by(q_wikidata) %>%
  tally() %>%
  filter(n >= 1)

# join the unique species onto all species by wikidata to retrieve the language and sum unique
unique_spec_lang <- inner_join(bound_unique_spec, bound_all_spec,  by = "q_wikidata") %>%
  group_by(language) %>%
  summarise(total_unique = sum(n)) %>%
  ungroup() %>%
  arrange(desc(total_unique)) %>%
  print() %>%
  mutate(language = factor(language, labels = c("Arabic", "German", "English", "Spanish", "French", "Italian", "Japanese", "Portuguese", "Russian", "Chinese"))) %>%
  mutate(language = fct_reorder(language, -total_unique)) %>%
  ggplot() +
    geom_bar(aes(x = language, y = total_unique), stat = "identity") +
    ylab("Unique species") +
    xlab("") +
    #scale_y_continuous(limits = c(0, 2100), expand = c(0, 0)) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
# save the plot for unique species in each language, and the list of unique species with wiki_id
ggsave("unique_species_language.png", scale = 0.9, dpi = 350)
write.csv(bound_unique_spec, "one_language_species.csv")


