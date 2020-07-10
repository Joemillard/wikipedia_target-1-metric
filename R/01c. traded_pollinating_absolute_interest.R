# read in required packages
library(dplyr)
library(data.table)
library(ggplot2)
library(forcats)
library(patchwork)

# read in the total monthly view data
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/user_trends/total_monthly_views_10-languages.rds"))

## models for rate of change predicted as function of pollinator and traded
# first off, merge the FAO fish and traded species with the rates of change
# languages to add as column for facet wrap
languages_full <- c("Spanish", "French", "German", "Japanese", "Italian", 
                    "Arabic", "Russian", "Portuguese", "Chinese", "English")

# classes to add as column
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random_data")

# read in pollinator data
pollinat <- read.csv("data/COL_compiled_pollinators_add_conf.csv", stringsAsFactors = FALSE) %>%
  select(genus, Class, confidence, fact_conf, comb_conf) %>%
  mutate(Class = tolower(Class)) %>%
  filter(genus != "Tephrozosterops") %>%
  unique()

# read in traded vertebrate species, remove first three empty rows, and then add the fourth row as column names
traded_species <- read.csv("data/class_wiki_indices/submission_2/globally_traded_species_Scheffers.csv", stringsAsFactors = FALSE)

# read in the iucn_titles to match genus name and class for the pollinators - filter out tephrozosterops, which has duplicated wiki id among languages and lower pollination confidence
iucn_titles <- read.csv("data/class_wiki_indices/submission_2/all_iucn_titles.csv", stringsAsFactors = FALSE) %>%
  select(genus_name, class_name, order_name, q_wikidata) %>%
  unique() %>%
  mutate(class_name = tolower(class_name)) %>%
  mutate(order_name = tolower(order_name)) %>%
  filter(genus_name != "Tephrozosterops")

# read in the q_wikidata for traded species
traded_species_en <- read.csv("data/class_wiki_indices/submission_2/traded_species_wikidata.csv", stringsAsFactors = FALSE) %>%
  select(-ï..)
traded_species_rem <- read.csv("data/class_wiki_indices/submission_2/traded_species_wikidata_fr_edit.csv", stringsAsFactors = FALSE) %>%
  select(-manual_search, -X)

# combine the two wildlife traded datasets
all_traded <- rbind(traded_species_en, traded_species_rem) %>%
  filter(qwiki_id != "") %>% 
  select(qwiki_id, ns) %>%
  unique()

# read in the fish species
fao_fishes <-  read.csv("data/class_wiki_indices/submission_2/ASFIS_sp/ASFIS_sp_2020.csv", stringsAsFactors = FALSE)

# read in the fish species with q_wikidata
fishes_en <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata.csv", stringsAsFactors = FALSE)
fishes_zh <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_zh.csv", stringsAsFactors = FALSE)
fishes_fr <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_fr.csv", stringsAsFactors = FALSE)
fishes_de <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_de.csv", stringsAsFactors = FALSE)
fishes_es <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_es.csv", stringsAsFactors = FALSE)
fishes_ru <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_ru.csv", stringsAsFactors = FALSE)
fishes_pt <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_pt.csv", stringsAsFactors = FALSE)
fishes_it <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_it.csv", stringsAsFactors = FALSE)
fishes_ar <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_ar.csv", stringsAsFactors = FALSE)
fishes_ja <- read.csv("data/class_wiki_indices/submission_2/fished_species_wikidata_other_ja.csv", stringsAsFactors = FALSE)

# bind together all the fished data
all_fishes <- rbind(fishes_en, fishes_zh, 
                    fishes_fr, fishes_de, 
                    fishes_es, fishes_ru, 
                    fishes_pt, fishes_it, 
                    fishes_ar, fishes_ja) %>%
  filter(qwiki_id != "") %>%
  select(qwiki_id, ns) %>%
  unique()

# add taxonomic class to each dataframe
for(i in 1:length(total_monthly_views)){
  for(j in 1:length(total_monthly_views[[i]])){
    total_monthly_views[[i]][[j]]$taxonomic_class <- classes[j]
  }
}

# merge the fish trade data with the average views
merge_fish <- function(data_file) {
  
  # merge the data for fish on q_wikidata
  data_fin <- left_join(data_file, all_fishes, by = c("q_wikidata" = "qwiki_id"))  %>%
    rename(used = ns)
  
  # change the used column to have Y/N for fished
  data_fin$used <- ifelse(!is.na(data_fin$used) & data_fin$taxonomic_class == "actinopterygii", "Y", "N")
  
  # check that the row numbers haven;t changed after merging
  data_check <- nrow(data_file) - nrow(data_fin)
  print(data_check)
  
  return(data_fin)
}

merge_trade <- function(data_file){
  
  # merge the rates_fish dataframe onto the traded animals
  data_fin <- left_join(data_file, all_traded, by = c("q_wikidata" = "qwiki_id"))
  
  # amend column for "used" for species that are traded
  data_fin$used[data_fin$ns == 0] <- "Y"
  
  # set all the insects as NA because we don't know
  data_fin$used[data_fin$taxonomic_class == "insecta"] <- NA
  
  # set any non squamate reptiles to NA
  data_fin$used[data_fin$taxonomic_class == "reptilia" & data_fin$order_name != "squamata"] <- NA
  
  # check that the row numbers haven;t changed after merging
  data_check <- nrow(data_file) - nrow(data_fin)
  
  print(data_check)
  
  return(data_fin)
  
}

# merge pollinators onto the trade data
merge_pollinators <- function(data_file){
  
  # bind the iucn_titles data onto the rates to retrieve the class and genus name
  data_fin <- left_join(data_file, iucn_titles, by = "q_wikidata")
  
  # join rates of change data onto the pollinator data, with full join to keep those that aren't pollinators
  data_fin <- left_join(data_fin, pollinat, by = c("genus_name" = "genus", "taxonomic_class" = "Class")) %>%
    select(-genus_name) %>%
    unique()
  
  # add column for whether that species is a pollinator, on basis of NAs in confidence column
  data_fin$pollinating[!is.na(data_fin$confidence)] <- "Y"
  data_fin$pollinating[is.na(data_fin$confidence)] <- "N"
  
  # check that the row numbers haven;t changed after merging
  data_check <- nrow(data_file) - nrow(data_fin)
  print(data_check)
  
  return(data_fin)
  
}

# iterate through each plot, and add it to previous for patchwork
combine_plots <- function(plot_list){
  total_species_plot <- list()
  for(i in 1:length(plot_list)){
    if(i == 1){
      total_species_plot <- plot_list[[i]]
    }
    if(i > 1){
      total_species_plot <- total_species_plot + plot_list[[i]]
    }
    
  }
  return(total_species_plot)
}

total_monthly_views_fish <- list()
for(i in 1:length(total_monthly_views)){
  total_monthly_views_fish[[i]] <- lapply(total_monthly_views[[i]], merge_fish)
  total_monthly_views_fish[[i]] <- lapply(total_monthly_views_fish[[i]], merge_trade)
  total_monthly_views_fish[[i]] <- lapply(total_monthly_views_fish[[i]], merge_pollinators)
  
}

# count number of views for articles of traded/pollinating

# boxplot of distribution
# function for calculating average views for each class for a given language
set_up_box_plot <- function(data_file){
  data_fin <- data_file %>%
    group_by(class_name, used, q_wikidata) %>% 
    summarise(total_views = sum(av_views)) %>% 
    ungroup()
  return(data_fin)
}

bound_views <- lapply(total_monthly_views_fish, rbindlist)

# run function to calculate average views per class over all languages
box_classes <- lapply(bound_views, set_up_box_plot)

# add column for language
for(i in 1:length(box_classes)){
  box_classes[[i]]$language <- languages_full[i]
}

# boxplot of total views per article, separated by traded/used
class_box_plot <- list()
for(i in 1:length(box_classes)){
  class_box_plot[[i]] <- box_classes[[i]] %>%
    filter(!class_name %in% c("insecta")) %>%
    mutate(class_name = factor(class_name, levels = c("reptilia", "actinopterygii", "mammalia", "aves", "insecta", "amphibia"),
                               labels = c("Reptiles", "Ray finned fishes", "Mammals", "Birds", "Insects", "Amphibians"))) %>%
    mutate(used = fct_reorder(used, -total_views, median)) %>%
    ggplot() +
    geom_boxplot(aes(x = class_name, log10(total_views), fill = used), outlier.shape = NA, size = 0.2) +
    scale_y_continuous(limits = c(0, 7.1), breaks = c(0, 2, 4, 6), labels = c("1", "100", "10,000", "1,000,000")) +
    scale_fill_manual("Traded or harvested", values = c("#0072B2", "#D55E00")) +
    facet_wrap(~language) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_text(size = 11, vjust = 0.9),
          legend.position = "none")
}

# run the function to combine all plots, and add the column layout
box_plot_series <- {combine_plots((class_box_plot[[1]] + ylab("Total article views") + theme(axis.text.y = element_text(angle = 45))) + 
                                    class_box_plot[2:5] + 
                                    (class_box_plot[[6]] + ylab("Total article views") + theme(axis.text.y = element_text(angle = 45))) +
                                    class_box_plot[7:9] + (class_box_plot[[10]] + theme(legend.position = "right"))) +
    plot_layout(ncol = 5)}


# save plot of traded/non-traded species on Wikipedia
ggsave("traded_species_wikipedia_boxplot.png", scale = 1.3, dpi = 350)



