# merge IUCN species with pollinator data
library(dplyr)

# read in species files
actin <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_ACTINOPTERYGII_monthly_views_user.csv", stringsAsFactors = FALSE)
amphib <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_AMPHIBIA_monthly_views_user.csv", stringsAsFactors = FALSE)
arach <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_ARACHNIDA_monthly_views_user.csv", stringsAsFactors = FALSE)
aves <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_AVES_monthly_views_user.csv", stringsAsFactors = FALSE)
chrondri <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_CHONDRICHTHYES_monthly_views_user.csv", stringsAsFactors = FALSE)
insect <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_INSECTA_monthly_views_user.csv", stringsAsFactors = FALSE)
mammal <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_MAMMALIA_monthly_views_user.csv", stringsAsFactors = FALSE)
reptile <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/iucn_REPTILIA_monthly_views_user.csv", stringsAsFactors = FALSE)

# read in onezoom leaf mapping
ordered_leaves <- read.csv("~/PhD/Aims/Aim 3 - quantifying pollinator cultural value/data/Rosindell_taxonomy/ordered_leaves.csv", stringsAsFactors = FALSE)

# read in the pollinator data
pollinat <- read.csv("C:/Users/Joeym/Documents/PhD/Aims/Aim 2 - understand response to environmental change/COL_compiled_pollinators_add_conf.csv", stringsAsFactors = FALSE)

# make list of species dataframes
taxa <- list(actin, amphib, arach, aves, chrondri, insect, mammal, reptile)

# read in iucn_redlist data
iucn_redlist <- read.csv("wikipedia_data/redlist_data_2019_10_11_15_05_45.csv", stringsAsFactors = FALSE) %>%
  mutate(taxonid = as.character(taxonid))

# get list of all unique IUCN taxonids for all IUCN species with wiki data
select_IUCN_IDs <- function(data) {
  result <- data %>%
    select(taxonid) %>%
    unique()
  return(result)
}

unique_IUCN <- lapply(taxa, select_IUCN_IDs)

# filter the ordered leaves for all those with IUCN wiki views
ordered_leaves_wiki <- ordered_leaves %>%
  filter(iucn %in% unlist(unique_IUCN))

# merge ordered_leaves_wiki with the order level redlist data
ordered_leaves_wiki <- inner_join(ordered_leaves_wiki, iucn_redlist, by = c("iucn" = "taxonid"))

# match the IUCN species with wiki views to the pollinator data
merge_wiki <- function(data) {
  data$taxonid <- as.character(data$taxonid)
  joined_wiki <- inner_join(data, ordered_leaves_wiki, by = c("taxonid" = "iucn"))
  joined_wiki <- joined_wiki %>%
    select(article, year, month, total_views, taxonid, name, order_name, family_name, class_name)
  return(joined_wiki)
}

merged_taxa <- lapply(taxa, merge_wiki)

# for each taxa subset, merge with the pollinator data and create new column for pollinating/non
merge_pollinat <- function(data) {
  joined_pollinat <- full_join(data, pollinat, by = c("name" = "scientific_name"))
  joined_pollinat <- joined_pollinat %>%
    filter(!is.na(article)) %>%
    select(article, taxonid, name, Class, Order, confidence, fact_conf, class_name, order_name) %>%
    unique()
  
  joined_pollinat$pollinating[is.na(joined_pollinat$Class)] <- "N"
  joined_pollinat$pollinating[!is.na(joined_pollinat$Class)] <- "Y"
  return(joined_pollinat)
}

merged_pollinators <- lapply(merged_taxa, merge_pollinat)

saveRDS(merged_pollinators, "IUCN_pollinators.rds")
