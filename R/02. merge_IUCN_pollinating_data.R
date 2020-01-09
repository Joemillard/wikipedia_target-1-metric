# merge IUCN species with pollinator data
library(dplyr)

# read in species files
actin <- read.csv("data/iucn_ACTINOPTERYGII_monthly_views_user.csv", stringsAsFactors = FALSE)
amphib <- read.csv("data/iucn_AMPHIBIA_monthly_views_user.csv", stringsAsFactors = FALSE)
arach <- read.csv("data/iucn_ARACHNIDA_monthly_views_user.csv", stringsAsFactors = FALSE)
aves <- read.csv("data/iucn_AVES_monthly_views_user.csv", stringsAsFactors = FALSE)
chrondri <- read.csv("data/iucn_CHONDRICHTHYES_monthly_views_user.csv", stringsAsFactors = FALSE)
insect <- read.csv("data/iucn_INSECTA_monthly_views_user.csv", stringsAsFactors = FALSE)
mammal <- read.csv("data/iucn_MAMMALIA_monthly_views_user.csv", stringsAsFactors = FALSE)
reptile <- read.csv("data/iucn_REPTILIA_monthly_views_user.csv", stringsAsFactors = FALSE)

# source the functions R script
source("R/00. functions.R")

# read in onezoom leaf mapping
ordered_leaves <- read.csv("data/ordered_leaves.csv", stringsAsFactors = FALSE)

# read in the pollinator data
pollinat <- read.csv("data/COL_compiled_pollinators_add_conf.csv", stringsAsFactors = FALSE)

# make list of species dataframes
taxa <- list(actin, amphib, arach, aves, chrondri, insect, mammal, reptile)

# read in iucn_redlist data
iucn_redlist <- read.csv("data/redlist_data_2019_10_11_15_05_45.csv", stringsAsFactors = FALSE) %>%
  mutate(taxonid = as.character(taxonid))

# get list of all unique IUCN taxonids for all IUCN species with wiki data
unique_IUCN <- lapply(taxa, select_IUCN_IDs)

# filter the ordered leaves for all those with IUCN wiki views
ordered_leaves_wiki <- ordered_leaves %>%
  filter(iucn %in% unlist(unique_IUCN))

# merge ordered_leaves_wiki with the order level redlist data
ordered_leaves_wiki <- inner_join(ordered_leaves_wiki, iucn_redlist, by = c("iucn" = "taxonid"))

# match the IUCN species with wiki views to the pollinator data
merged_taxa <- lapply(taxa, merge_wiki)

# for each taxa subset, merge with the pollinator data and create new column for pollinating/non
merged_pollinators <- lapply(merged_taxa, merge_pollinat)

saveRDS(merged_pollinators, "IUCN_pollinators.rds")
