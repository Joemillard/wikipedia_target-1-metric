### functions for Wikipedia cultruomics analysis - 09/01/2019

# get list of all unique IUCN taxonids for all IUCN species with wiki data
select_IUCN_IDs <- function(data) {
  result <- data %>%
    select(taxonid) %>%
    unique()
  return(result)
}

# match the IUCN species with wiki views to the pollinator data
merge_wiki <- function(data) {
  data$taxonid <- as.character(data$taxonid)
  joined_wiki <- inner_join(data, ordered_leaves_wiki, by = c("taxonid" = "iucn"))
  joined_wiki <- joined_wiki %>%
    select(article, year, month, total_views, taxonid, name, order_name, family_name, class_name)
  return(joined_wiki)
}

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

# aggregate the views for each of birds, mammals, and insects
run_dat <- function(terms_1, av_all){
  
  terms <- terms_1  
  
  terms$year <- substr(terms$timestamp, start = 1, stop = 4)
  terms$month <- substr(terms$timestamp, start = 5, stop = 6)
  terms$day <- substr(terms$timestamp, start = 7, stop = 8)
  
  terms <- terms %>%
    select(article, q_wikidata, year, month, views)
  
  # calculate average per each month
  terms_av <- terms %>%
    group_by(article, q_wikidata, year, month) %>%
    summarise(av_views = sum(views)) %>%
    ungroup()
  
  if(av_all == TRUE) {
    
    # calculate average per each month
    terms_av_year <- terms %>%
      group_by(year, month) %>%
      summarise(av_views = sum(views)) %>%
      ungroup()
    
    terms_av_year$date <- paste(terms_av_year$year, terms_av_year$month, sep = "-")
    terms_av_year$date <- as.Date(paste(terms_av_year$date,"-01",sep=""))
    return(terms_av_year)
    
  }
  
  else{
    terms_av$date <- paste(terms_av$year, terms_av$month, sep = "-")
    terms_av$date <- as.Date(paste(terms_av$date,"-01",sep=""))
    
    return(terms_av)
  }
}

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

# filter out any taxonomic orders without any yesses
filter_pollinator <- function(pollinators){
  pollinators <- pollinators %>%
    filter(pollinating == "Y")
  
  return(pollinators)
}

# rescale each dataframe to start at 1970 and merge back with the views
rescale_iucn <- function(iucn_no_dup){
  iucn_no_dup_2 <- iucn_no_dup %>%
    mutate(year = as.numeric(year)) %>%
    mutate(month = as.numeric(month))
  
  rescale <- iucn_no_dup_2 %>%
    select(year, month) %>%
    unique() %>%
    mutate(dec_date = 1970 + (year - 2015)*12 + month)
  
  # merge back with the view data
  rescale <- inner_join(iucn_no_dup_2, rescale, by = c("year", "month"))
  return(rescale)
}

# create data.frame for those with 53 rows (full time series)
select_comp <- function(x){
  insect_comp <- x %>%
    select(article) %>%
    group_by(article) %>%
    tally() %>%
    ungroup() %>%
    filter(n == 57)
  
  # subset from insects only those with 53 rows i.e. complete
  iucn_insects_filt <- x %>%
    filter(article %in% insect_comp$article)
  return(iucn_insects_filt)
  
}

# select just for new year column, rename columns, and remove NAs
structure_lpi <- function(iucn_no_dup, poll){
  iucn_no_dup <- iucn_no_dup %>%
    filter(pollinating %in% poll) %>%
    select(article, dec_date, av_views) %>%
    mutate(ID = as.character(as.numeric(as.factor(article)))) %>%
    rename(Binomial = article) %>%
    rename(year = dec_date) %>%
    rename(popvalue = av_views) %>%
    filter(complete.cases(.)) %>%
    mutate(Binomial = ID) %>%
    select(Binomial, ID, year, popvalue)
}

# select just for new year column, rename columns, and remove NAs
structure_lpi_overall <- function(iucn_no_dup, poll){
  iucn_no_dup <- iucn_no_dup %>%
    select(article, dec_date, av_views) %>%
    mutate(ID = as.character(as.numeric(as.factor(article)))) %>%
    rename(Binomial = article) %>%
    rename(year = dec_date) %>%
    rename(popvalue = av_views) %>%
    filter(complete.cases(.)) %>%
    mutate(Binomial = ID) %>%
    select(Binomial, ID, year, popvalue)
}

# bind the random values back onto main dataframe and calculate adjusted lpi
join_random <- function(data){
  data$Year <- as.character(data$Year)
  data <- inner_join(data, random_wiki_lpi, by = c("Year", "date"))
  #data$adjusted_lpi <- data$LPI_final.x - data$LPI_final.y
  return(data)
}

# create vectors for each of the list of pollinating animals
extract_polls <- function(data, poll){
  unique_species <- c(data %>% 
                        filter(pollinating == poll) %>%
                        select(article) %>%
                        unique())
  
  return(unique_species)
}

# adjust the lambdas for each of the subsets with random values
adjust_lambda <- function(x, random_data){
  data <- reshape2::melt(x, id = c("X", "SpeciesSSet", "Freq"))
  data <- inner_join(data, random_data, by = c("variable" = "date"))
  data$adjusted_lambda <- data$value - data$lamda
  data <- data %>%
    dplyr::select(X, SpeciesSSet, Freq, variable, adjusted_lambda)
  
  data <- dcast(data, X + SpeciesSSet + Freq ~ variable)
  return(data)
}

# function to unique the articles with taxonid 
select_col <- function(data) {
  fin <- data %>%
    dplyr::select(article, taxonid) %>%
    unique()
  return(fin)
}