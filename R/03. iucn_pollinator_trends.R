## script for constructing trends on ecological system
library(dplyr)
library(data.table)
library(rlpi)
library(ggplot2)
library(forcats)
library(rvest)
library(tm)

# read in total monthtly views user
# iucn_views <- read.csv("wikipedia_data/iucn_total_monthly_views_user.csv", stringsAsFactors = FALSE)

# read in view data for daily views
bird_views <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/bird_user_trends.csv", stringsAsFactors = FALSE)
mammal_views <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/mammal_user_trends.csv", stringsAsFactors = FALSE)
insect_views <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/insect_user_trends.csv", stringsAsFactors = FALSE)

# aggregate the views for each of birds, mammals, and insects
run_dat <- function(terms_1, av_all){
  
  terms <- terms_1  
  
  terms$year <- substr(terms$timestamp, start = 1, stop = 4)
  terms$month <- substr(terms$timestamp, start = 5, stop = 6)
  terms$day <- substr(terms$timestamp, start = 7, stop = 8)
  
  terms <- terms %>%
    select(article, year, month, views)
  
  # calculate average per each month
  terms_av <- terms %>%
    group_by(article, year, month) %>%
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

# calculate monthly total views
bird_av <- run_dat(bird_views, av_all = FALSE)
mammal_av <- run_dat(mammal_views, av_all = FALSE)
insect_av <- run_dat(insect_views, av_all = FALSE)

# merge the view data and change article to no space
iucn_views <- rbind(bird_av, mammal_av, insect_av)
iucn_views$article <- gsub("_", " ", iucn_views$article)

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
  group_by(class_name, order_name, pollinating) %>%  
  tally() %>% 
  ungroup()
  
pollinator_plot <- inner_join(order_pollinator_counts, order_pollinators, by = c("class_name", "order_name")) %>%
  mutate(pollinating = factor(pollinating, levels = c("Y", "N"), labels = c("Yes", "No"))) %>%
  mutate(class_name = factor(class_name, levels = c("AVES", "INSECTA", "MAMMALIA"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() +
  geom_bar(aes(x = NA, fill = pollinating, y = n.x), stat = "identity") +
  theme(axis.text = element_text(angle = 90)) +
  facet_wrap(~class_name) +
  ylab("Total species") +
  xlab("") +
  scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8900)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("pollinating_iucn-species_wiki.png", dpi = 350, scale = 1.1)

# join the pollinator data onto the view data, for each of 6 subsets (birds, Y/N; insects, Y/N; mammals, Y/N)
iucn_views_poll <- list()
for(i in 1:length(iucn_pollinators)){
  iucn_views_poll[[i]] <- inner_join(iucn_views, iucn_pollinators[[i]], by = c("article")) %>%
    select(article, year, month, pollinating, av_views) %>%
    arrange(article, pollinating, year, month) %>%
    unique() %>%
    data.frame()
}



### put each dataframe into the lpi structure and run lpi
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

iucn_views_poll <- lapply(iucn_views_poll, rescale_iucn)

# create data.frame for those with 43 rows for each
select_comp <- function(x){
  insect_comp <- x %>%
    select(article) %>%
    group_by(article) %>%
    tally() %>%
    ungroup() %>%
    filter(n == 53)
  
  # subset from insects only those with 43 rows i.e. complete
    iucn_insects_filt <- x %>%
      filter(article %in% insect_comp$article)
      return(iucn_insects_filt)
  
}

iucn_pollinators_comp <- lapply(iucn_views_poll, select_comp)

# save pollinator list as rds
saveRDS(iucn_pollinators_comp, "iucn_pollinators_comp.rds")

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

# separate lists for Y/N pollinating by class, and create list of 6 objects
lpi_structured_Y <- lapply(iucn_pollinators_comp, structure_lpi, poll = "Y")
lpi_structured_N <- lapply(iucn_pollinators_comp, structure_lpi, poll = "N")
lpi_structured <- c(lpi_structured_Y, lpi_structured_N) 

# list of classes by pollinating
groupings <- c("bird_Y", "insect_Y", "mammal_Y", "bird_N", "insect_N", "mammal_N")
class_group <- c("bird", "insect", "mammal","bird", "insect", "mammal")
pollinat_YN <- c("Y", "Y", "Y", "N", "N", "N")

# write each file to table
for(i in 1:length(groupings)){
  write.table(lpi_structured[[i]], paste(groupings[i], "data.txt", sep = "_"), row.names = FALSE)
  infile_df <- data.frame(FileName = paste(groupings[i], "data.txt", sep = "_"), Group = 1, Weighting = 1)
  write.table(infile_df, paste(groupings[i], "pages_all_infile.txt", sep = "_"), row.names = FALSE)
}

lpi_trends <- list()
for(i in 1:length(groupings)){
  lpi_trends[[i]] <- LPIMain(paste(groupings[i], "pages_all_infile.txt", sep = "_"), REF_YEAR = 1977, PLOT_MAX = 2029)
}

# save rds for pollinator trends
saveRDS(lpi_trends, "lpi_trends_pollinator_comp-series_2.rds")



### make plot for trends
lpi_trends <- readRDS("lpi_trends_pollinator_comp-series_2.rds")

# add column for class and pollinating
for(i in 1:length(class_group)){
  lpi_trends[[i]]$class <- class_group[i]
  lpi_trends[[i]]$pollinat <- pollinat_YN[i]
  lpi_trends[[i]]$groupings <- pollinat_YN[i]
  lpi_trends[[i]]$date <- as.numeric(rownames(lpi_trends[[i]]))
  lpi_trends[[i]]$Year <- (lpi_trends[[i]]$date - 1970)/12 + 2015
  #rownames(lpi_trends[[i]]) <- lpi_trends[[i]]$Year 
}

# make plot of trends over time for each grouping
lpi_trends %>%
  rbindlist %>%
  filter(LPI_final != -99) %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_line(aes(x = Year, y = LPI_final, group = groupings, colour = pollinat), size = 1) + 
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = groupings, fill = pollinat), alpha = 0.2) +
  facet_wrap(~class, scales = "free_x") +
  theme_bw() + 
  theme(panel.grid = element_blank())

### random monthly trend
# random_monthly_trends_init <- read.csv("wikipedia_data/random_total_monthly_views_user.csv", stringsAsFactors = FALSE)
random_monthly_trends_init <- read.csv("wikipedia_data/view_data_by_class_totals_useronly/random_user_trends.csv", stringsAsFactors = FALSE)

# aggregate the views for the random trend
random_monthly_trends_init <- run_dat(random_monthly_trends_init, av_all = FALSE)

# subset the random pages for just 2000 pages
#random_unique <- unique(random_monthly_trends_init$article)[0:5000]
random_monthly_trends_init <- random_monthly_trends_init %>%
  mutate(year = as.numeric(year)) %>%
  mutate(month = as.numeric(month))
  
# select only complete time series
random_monthly_trends_init <- select_comp(random_monthly_trends_init)

# subtract 2015 to get first year as '0', then multiply by 12 so each original year represents 12 'years', add months and add 1970 for baseline
random_monthly_trends <- plyr::ddply(random_monthly_trends_init, c("article", "year", "month"), summarise, dec_date = (1970 + (year - 2015)*12 + month))

# Merge back into view data
random_monthly_trends <- merge(random_monthly_trends, random_monthly_trends_init, by=c("article", "year", "month"), all = T)

# Create unique ID for each page/species using factor
random_monthly_trends$id <- as.numeric(as.factor(random_monthly_trends$article))

# Select and order columns
random_monthly_trends <- random_monthly_trends[c("article", "id", "dec_date", "av_views")]

# Forcing columns into format for rlpi package
colnames(random_monthly_trends) <- c("Binomial", "ID", "year", "popvalue")
random_monthly_trends$Binomial <- random_monthly_trends$ID

# *** Removing NAs in data (because of error 'Error in { : task 1 failed - "missing value where TRUE/FALSE needed"')
# *** Should add to rlpi the error check for NAs in all columns (apart from pop value)
random_monthly_trends <- random_monthly_trends[complete.cases(random_monthly_trends), ]

write.table(random_monthly_trends, "random_trend_data.txt", row.names = FALSE)
infile_df <- data.frame(FileName="random_trend_data.txt", Group = 1, Weighting = 1)
write.table(infile_df, "random_pages_all_infile.txt", row.names = F)

# run rlpi and save as rds
random_wiki_lpi <- LPIMain("random_pages_all_infile.txt", REF_YEAR = 1977, PLOT_MAX = 2029, basedir = ".")
saveRDS(random_wiki_lpi, "lpi_trend_random_3.rds")
random_wiki_lpi <- readRDS("lpi_trend_random_3.rds")

# adjust the year column
random_wiki_lpi$date <- as.numeric(rownames(random_wiki_lpi))
random_wiki_lpi$Year <- (random_wiki_lpi$date - 1970)/12 + 2015
random_wiki_lpi$Year <- as.character(random_wiki_lpi$Year)

# bind the random values back onto main dataframe and calculate adjusted lpi
join_random <- function(data){
  data$Year <- as.character(data$Year)
  data <- inner_join(data, random_wiki_lpi, by = c("Year", "date"))
  data$adjusted_lpi <- data$LPI_final.x - data$LPI_final.y
  return(data)
}

lpi_trends <- lapply(lpi_trends, join_random)

#### Robin calculating lambdas
lpi_trends_corr = lpi_trends
#
for (i in 1:length(lpi_trends_corr)) {
  group_index = lpi_trends_corr[[i]]

  index_values = group_index$LPI_final.x
  lambdas = diff(log10(index_values[1:53]))

  random_index = group_index$LPI_final.y
  r_lambdas = diff(log10(random_index[1:53]))

  corrected_lambdas = lambdas - r_lambdas

  corrected_index = cumprod(10^c(0, corrected_lambdas))

    lpi_trends_corr[[i]]$LPI_final.x[1:53] = corrected_index

}

####

### make plot of trends over time for each grouping, adjusted for random
lpi_trends_corr %>%
  rbindlist %>% 
  filter(LPI_final.x !=  -99) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(pollinat = factor(pollinat, levels = c("Y", "N"), labels = c("Yes", "No"))) %>% 
  mutate(class = factor(class, levels = c("bird", "insect", "mammal"), labels = c("Birds", "Insects", "Mammals"))) %>%
  ggplot() +
  geom_point(aes(x = Year, y = LPI_final.x, colour = pollinat)) + 
  geom_line(aes(x = Year, y = LPI_final.x, colour = pollinat)) +
  #geom_smooth(aes(x = Year, y = adjusted_lpi, group = groupings, colour = pollinat, fill = pollinat), lm = "loess") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, colour = "grey") +
  facet_wrap(~class, scales = "free_x") +
  scale_colour_manual(name = "Pollinating", values = c("black", "red")) +
  scale_fill_manual(name = "Pollinating", values = c("black", "red")) +
  theme_bw() +
  ylab("Random adjusted index")

ggsave("pollinating_trends_comp_5.png", scale = 1.1, dpi = 350)
  
### wikipedia text-mining for similarity to pollination page
# set up vectors for wiki_prefix and taxonomic orders
wiki_prefix <- "https://en.wikipedia.org/wiki/"

# create vectors for each of the list of pollinating animals
extract_polls <- function(data, poll){
  unique_species <- c(data %>% 
    filter(pollinating == poll) %>%
    select(article) %>%
    unique())
    
  return(unique_species)
}

# combine lists for the species
unique_species_Y <- lapply(iucn_pollinators_comp, extract_polls, poll = "Y")
unique_species_N <- lapply(iucn_pollinators_comp, extract_polls, poll = "N")
unique_species <- c(unique_species_Y, unique_species_N)

# put each article in wikipedia format
species_wiki_clean <- list()

# replace space with an underscore for each string
for(i in 1:length(unique_species)){
  unique_species_wiki <- c()
  for(j in 1:length(unique_species[[i]]$article)){
    unique_species_wiki[j] <- gsub(" ", "_", unique_species[[i]]$article[j])
  }
  species_wiki_clean[[i]] <- unique_species_wiki
  print(i)
}

# function for scraping the html files for each page
# still need to add in removing all the species names from the pollination page
extract_html <- function(taxa_pages, wiki_prefix){
  
  # create start up vectors
  wiki_pages <- list()
  
  # iterate through eahc of the taxonomic orders
  for(i in 1:length(taxa_pages)){
    
    tryCatch({
    
    # read the html for each taxonomic order
    taxa <- read_html(paste(wiki_prefix, taxa_pages[i], sep = ""))
    
    # extract all the links from each of the taxonomic orders
    wiki_pages[[i]] <- taxa %>%
      html_nodes("p") %>%
      html_text() %>%
      data.frame() %>%
      rename(text = ".") %>%
      mutate(doc_id = taxa_pages[i])
    
    print(i)
    
    }, error = function(x) print(c(i, taxa_pages[i])))
    
  }
  
  # bind the links from each taxonomy and return
  wiki_pages <- rbindlist(wiki_pages)
  return(wiki_pages)
  
}

# run function for downloadikng wiki html for each page
taxa_html <- lapply(species_wiki_clean, extract_html, wiki_prefix)

# aggregate paragraphs by page and add page back in for taxa
pollination_text <- list()
for(i in 1:length(taxa_html)){
  pollination_text[[i]] <- aggregate(text ~ doc_id, data = taxa_html[[i]], paste, collapse = "")
}

# save pollination text as rds file
saveRDS(pollination_text, "scraped_pollination_text.rds")
pollination_text <- readRDS("scraped_pollination_text.rds")

pollination_text <- rbindlist(pollination_text)

# and for pollination page
html_pollination <- lapply("Pollination", extract_html, wiki_prefix)
html_pollination_text <- list()
html_pollination_text[[1]] <- aggregate(text ~ doc_id, data = html_pollination[[1]], paste, collapse = "")

bound_text <- rbind(pollination_text, html_pollination_text[[1]])

# function for cleaning up the data
# remove any characters within ()
clean_text <- function(data){
  pollination_text <- data

  # convert to dtm matrix
  poll_documents <- VCorpus(DataframeSource(pollination_text))

  # clean up the corpus
  poll_documents <- poll_documents %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stemDocument) %>%
    tm_map(content_transformer(tolower))
  
  return(poll_documents)
  
}

run_sim <- function(data){
  poll_documents <- data
  
  poll_documents <- DocumentTermMatrix(poll_documents, control = list(weighting = weightTfIdf))
  
  poll_matrix <-  tm::removeSparseTerms(poll_documents, 0.995) 
  poll_matrix <- as.matrix(poll_matrix) 
  
  # calculate distance matrix and clusters
  dist.matrix <- proxy::dist(poll_matrix, method = "cosine") 
  return(dist.matrix)
}
 
pollination_text_clean <- clean_text(bound_text)

pollination_text_sim <- run_sim(pollination_text_clean)

# build dtm matrix and weight for tf-idf
melted_distance <- melt(as.matrix(pollination_text_sim)) %>%
  filter(Var2 == "Pollination")

# save the pollination relatedness data
saveRDS(melted_distance, "pollination_sim_0.995_stem_lower.rds")

# join the similarities back onto the index
joined_views_poll <- inner_join(melted_distance, views_pages, c("Var1" = "article"))

