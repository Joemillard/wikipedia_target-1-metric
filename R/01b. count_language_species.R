## script for calculating number of species (and number of complete time series) in each language and plotting
# **** check corrected for only pages with complete trends and for average view files ****

library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(forcats)
library(patchwork)
library(here)

# read in the rds for total monthly views - change this to average views
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/user_trends/total_monthly_views_10-languages.rds"))

# set up vector for languages, classes, and directory
languages_short <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
languages_full <- c("Spanish", "French", "German", "Japanese", "Italian", 
               "Arabic", "Russian", "Portuguese", "Chinese (Mandarin)", "English")
classes <- c("Actinopterygii", "Amphibia", "Aves", "Insecta", "Mammalia", "Reptilia")

# function for counting the number of species in each class/language grouping
count_language_species <- function(data_file){
  data_fin <- data_file %>%
    select(article) %>%
    unique() %>%
    tally()
  return(data_fin)
}

# build dataframe with number of species for each grouping and rename columns (maintains order of languages/classes)
run_count_total <- function(data_file, languages, classes){
  total_species <- list()
  all_language_species <- c()
  for(i in 1:length(data_file)){
      total_species[[i]] <- lapply(data_file[[i]], count_language_species) %>% 
        data.frame()
  }
  
  # bind together, assign the columns to the list and return 
  total_species <- rbindlist(total_species)
  colnames(total_species) <- classes
  total_species$language <- languages_short
  return(total_species)
}

## plot number of species for each language, for all series, with separate factor order on the x axis - slice for each language
ind_species_plot <- list()
step <- 6
for(i in 1:length(languages_short)){
  ind_species_plot[[i]] <- run_count_total(total_monthly_views, languages_short, classes) %>%
    reshape2::melt(id  = "language") %>%
    arrange(language) %>%
    group_by(language) %>%
    mutate(total = sum(value)) %>%
    ungroup() %>%
    arrange(desc(total)) %>% 
    slice((step-5):step) %>% print() %>%
    mutate(language = factor(language, levels = languages_short, 
                            labels = c("Spanish", "French", "German", "Japanese", "Italian", 
                                      "Arabic", "Russian", "Portuguese", "Chinese (Mandarin)", "English"))) %>%
    mutate(language = fct_reorder(language, -value)) %>%
    mutate(variable = fct_reorder(variable, -value)) %>%
    ggplot() +
      geom_bar(aes(x = variable, y = value), stat = "identity") +
      facet_wrap(~language) +
      xlab(NULL) +
      scale_y_continuous(limits = c(0, 11000), expand = c(0, 0)) +
      ylab(NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
            panel.grid.minor = element_blank(), 
            axis.title.y = element_text(size = 13, vjust = 0.9))
    
  # step up slice of data for languages
    step <- step + 6
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

# run the function to combine all plots, and add the column layout
total_species_plot <- {combine_plots(ind_species_plot) + plot_layout(ncol = 5)}

# assign a class column for each subset and collapse all lists to single dataframe
class_views <- list()
for(i in 1:length(total_monthly_views)){
  for(j in 1:length(total_monthly_views[[i]])){
    total_monthly_views[[i]][[j]]$class <- classes[j]
  }
  class_views[[i]] <- rbindlist(total_monthly_views[[i]])
}

# tally the number of species in each class and plot
class_views_plot <- rbindlist(class_views) %>%
  select(q_wikidata, class) %>%
  unique() %>%
  group_by(class) %>%
  tally() %>%
  mutate(class = fct_reorder(class, -n)) %>%
  ggplot() +
    geom_bar(aes(x = class, y = n), stat = "identity") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15000)) +
    xlab("") +
    ylab("Total species") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(), 
          axis.title.y = element_text(size = 13))

# combine the plots for the number of species in each class and the number of species in each language
total_species_language <- class_views_plot + total_species_plot + plot_layout(ncol = 1)

# save the combined plot
ggsave("outputs/total_language_species.png", scale = 1.3, dpi = 350)

## plot for complete time series
# count number of months of views per article
count_months <- function(data_file){
  data_fin <- data_file %>%
    group_by(article) %>%
    tally()
  
  return(data_fin)
}

# run function to count number of months of views per article
total_months <- list()
for(i in 1:length(total_monthly_views)) {
  total_months[[i]] <- lapply(total_monthly_views[[i]], count_months)
}

# count number of complete series for each language/class combination
series_frame <- list()
all_series_frame <- list()
for(i in 1:length(total_months)){
  for(j in 1:length(total_months[[i]])){
    counter <- 0 # set up the counter
    total_number <- length(total_months[[i]][[j]]$n) # calculate number of species for that class, and count through each species
    for(k in 1:length(total_months[[i]][[j]]$n)){
      if(total_months[[i]][[j]]$n[k] == 57){
        counter <- counter + 1 # if complete series, add one to counter
      }
    }
    proportion <- counter / total_number # calculate the proportion of complete series for that class
    series_frame[[j]] <- data.frame("languages"= languages_short[i], "classes" = classes[j], counter, total_number, proportion) # print the class, number of complete series, the total number of articles, and proportion complete
  }
  all_series_frame[[i]] <- rbindlist(series_frame)
}

# name each set of complete series dataframes according to the language vector
names(all_series_frame) <- languages_short


# create new vector for order of languages according to complete series
language_order <- rbindlist(all_series_frame) %>%
  group_by(languages) %>%
  summarise(total = sum(total_number)) %>%
  arrange(-total) %>%
  pull(languages)

# order names by the new ordered vector
all_series_frame <- all_series_frame[c(language_order)]

# update the full vector strings for each language
updated_language_order <- c("English", "Spanish", "French", "Portuguese", "Chinese", 
           "Italian", "German", "Russian", "Arabic", "Japanese")

# update plot for those in complete series i.e. data for every month
## plot number of species for each language, for all series, with separate factor order on the x axis - slice for each language
full_series_plot <- list()
for(i in 1:length(all_series_frame)){
  full_series_plot[[i]] <- all_series_frame[[i]] %>%
    mutate(languages = factor(languages, levels = language_order[i], labels = updated_language_order[i])) %>%
    mutate(classes = fct_reorder(classes, -total_number)) %>%    
    ggplot() +
    geom_bar(aes(x = classes, y = total_number), position = "identity", stat = "identity") +
    geom_bar(aes(x = classes, y = counter, fill = "Complete series"), position = "identity", stat = "identity") +
    facet_wrap(~languages) +
    xlab(NULL) +
    scale_fill_manual(NULL, values = "red") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, (max(all_series_frame[[i]]$total_number) + max(all_series_frame[[i]]$total_number*0.1)))) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
          panel.grid.minor = element_blank(), 
          axis.title.y = element_text(size = 13, vjust = 0.9),
          legend.position = "none")
}

# run the function to combine all plots, and add the column layout
total_species_series <- {combine_plots(full_series_plot) + plot_layout(ncol = 5)}
  
# save the plot for number of complete series
ggsave("complete_series_count.png", scale = 1.2, dpi = 350)




