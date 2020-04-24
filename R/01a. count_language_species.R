## script for calculating number of species (and number of complete time series) in each language and plotting
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(forcats)
library(patchwork)

# read in the rds for total monthly views
total_monthly_views <- readRDS(here::here("data/class_wiki_indices/submission_2/total_monthly_views_10-languages.rds"))

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
classes <- c("Actinopterygii", "Amphibia", "Aves", "Insecta", "Mammalia", "Reptilia")

# function for counting the number of species in each class/language grouping
count_total_species <- function(data_file){
  data_fin <- data_file %>%
    select(article) %>%
    unique() %>%
    tally()
  return(data_fin)
}

# build dataframe number of species for each grouping and reassign columns (maintains order of languages/classes)
run_count_total <- function(data_file, languages, classes){
  total_species <- list()
  for(i in 1:length(data_file)){
    total_species[[i]] <- lapply(data_file[[i]], count_total_species) %>% 
      data.frame()
  }

  # bind together, assign the columns to the list and return 
  total_species <- rbindlist(total_species)
  colnames(total_species) <- classes
  total_species$language <- languages
  return(total_species)
}

# plot number of species for each language, with separate factor order on the x axis - slice for each language
ind_species_plot <- list()
step <- 6
for(i in 1:length(languages)){
  ind_species_plot[[i]] <- run_count_total(total_monthly_views, languages, classes) %>% 
    reshape2::melt(id  = "language") %>%
    arrange(language) %>%
    group_by(language) %>%
    mutate(total = sum(value)) %>%
    ungroup() %>%
    arrange(desc(total)) %>%
    slice((step-5):step) %>%
    mutate(language = factor(language, levels = languages, 
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
            axis.title.y = element_text(size = 15))
    
  # step up slice of data for languages
    step <- step + 6
}

# iterate through each plot, and add it to previous for patchwork
combine_plots <- function(plot_list){
  for(i in 1:length(plot_list)){
    if(i == 1){
      total_species_plot <- plot_list[[i]]
    }
    if(i > 1){
      if(i == 5){
        total_species_plot <- total_species_plot + (plot_list[[i]] + ylab("Total species"))
      }
      else{
        total_species_plot <- total_species_plot + plot_list[[i]]
      }
    }
    
  }
  return(total_species_plot)
}

# run the function to combine all plots, and add the column layout
total_species_plot <- combine_plots(ind_species_plot) + plot_layout(ncol = 4)

# save the combined plot
ggsave("outputs/total_language_species.png", scale = 1.3, dpi = 350)






count_complete_species <- function(data_file){
  data_file %>%
    group_
}