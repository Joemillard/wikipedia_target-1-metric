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
               "Arabic", "Russian", "Portuguese", "Chinese", "English")
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
ind_species <- list()
step <- 6
for(i in 1:length(languages_short)){
  ind_species[[i]] <- run_count_total(total_monthly_views, languages_short, classes) %>%
    reshape2::melt(id  = "language") %>%
    arrange(language) %>%
    group_by(language) %>%
    mutate(total = sum(value)) %>%
    ungroup() %>%
    arrange(desc(total)) %>% 
    slice((step-5):step) %>% 
    mutate(language = factor(language, levels = languages_short, 
                            labels = c("Spanish", "French", "German", "Japanese", "Italian", 
                                      "Arabic", "Russian", "Portuguese", "Chinese", "English"))) %>%
    mutate(language = fct_reorder(language, -value)) %>%
    mutate(variable = fct_reorder(variable, -value))
  
  ind_species_plot[[i]] <- ind_species[[i]] %>%
      ggplot() +
        geom_bar(aes(x = variable, y = value), stat = "identity") +
        facet_wrap(~language) +
        xlab(NULL) +
        scale_y_continuous(limits = c(0, max(ind_species[[i]]$value * 1.1)), expand = c(0, 0)) +
        ylab(NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
              panel.grid = element_blank(), 
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
total_species_plot <- {combine_plots((ind_species_plot[[1]] + ylab("Total species")) + ind_species_plot[2:5] + (ind_species_plot[[6]] + ylab("Total species")) + ind_species_plot[7:10]) + plot_layout(ncol = 5)}

# save the plot for species in each language
ggsave("all_language_species_plot.png", scale = 1.1, dpi = 350)

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
    xlab(NULL) +
    ylab("Total species") +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title.y = element_text(size = 13),
          axis.text = element_text(size = 11))

# plot for number of views in each language
# calculate total views for each language
group_views <- function(data_file, languages){
  language_total <- c(rep(0, 10))
  for(i in 1:length(data_file)){
    for(j in 1:length(data_file[[i]])){
      language_total[[i]] <- language_total[[i]] + sum(data_file[[i]][[j]]$av_views, na.rm = TRUE)
    }
  }
  
  # build dataframe for views for each language and return it
  language_total <- data.frame("language" = languages, "views" = language_total)
  return(language_total)
}

# run function for each language views and build bar plot
plot_views <- group_views(total_monthly_views, languages = languages_full) %>%
  mutate(language = fct_reorder(language, -views)) %>%
  ggplot() +
  geom_bar(aes(x = language, y = views), stat = "identity") + 
  geom_text(aes(x = language, y = views + 50000000, label = (round(views/1000000, digits = 2)))) +
  xlab(NULL) +
  ylab("Total user views (millions)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200000000), 
                     breaks = c(0, 300000000, 600000000, 900000000, 1200000000), labels = c("0", "300", "600", "900", "1200")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 13))

# combine the plots for the total species for all class/languages and views for all languages
plot_views + class_views_plot + plot_layout(ncol = 1)

ggsave("all_language_views-species.png", scale = 1.2, dpi = 350)

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
    print()
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

# plot for the top 5 viewed species for each language
bound_views <- lapply(total_monthly_views, rbindlist)

# function for calculating the top 5 viewed species for each language
calc_top <- function(data_file){
  data_fin <- data_file %>%
    group_by(class, q_wikidata, article) %>%
    summarise(total_views = sum(av_views)) %>%
    ungroup() %>%
    arrange(-total_views) %>%
    slice(0:5)
  return(data_fin)
}

# run function for calculating top 5 species for each language
top_views <- lapply(bound_views, calc_top)

# add the language to each dataframe of top 5 species
top_species <- list() 
for(i in 1:length(top_views)){
  top_species[[i]] <- top_views[[i]] %>%
    mutate(article = fct_reorder(article, -total_views)) %>% 
    mutate(language = languages_full[i])
}

# all unique species IDs
top_species_frame <- rbindlist(top_species) %>%
  select(q_wikidata) %>%
  unique()

# make dataframe for all unique species in Latin binomial with q_wikidata - note order of species vector
latin_species <- data.frame("q_wikidata" = top_species_frame$q_wikidata,
                            "species" = c("Homo sapiens",
                                          "Ailuropoda melanoleuca",
                                          "Canis lupus",
                                          "Panthera tigris",
                                          "Panthera onca",
                                          "Phascolarctos cinereus",
                                          "Ornithorhynchus anatinus",
                                          "Natrix natrix",
                                          "Turdus merula",
                                          "Balaenoptera musculus",
                                          "Seriola quinqueradiata",
                                          "Orcinus orca",
                                          "Paguma larvata",
                                          "Nyctereutes procyonoides",
                                          "Myocastor coypus",
                                          "Mantis religiosa",
                                          "Panthera pardus",
                                          "Acinonyx Jubatus",
                                          "Gulo gulo",
                                          "Prionailurus bengalensis"))

# bind Latin binomial species on to each frame and then plot
top_species_plot <- list()

# create list of colours for each class
bar_colours <- list(c("#000000"),
                    c("#000000"),
                    c("#000000", "#E69F00", "#56B4E9"),
                    c("#000000", "#009E73"),
                    c("#000000", "#F0E442"),
                    c("#000000"),
                    c("#000000"),
                    c("#000000"),
                    c("#000000"),
                    c("#000000"))

# plot top 5 species for each language, including all classes 
for(i in 1:length(top_species)){
  top_species_plot[[i]] <- inner_join(top_species[[i]], latin_species, by = c("q_wikidata")) %>%
    mutate(species = fct_reorder(species, -total_views)) %>%
    mutate(class = factor(class, levels = c("Mammalia", "Reptilia", "Aves", "Actinopterygii", "Insecta"))) %>%
    print() %>%
      ggplot() +
      geom_bar(aes(x = species, y = total_views, fill = class), stat = "identity") +
      scale_y_continuous(expand = c(0, 0), labels = function(x) format(x, scientific = FALSE, big.mark=","), limits = c(0, (max(top_views[[i]]$total_views) + max(top_views[[i]]$total_views*0.1)))) +
      scale_fill_manual("Taxonomic class", values = bar_colours[[i]]) +
      facet_wrap(~language) +
      ylab(NULL) +
      xlab(NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
        panel.grid = element_blank(), 
        axis.title.y = element_text(size = 13, vjust = 0.9),
        legend.position = "none")
}

# add full vectors for language, using the same order, and then order by those with most species
names(top_species_plot) <- languages_full
top_species_plot <- top_species_plot[updated_language_order]

# combine the plot for top 5 species
top_viewed_species <- {combine_plots(top_species_plot) + plot_layout(ncol = 5)} 

# save plot for top 5 number of views
ggsave("top_5_species.png", scale = 1.1, dpi = 350)

# average views per class plot
# function for calculating average views for each class for a given language
calc_average <- function(data_file){
  data_fin <- data_file %>%
    group_by(class, q_wikidata) %>% 
    summarise(total_views = sum(av_views)) %>% 
    ungroup() %>%
    group_by(class) %>%
    summarise(average_views = median(total_views)) %>%
    ungroup() %>%
    arrange(-average_views)
  return(data_fin)
}

# run function to calculate average views per class over all languages
average_classes <- lapply(bound_views, calc_average)

# add column for language
for(i in 1:length(average_classes)){
  average_classes[[i]]$language <- languages_full[i]
}

# add full vectors for language, using the same order, and then order by those with most species
names(average_classes) <- languages_full
average_classes <- average_classes[updated_language_order]

# build plot for median number of views per class/language combination
av_series_plot <- list()
for(i in 1:length(average_classes)){
  av_series_plot[[i]] <- average_classes[[i]] %>% print() %>%
    mutate(class = fct_reorder(class, -average_views, median)) %>%
    ggplot() +
    geom_bar(aes(x = class, y = average_views), position = "identity", stat = "identity") +
    facet_wrap(~language) +
    xlab(NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, (max(average_classes[[i]]$average_views) + max(average_classes[[i]]$average_views*0.1)))) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1), 
          panel.grid = element_blank(), 
          axis.title.y = element_text(size = 13, vjust = 0.9),
          legend.position = "none")
}

# run the function to combine all plots, and add the column layout
av_species_series <- {combine_plots((av_series_plot[[1]] + ylab("Total median views")) + av_series_plot[2:10]) + plot_layout(ncol = 5)}

# boxplot of distribution
# function for calculating average views for each class for a given language
set_up_box_plot <- function(data_file){
  data_fin <- data_file %>%
    group_by(class, q_wikidata) %>% 
    summarise(total_views = sum(av_views)) %>% 
    ungroup()
  return(data_fin)
}

# run function to calculate average views per class over all languages
box_classes <- lapply(bound_views, set_up_box_plot)

# add column for language
for(i in 1:length(box_classes)){
  box_classes[[i]]$language <- languages_full[i]
}

# add full vectors for language, using the same order, and then order by those with most species
names(box_classes) <- languages_full
box_classes <- box_classes[updated_language_order]

# build plot for boxplot of views per class/language combination
class_box_plot <- list()
for(i in 1:length(box_classes)){
class_box_plot[[i]] <- box_classes[[i]] %>%
  mutate(class = fct_reorder(class, -total_views, median)) %>%
  ggplot() +
    geom_boxplot(aes(class, log10(total_views)), outlier.shape = NA, size = 0.2) +
    facet_wrap(~language) +
    scale_y_continuous(limits = c(0, 7.5), breaks = c(0, 2, 4, 6), labels = c("1", "100", "10,000", "1,000,000")) +
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
                                    class_box_plot[7:10]) +
                                    plot_layout(ncol = 5)}


ggsave("total_view_distribution.png", scale = 1.1, dpi = 350)

# build plot of median views for pollinating and non-pollinating and traded/non-traded
