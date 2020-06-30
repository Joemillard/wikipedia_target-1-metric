# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(boot)
library(RColorBrewer)
library(viridis)
library(forcats)
library(cowplot)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/average_lambda")

# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")

# read in the lambda files 
random_trend <- readRDS("Z:/submission_2/overall_daily-views_10-random-languages_from_lambda_no-species.rds")

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend)){
  random_trend[[i]]$date <- as.numeric(rownames(random_trend[[i]]))
  random_trend[[i]]$Year <- (random_trend[[i]]$date - 1970)/12 + 2015
  random_trend[[i]]$Year <- as.character(random_trend[[i]]$Year)
  
  # calculate lambda for random
  random_trend[[i]] <- random_trend[[i]] %>%
    filter(date %in% c(1977:2033))
  random_trend[[i]]$lamda = c(0, diff(log10(random_trend[[i]]$LPI_final[1:57])))
  random_trend[[i]]$date <- paste("X", random_trend[[i]]$date, sep = "")
  random_trend[[i]]$language <- languages[i]
}

# bind together and plot the random trends
rbindlist(random_trend) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LPI_final, group = language)) +
  geom_ribbon(aes(x = Year, ymin = CI_low, ymax = CI_high, group = language), alpha = 0.3) +
  facet_wrap(~language) +
  theme_bw()

# string for pollinating classes, plus random
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(classes, directory){
  
  # bring in all the files in that directory and assign to a list
  view_files <- list()
  for(i in 1:length(languages)){
    view_files[[i]] <- list.files(directory, pattern = languages[i])
  }
  
  # unlist the files in the correct order
  file_order <- unlist(view_files)
  
  # set up empty list for files for each language
  user_files_dir <- list()
  user_files <- list()
  
  # set up each of the file directories and order consisten with the random overall trend
  for(i in 1:length(classes)){
    user_files[[i]] <- list.files(directory, pattern = classes[i])
    user_files[[i]] <- user_files[[i]][order(match(user_files[[i]], file_order))]
    user_files_dir[[i]] <- paste0(directory, "/", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files_dir)
}

# run the function with 10 languages, specifying the directory
user_files <- view_directories(classes,
                               directory)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# adjust the lambdas for each species for each language with random
adj_lambdas <- list()
all_lambdas <- list()
for(i in 1:length(language_views)){
  for(j in 1:length(random_trend)){
    data_file <- language_views[[i]][[j]]
    adj_lambdas[[j]] <- cbind(data_file[, 1:3], sweep(data_file[, 4:ncol(data_file)], 2, random_trend[[j]]$lamda, FUN = "-"))
  }
  all_lambdas[[i]] <- adj_lambdas
}

# smooth the adjusted random lambda for each species
# iterate through all the articles of that class/language
smooth_series <- function(X){
  
  # create index
  index <- cumprod(10^c(0, X))

  x_range <- 1:length(index)
  y.loess <- loess(index~x_range, span = 0.39)
  data_fin <- predict(y.loess, data.frame(x_range))
  return(data_fin)
}

# convert back to lambda
create_lambda <- function(X){
  lambda <- c(1, diff(log10(X)))
  return(lambda)
}

# convert back to index, run the smooth for random adjusted lambda, and then convert back the lamda
smooth_all_groups <- function(data_file){
  
  # smooth the series for each row (species)
  smoothed_indices <- apply(X = data_file[nrow(data_file),5:ncol(data_file)], 1, FUN = smooth_series)

  # convert the smoothed series back into lambda, and then transpose back to years as columns
  smoothed_lambda <- apply(smoothed_indices, 2, FUN = create_lambda) %>%
    t()

  # add back in the original column names
  colnames(smoothed_lambda) <- colnames(data_file)[4:ncol(data_file)]
  
  # bind the adjusted smoothed lambda back onto the first four columns
  smoothed_lambda <- cbind(data_file[,1:3], smoothed_lambda)
  
  return(smoothed_lambda)

}

smoothed_adjusted_lamda <- list()
for(i in 1:length(all_lambdas)){
  smoothed_adjusted_lamda[[i]] <- lapply(all_lambdas[[i]], smooth_all_groups)
  print(i)
}

# Function to calculate index from lambdas selected by 'ind'
create_lpi <- function(lambdas, ind = 1:nrow(lambdas)) {
  
  # remove na rows
  lambdas_new <- lambdas[complete.cases(lambdas), ]
  
  # select columns from lambda file to calculate mean, and build a cumprod trend
  lambda_data <- lambdas_new[, 5:ncol(lambdas_new)]
  this_lambdas <- lambda_data[ind, ]
  mean_ann_lambda <- colMeans(this_lambdas, na.rm = TRUE)
  trend <- cumprod(10^c(0, mean_ann_lambda))
  return(trend)
}

# function for boostrapping the create_lpi function for each lambda, and generating a 95 % confidence interval
run_each_group <- function(lambda_files, random_trend){
  
  # Bootstrap these to get confidence intervals
  dbi.boot <- boot(lambda_files, create_lpi, R = 1000)
  
  # Construct dataframe and get mean and 95% intervals
  boot_res <- data.frame(LPI = dbi.boot$t0)
  boot_res$Year <- random_trend$Year[1:(nrow(random_trend))]
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.975), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.025), na.rm = TRUE)
  return(boot_res)
}

# run the boostrapping of trends for each lambda, and adjust for the random of that language
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(smoothed_adjusted_lamda)){
  for(j in 1:length(smoothed_adjusted_lamda[[i]])){
    lpi_trends_adjusted[[j]] <- run_each_group(smoothed_adjusted_lamda[[i]][[j]], random_trend[[j]]) %>%
      mutate(language = random_trend[[j]]$language)
    
  }
  
  # bind together the trends for that language
  bound_trends[[i]] <- rbindlist(lpi_trends_adjusted) %>%
    mutate(taxa = classes[i])
}

# bind together the trend for all languages
fin_bound_trends <- rbindlist(bound_trends)

# plot all the class level trends
fin_bound_trends %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(taxa = factor(taxa, levels = c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia", "random"),
                       labels = c("Ray finned fishes", "Amphibians", "Birds", "Insects", "Mammals", "Reptiles", "Random"))) %>%
  mutate(language = factor(language, levels = c("\\^ar_", "\\^zh_", "\\^en_", "\\^fr_", "\\^de_", "\\^it_", "\\^ja_", "\\^pt_", "\\^ru_", "\\^es_"),
                           labels = c("Arabic", "Chinese", "English", "French", "German", "Italian", "Japanese", "Portuguese", "Russian", "Spanish"))) %>%
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = language), alpha = 0.4) +
  #geom_smooth(aes(x = Year, y = LPI, fill = language, colour = language), alpha = 0.4, method = "loess", span = 0.2, se = FALSE) +
  geom_line(aes(x = Year, y = LPI, colour = language)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_fill_manual("Language", values = c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) +
  scale_colour_manual("Language", values = c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) +
  facet_grid(~taxa, scales = "free_y") +
  ylab("SAI") +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("average_random_adjusted_class_SAI_free_1000_95_no-random-species.png", scale = 1.3, dpi = 350)

# convert series back to lambda, and then take sets varying the start date up by one
# figure for overall changes of different groupings
# first calculate average lambda for each series
language_frame <- list()
for(i in 1:56){
  language_frame[[i]] <- fin_bound_trends %>%
    group_by(language, taxa) %>%
    filter(row_number() %in% c(i:57)) %>%
    mutate(lambda = c(0, diff(log10(LPI)))) %>%
    mutate(conf_diff = 1 - (mean(LPI_upr-LPI_lwr))) %>% 
    mutate(average_lambda = mean(lambda)) %>% 
    ungroup() %>% 
    filter(taxa != "random") %>%
    select(language, taxa, conf_diff, average_lambda) %>%
    unique() %>%
    mutate(factor_rate = factor(ifelse(average_lambda > 0, "increasing", "decreasing"))) %>% 
    mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low"))) %>%
    mutate(series_start = i)
}

# count the number increasing and decreasing time series for each taxa/language combination
series_start_var <- rbindlist(language_frame) %>%
  group_by(taxa, language) %>%
  count(factor_rate) %>%
  ungroup() %>%
  arrange(taxa, language) %>%
  reshape2::dcast(taxa + language ~ factor_rate)

series_start_var[is.na(series_start_var)] <- 0
series_start_var$change_diff <- series_start_var$increasing - series_start_var$decreasing

# count number increasing and decreasing
series_start_var <- series_start_var %>%
  mutate(up_down = ifelse(change_diff > 0, "increasing", "decreasing")) 

# build plot for language and certainty 
rate_plot_series_var <- series_start_var %>%
  mutate(up_down = factor(up_down, levels = c("increasing", "decreasing"), labels = c("Increase", "Decrease"))) %>%
  mutate(taxa = factor(taxa, levels = c("insecta", "actinopterygii", "amphibia", "mammalia", "aves", "reptilia"),
                       labels = c("Insecta", "Actinopterygii", "Amphibia", "Mammalia", "Aves", "Reptilia"))) %>% 
  mutate(language = factor(language, levels = c("\\^ar_", "\\^fr_", "\\^zh_", "\\^en_", "\\^de_", "\\^es_", "\\^it_", "\\^ja_", "\\^pt_" , "\\^ru_"),
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  ggplot() +
  ggtitle("Total monthly view trends") +
  geom_tile(aes(x = language, y = taxa, fill = up_down), colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        legend.position = "none") 

# build dataframe for setting out the legend
d <- expand.grid(x=1:2,y=1:2)
d <- merge(d,data.frame(x=1:2,xlabel=c("X low", "X high")),by="x")
d <- merge(d,data.frame(y=1:2,ylabel=c("Y low", "Y high")),by="y")
d$alpha_val <- d$x

# build the legend with the appropriate colours for change and certainty
g.legend <- ggplot(d, aes(x,y,fill = ylabel)) +
  geom_tile(colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.4, yend = 0.6 ), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.6, yend = 2.4), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  theme_void() +
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10),
        axis.title.y = element_text(angle = 90),
        axis.title.x = element_text(hjust = 0.6, vjust = -0.8),
        axis.text.x = element_text()) +
  labs(title=NULL,x= "Certainty", y = "Change") +
  theme(axis.title=element_text(color="black"))

# combine the plot and the legend
ggdraw() +
  draw_plot(rate_plot, -0.13, width = 1, height = 1, scale = 0.75) +
  draw_plot(g.legend, width = 0.2, height = 0.3, scale = 0.95, hjust = -3.65, vjust = -1.96)

### rest to be reset to how it was before for whole series

# calculate number of factors for rates and confidence for each language
sort_rate_lang <- language_frame %>%
  group_by(language) %>%
  count(factor_rate) %>%
  ungroup() %>%
  filter(factor_rate == "decreasing") %>%
  select(language, n)

# add en at the bottom for engligh with no low confidence groupings
sort_conf_lang <- language_frame %>%
  group_by(language) %>%
  count(factor_conf) %>%
  ungroup()  %>%
  filter(factor_conf == "low") %>%
  select(language, n) %>%
  add_row(language = "\\^en_", n = 0)

# join together number of factors and sort on rate and confidence
joined_order_lang <- inner_join(sort_rate_lang, sort_conf_lang, by = "language") %>%
  arrange(desc(n.x), n.y)  %>% 
  mutate(language = factor(language, levels = language)) %>% pull(language)

# calculate number of factors for rates and confidence for each taxa
sort_rate_taxa <- language_frame %>%
  group_by(taxa) %>%
  count(factor_rate) %>%
  filter(factor_rate == "decreasing") %>%
  select(taxa, n) %>%
  arrange(desc(n)) %>%
  ungroup()

sort_conf_taxa <- language_frame %>%
  group_by(taxa) %>%
  count(factor_conf) %>%
  filter(factor_conf == "low") %>%
  select(taxa, n) %>%
  arrange(n) %>%
  ungroup()

# join together number of factors and sort on rate and confidence
joined_order_taxa <- inner_join(sort_rate_taxa, sort_conf_taxa, by = "taxa") %>%
  arrange(desc(n.x), n.y)  %>% 
  mutate(taxa = factor(taxa, levels = taxa)) %>% pull(taxa)

# build plot for language and certainty 
rate_plot <- fin_bound_trends %>%
  group_by(language, taxa) %>%
  mutate(lambda = c(0, diff(log10(LPI)))) %>% 
  filter(lambda != 0) %>%
  mutate(conf_diff = 1 - (mean(LPI_upr-LPI_lwr))) %>%
  mutate(mean_lambda = mean(lambda)) %>%
  mutate(certainty = abs(mean_lambda) * conf_diff) %>%
  ungroup() %>%
  select(mean_lambda, taxa, language, conf_diff) %>%
  unique() %>%
  filter(taxa != "random") %>% 
  mutate(factor_rate = factor(ifelse(mean_lambda > 0, "increasing", "decreasing"))) %>% 
  mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low"))) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing", "decreasing"), labels = c("Increase", "Decrease"))) %>%
  mutate(factor_conf = factor(factor_conf, levels = c("low", "high"), labels = c("Low",  "High"))) %>%
  mutate(alpha_val = as.numeric(factor_conf)) %>%
  mutate(taxa = factor(taxa, levels = joined_order_taxa,
                       labels = c("Insecta", "Actinopterygii", "Amphibia", "Mammalia", "Aves", "Reptilia"))) %>% 
  mutate(language = factor(language, levels = joined_order_lang,
                           labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(plot_order = (as.numeric(factor_rate) + as.numeric(factor_conf))) %>%
  ggplot() +
  ggtitle("Total monthly view trends") +
  geom_tile(aes(x = language, y = taxa, fill = factor_rate, alpha = alpha_val), colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        legend.position = "none") 

# build dataframe for setting out the legend
d <- expand.grid(x=1:2,y=1:2)
d <- merge(d,data.frame(x=1:2,xlabel=c("X low", "X high")),by="x")
d <- merge(d,data.frame(y=1:2,ylabel=c("Y low", "Y high")),by="y")
d$alpha_val <- d$x

# build the legend with the appropriate colours for change and certainty
g.legend <- ggplot(d, aes(x,y,fill=ylabel, alpha = alpha_val))+
  geom_tile(colour = "white", size = 1.5) +
  scale_fill_manual("Rate of change", values = c("#009E73", "#D55E00")) +
  scale_alpha(range = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Low", "High")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.4, yend = 0.6 ), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_segment(aes(x=0.3, xend = 0.3, y = 1.6, yend = 2.4), arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  theme_void() +
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10),
        axis.title.y = element_text(angle = 90),
        axis.title.x = element_text(hjust = 0.6, vjust = -0.8),
        axis.text.x = element_text()) +
  labs(title=NULL,x= "Certainty", y = "Change") +
  theme(axis.title=element_text(color="black"))

# combine the plot and the legend
ggdraw() +
  draw_plot(rate_plot, -0.13, width = 1, height = 1, scale = 0.75) +
  draw_plot(g.legend, width = 0.2, height = 0.3, scale = 0.95, hjust = -3.65, vjust = -1.96)

# save the plot with legend and plot combined for change and certainty
ggsave("average_change_uncertainty_comb_2.png", scale = 1.1, dpi = 400)