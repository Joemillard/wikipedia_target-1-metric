# read in required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)
library(boot)
library(RColorBrewer)
library(cowplot)

# source the functions R script
source("R/00. functions.R")

# script for pollinator models using new language data
# read in the random rds file
directory <- here::here("data/class_wiki_indices/submission_2/lambda_files/annual_lambda")

# read in the string of languages - original order sorted alphabetically for files read in
languages <- c("\\^es_", "\\^fr_", "\\^de_", "\\^ja_", "\\^it_", "\\^ar_", "\\^ru_", "\\^pt_", "\\^zh_", "\\^en_")

# read in the lambda files 
random_trend <- readRDS("annual_overall_10-random-languages.rds")

# adjust each of the lambda values for random
# adjust the year column
for(i in 1:length(random_trend)){
  random_trend[[i]]$date <- as.numeric(rownames(random_trend[[i]]))
  random_trend[[i]]$Year <- (random_trend[[i]]$date - 1970) + 2016
  random_trend[[i]]$Year <- as.character(random_trend[[i]]$Year)
  
  # calculate lambda for random
  random_trend[[i]] <- random_trend[[i]] %>%
    filter(date %in% c(1970:1973))
  random_trend[[i]]$lamda = c(0, diff(log10(random_trend[[i]]$LPI_final[1:4])))
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
  dbi.boot <- boot(lambda_files, create_lpi, R = 100)
  
  # Construct dataframe and get mean and 95% intervals
  boot_res <- data.frame(LPI = dbi.boot$t0)
  boot_res$Year <- random_trend$Year[1:(nrow(random_trend))]
  boot_res$LPI_upr <- apply(dbi.boot$t, 2, quantile, probs = c(0.95), na.rm = TRUE) 
  boot_res$LPI_lwr <- apply(dbi.boot$t, 2, quantile, probs = c(0.05), na.rm = TRUE)
  return(boot_res)
}

# run the boostrapping of trends for each lambda, and adjust for the random of that language
lpi_trends_adjusted <- list()
bound_trends <- list()
for(i in 1:length(all_lambdas)){
  for(j in 1:length(all_lambdas[[i]])){
    lpi_trends_adjusted[[j]] <- run_each_group(all_lambdas[[i]][[j]], random_trend[[j]]) %>%
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
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = LPI_lwr, ymax = LPI_upr, fill = language), alpha = 0.3) +
  geom_line(aes(x = Year, y = LPI, colour = language)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~taxa, scales = "free_y") +
  ylab("SAI") +
  xlab(NULL) +
  theme_bw()

ggsave("random_annual_adjusted_class_SAI_free.png", scale = 1.3, dpi = 350)


# figure for overall changes of different groupings
# first calculate average lambda for each series
language_frame <- fin_bound_trends %>%
  group_by(language, taxa) %>%
  mutate(lambda = c(0, diff(log10(LPI)))) %>%
  mutate(conf_diff = 1 - (mean(LPI_upr-LPI_lwr))) %>% 
  mutate(average_lambda = mean(lambda)) %>% 
  ungroup() %>% 
  filter(taxa != "random") %>%
  select(language, taxa, conf_diff, average_lambda) %>%
  unique() %>%
  mutate(factor_rate = factor(ifelse(average_lambda > 0, "increasing", "decreasing"))) %>% 
  mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low")))

# calculate number of factors for rates and confidence for each language
sort_rate_lang <- language_frame %>%
  group_by(language) %>%
  count(factor_rate) %>%
  ungroup() %>%
  filter(factor_rate == "increasing") %>%
  select(language, n)

# add en at the bottom for engligh with no low confidence groupings
sort_conf_lang <- language_frame %>%
  group_by(language) %>%
  count(factor_conf) %>%
  ungroup()  %>%
  filter(factor_conf == "low") %>%
  select(language, n)

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
  ungroup() %>%
  select(mean_lambda, taxa, language, conf_diff) %>%
  unique() %>%
  filter(taxa != "random") %>% 
  mutate(factor_rate = factor(ifelse(mean_lambda > 0, "increasing", "decreasing"))) %>% 
  mutate(factor_conf = factor(ifelse(conf_diff > quantile(conf_diff, 0.5), "high", "low"))) %>%
  mutate(factor_rate = factor(factor_rate, levels = c("increasing", "decreasing"), labels = c("Increase", "Decrease"))) %>%
  mutate(factor_conf = factor(factor_conf, levels = c("low", "high"), labels = c("Low",  "High"))) %>%
  mutate(alpha_val = as.numeric(factor_conf)) %>%
  #mutate(taxa = factor(taxa, levels = joined_order_taxa, 
  #                     labels = c("Insecta", "Actinopterygii", "Amphibia", "Mammalia", "Aves", "Reptilia"))) %>% 
  #mutate(language = factor(language, levels = joined_order_lang,
  #                         labels = c("Arabic", "French", "Chinese", "English", "German", "Spanish", "Italian", "Japanese", "Portuguese", "Russian"))) %>%
  mutate(plot_order = (as.numeric(factor_rate) + as.numeric(factor_conf))) %>%
  ggplot() +
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
ggsave("average_change_annual_uncertainty.png", scale = 1.1, dpi = 400)
