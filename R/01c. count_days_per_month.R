library(data.table)
library(dplyr)
library(ggplot2)

# set up vector for languages, classes, and directory
languages <- c("^es_", "^fr_", "^de_", "^ja_", "^it_", "^ar_", "^ru_", "^pt_", "^zh_", "^en_")
directory <- "J:/submission_2/user_trends/"
classes <- c("actinopterygii", "amphibia", "aves", "insecta", "mammalia", "reptilia")

# read in the view data for all taxonomic classes
# loop through each directory and create a list of all files for users
view_directories <- function(languages, directory, view_files){
  
  # bring in all the files in that directory
  view_files <- list.files(directory)
  
  # set up empty list for files for each language
  user_files <- list()
  
  # set up each of the file directories
  for(i in 1:length(languages)){
    user_files[[i]] <- list.files(directory, pattern = languages[i])
    user_files[[i]] <- paste0(directory, "", user_files[[i]])
  }
  
  # return list of full file paths for each language
  return(user_files)
}

# run the function with 10 languages, specifying the directory
user_files <- view_directories(languages,
                               directory)

# read in all the files in groups for each language
language_views <- list()
system.time(for(i in 1:length(user_files)){
  language_views[[i]] <- lapply(user_files[[i]], fread, encoding = "UTF-8", stringsAsFactors = FALSE)
})

# remove extra error columns from chinese dataframe - extra dataframe to avoid overwrite
language_views_edit <- language_views
language_views_edit[[9]][[1]] <- language_views_edit[[9]][[1]] %>%
  dplyr::select(-title, -V2)

# count the number of days per month
subset_amphib$year <- substr(subset_amphib$timestamp, start = 1, stop = 4)
subset_amphib$month <- substr(subset_amphib$timestamp, start = 5, stop = 6)
subset_amphib$day <- substr(subset_amphib$timestamp, start = 7, stop = 8)

# count the number of days per month, and then calculate a rate of change for those days per month
days <- subset_amphib %>%
  group_by(q_wikidata, article, year) %>%
  count(month) %>%
  ungroup() %>%
  group_by(article) %>%
  mutate(rate = c(0, diff(log10(n)))) %>%
  ungroup() %>%
  mutate(date_col = paste(year, month, "01", sep = "/")) %>% 
  mutate(date_col = as.Date(date_col, format = "%Y/%m/%d")) %>%
  mutate(dec_date = 1970 + (year - 2015)*12 + month)



day_count <- subset_amphib %>%
  group_by(article, year) %>%
  count(month) %>%
  ungroup() %>%
  mutate(date_col = paste(year, month, "01", sep = "/")) %>% 
  mutate(date_col = as.Date(date_col, format = "%Y/%m/%d")) %>%
  group_by(date_col) %>% 
  mutate(mean_days = mean(n)) %>%
  mutate(lower_interval = mean_days - (1.96 * mean_days/sqrt(length(mean_days)))) %>%
  mutate(upper_interval = mean_days + (1.96 * mean_days/sqrt(length(mean_days)))) %>%
  ungroup() %>%
  select(-article, -year, -month, -n) %>%
  unique() %>%
  mutate(rate = c(0, diff(log10(mean_days)))) %>%
  mutate(index = cumprod(10^c(rate)))

subset_amphib %>%
  group_by(year, month) %>%
  tally(views) %>% 
  ungroup() %>% 
  mutate(date_col = paste(year, month, "01", sep = "/")) %>% 
  mutate(date_col = as.Date(date_col, format = "%Y/%m/%d")) %>%
  mutate(rate = c(0, diff(log10(n)))) %>%
  mutate(adjusted_rate = (rate - day_count$rate)) %>%
  mutate(adjusted_index = cumprod(10^c(adjusted_rate))) %>%
  mutate(index = cumprod(10^c(rate))) %>%
  ggplot() +
    geom_line(aes(x = date_col, y = index)) +
    geom_line(aes(x = date_col, y = adjusted_index), colour = "red")

subset_amphib %>%
  group_by(year, month) %>%
  summarise(n = mean(views)) %>% 
  ungroup() %>% 
  mutate(date_col = paste(year, month, "01", sep = "/")) %>% 
  mutate(date_col = as.Date(date_col, format = "%Y/%m/%d")) %>%
  ggplot() +
  geom_line(aes(x = date_col, y = n))

subset_amphib %>%
  group_by(year, month) %>%
  tally(views) %>% 
  ungroup() %>% 
  mutate(date_col = paste(year, month, "01", sep = "/")) %>% 
  mutate(date_col = as.Date(date_col, format = "%Y/%m/%d")) %>%
  mutate(rate = c(0, diff(log10(n)))) %>%
  mutate(adjusted_rate = (rate - day_count$rate)) %>%
  mutate(index = cumprod(10^c(adjusted_rate))) %>%
  ggplot() +
  geom_line(aes(x = date_col, y = n))
  


# do the pages that you pick up vary between accesses of the API?
# what's the variation about the mean number of days? 
# rate of change for number of days in each month? 

# * all of above points solved by adjusting for rate of chage of number of days


