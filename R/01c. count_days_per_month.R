library(data.table)
library(dplyr)
library(ggplot2)

# read in the new french 
fr_amphib <- fread("Z:/submission_2/user_trends/fr_amphibia__user_trends.csv")

summary(fr_amphib)
subset_amphib <- fr_amphib

# count the number of days per month
subset_amphib$year <- substr(subset_amphib$timestamp, start = 1, stop = 4)
subset_amphib$month <- substr(subset_amphib$timestamp, start = 5, stop = 6)
subset_amphib$day <- substr(subset_amphib$timestamp, start = 7, stop = 8)

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


