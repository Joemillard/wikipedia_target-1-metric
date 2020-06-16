library(dplyr)
library(pageviews)

titles <- read.csv("data/class_wiki_indices/submission_2/all_iucn_titles.csv")

titles <- titles %>%
  filter(class_name %in% c("MAMMALIA", "REPTILIA", "AVES", "INSECTA", "AMPHIBIA", "ACTINOPTERYGII")) %>%
  filter(site %in% c("enwiki")) %>% 
  droplevels()

table(titles$class_name)

# experimenting with pageviews
str(article_pageviews(project = "de.wikipedia", article = "R_(Programmiersprache)"
                      , start = as.Date('2015-11-01'), end = as.Date("2015-11-02")
                      , user_type = c("user", "bot"), platform = c("desktop", "mobile-web")))

str(article_pageviews(project = "en.wikipedia", article = "R_(programming_language)"
                      , start = as.Date('2015-11-01'), end = as.Date("2015-11-02")
                      , user_type = c("user", "bot"), platform = c("desktop", "mobile-web")))
