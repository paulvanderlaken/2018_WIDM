### 2018/05/01
### WIE IS DE MOL
### TWITTER ANALYSIS
### PART 1: SCRAPER
### PAULVANDERLAKEN.COM

### SETUP
pkg <- c("here", "rtweet", "tidyverse")
sapply(pkg, function(x){
  if(!x %in% installed.packages()) install.packages(x)
  library(x, character.only = TRUE)
  return(x)
})

options(scipen = 999)

dir.create("data")
dir.create("images")

file_name = "WIDM.csv"


### DOWNLOAD NEW TWEETS
raw <- search_tweets("widm OR wieisdemol", n = 17000, include_rts = FALSE, retryonratelimit = TRUE)

new_tweets <- raw %>% 
  select(created_at, screen_name, text, source, reply_to_screen_name:hashtags, 
         media_type, mentions_screen_name, place_name, bbox_coords) %>%
  mutate(text = gsub("\n|;", " ", text)) %>% # delete newlines & semicolons
  mutate(hashtags = map_chr(hashtags, function(x) paste(x, collapse = ", "))) %>%
  mutate(media_type = map_chr(media_type, function(x) paste(x, collapse = ", "))) %>%
  mutate(mentions_screen_name = map_chr(mentions_screen_name, function(x) paste(x, collapse = ", "))) %>%
  mutate(bbox_coords = map_chr(bbox_coords, function(x) paste(x, collapse = ", ")))


### MERGE WITH PRIOR DOWNLOADS
if(!file_name %in% list.files(paste(here(), "data", sep = "/"))) { 

  tweets <- new_tweets
  write_excel_csv(tweets, path = paste(here(), "data", file_name, sep = "/"))
  
} else {
  
  old_tweets <- read_csv(file = paste(here(), "data", file_name, sep = "/"))
  write_excel_csv(old_tweets, path = paste(here(), "data", "backup.csv", sep = "/"))
  tweets <- bind_rows(old_tweets, new_tweets)
  tweets <- tweets[!duplicated(tweets[ , c("created_at", "screen_name", "text")]), ]
  tweets <- tweets %>% arrange(desc(created_at))
  write_excel_csv(tweets, path = paste(here(), "data", file_name, sep = "/"))
  
}
