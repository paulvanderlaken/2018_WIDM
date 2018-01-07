### 2018/06/01
### WIE IS DE MOL
### TWITTER ANALYSIS
### PART 3: TEXT ANALYSIS
### PAULVANDERLAKEN.COM

### SETUP ####
pkg <- c("tidyquant", "here", "tidyverse", "scales")
sapply(pkg, function(x){
  if(!x %in% installed.packages()) install.packages(x)
  library(x, character.only = TRUE)
  return(x)
})

source(here("standard_file.R"))

### KANALEN ####
top_size = 20

sources <- tweets %>%
  group_by(source) %>%
  count() %>%
  ungroup() %>%
  top_n(top_size, n) %>%
  arrange(desc(n)) 

sources %>%
  ggplot(aes(x = reorder(source, n), y = n)) +
  annotation_custom(rast) +
  geom_bar(aes(fill = !grepl("tw", tolower(source))), stat = "identity") +
  coord_flip() +
  theme_light() +
  scale_fill_manual(labels = c("Twitter", "Extern"), 
                    values = c("green", "blue"),
                    name = NULL) +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.background = element_blank()) +
  labs(title = paste("Top", top_size, "meest gebruikte kanalen"),
       subtitle = subtitle_data_date,
       y = "Aantal tweets", x = NULL,
       caption = "paulvanderlaken.com")
save_ggplot("kanalen", width = 4.5, height = 5)



### MOLLOTEN ####

top_size = 30

users <- tweets %>%
  group_by(screen_name) %>%
  count() %>%
  ungroup() %>%
  top_n(top_size, n) %>%
  arrange(desc(n))

users %>%
  mutate(screen_name = paste0("@", screen_name)) %>%
  ggplot(aes(x = reorder(screen_name, n), y = n)) +
  annotation_custom(rast) +
  geom_bar(aes(alpha = n), stat = "identity", fill = "green") +
  coord_flip() +
  theme_light() +
  scale_alpha_continuous(range = c(0.4, 1)) +
  theme(legend.position = "none") +
  labs(title = paste("Top", top_size, "grootste molloten"),
       subtitle = subtitle_data_date,
       y = "Aantal tweets", x = NULL,
       caption = "paulvanderlaken.com")
save_ggplot("molloten", width = 4, height = 5)
 


### VERMELDINGEN ####

top_size = 30

temp <- tweets %>% filter(!is.na(mentions_screen_name))

map(temp$mentions_screen_name, function(x) {
  str_split(x, pattern = ", ") %>% 
    unlist()
}) %>%
  set_names(temp$created_at) %>% 
  map_df(~ data_frame(mentioned = .x), .id = "time") %>%
  filter(mentioned != "NA") ->
  mentions

mentions %>%
  mutate(mentioned = paste0("@", mentioned)) %>%
  group_by(mentioned) %>%
  count() %>%
  ungroup() %>%
  filter(mentioned != "@WieIsDeMol") %>%
  top_n(top_size) %>%
  ggplot(aes(x = reorder(mentioned, n), y = n)) +
  annotation_custom(rast) +
  geom_bar(aes(alpha = n), stat = "identity", fill = "green") +
  coord_flip() +
  theme_light() +
  scale_alpha_continuous(range = c(0.4, 1)) +
  theme(legend.position = "none") +
  labs(title = paste("Top", top_size,"genoemde gebruikers"), 
       subtitle = subtitle_data_date,
       y = "Aantal tweets", x = NULL,
       caption = "paulvanderlaken.com")
save_ggplot("user_mentions", width = 4, height = 5)


### TIJDSERIE ####

tweets %>%
  group_by(created_at) %>%
  count() %>% 
  ungroup() %>%
  mutate(cumulative = cumsum(n)) %>%
  ggplot(aes(x = created_at, y = cumulative)) +
  geom_dotplot(aes(y = n), binwidth = 360, dotsize = 1) +
  # geom_line(col = "grey", size = 1) +
  geom_line(size = 1, col = 'blue') +
  scale_x_datetime(date_breaks = "12 hours", 
                   labels = date_format("%d-%m\n%H:%M"),
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct("2017-12-26 01:00:00 GTM+1"),
                     max(tweets$created_at) + 3600 * 12
                   )) +
  theme_light() +
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Datum & Tijd", y = "Aantal tweets",
       title = "#WIDM op Twitter de avond van 3 januari al begonnen",
       subtitle = "Het aantal tweets (zwart) en het cumulatief (blauw)",
       caption = "paulvanderlaken.com")
save_ggplot("tweet_count_overtime", width = 8, height = 4)

### TIJDSERIE MINUUT ####
min = 1
timeframe = ifelse(min == 1, "minuut", paste(min, "minuten"))
tweets %>%
  mutate(created_at = as.POSIXct(cut(created_at, breaks = paste(min ,"min")))) %>%
  group_by(created_at) %>%
  count() %>%
  ungroup() %>%
  mutate(cumulative = cumsum(n),
         cumulative_perc = (cumulative/sum(n))*max(n)) -> 
  tweets_min

tweets_min %>%
  ggplot(aes(x = created_at, y = n)) +
  annotation_custom(rast) +
  geom_bar(stat = "identity", position = "identity", width = 1000, col = "black") +
  geom_line(aes(y = cumulative_perc), col = 'green', size = 0.5) +
  # geom_point(size = 0.1) +
  scale_y_continuous(sec.axis = sec_axis(~./max(tweets_min$n), 
                                         labels = scales::percent, 
                                         name = "Cumulatief")) +
  scale_x_datetime(date_breaks = "12 hours",
                   labels = date_format("%d-%m\n%H:%M"),
                   expand = c(0, 0),
                   limits = c(
                     as.POSIXct("2017-12-26 01:00:00 GTM+1"),
                     max(tweets$created_at) + 3600 * 12
                   )) +
  theme_light() +
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "Datum & Tijd", y = paste("Aantal tweets per", timeframe),
       title = paste("Eerste aflevering #WIDM explodeert op Twitter:",
                     "tot", max(tweets_min$n), "tweets per", timeframe),
       subtitle = paste(paste(nrow(tweets),"tweets met #WIDM tot", 
                              format.POSIXct(max(tweets$created_at), "%d %B %H:%M")),
                        paste("Het aantal tweets per", timeframe, 
                              "(zwart) en het cumulatief (groen)"),
                        sep = "\n"),
       caption = "paulvanderlaken.com")
save_ggplot("tweet_count_overtime", width = 10, height = 5)
