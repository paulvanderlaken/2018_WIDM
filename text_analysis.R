### 2018/06/01
### WIE IS DE MOL
### TWITTER ANALYSIS
### PART 3: TEXT ANALYSIS
### PAULVANDERLAKEN.COM

### SETUP
pkg <- c("here", "tidyverse", "tidytext", "tm", "patchwork")
sapply(pkg, function(x){
  if(!x %in% installed.packages()) install.packages(x)
  library(x, character.only = TRUE)
  return(x)
})

source(here("standard_file.R"))

censor = " "

tweets %>%
  group_by(screen_name) %>%
  mutate(user_tweets = n()) %>%
  ungroup() %>%
  mutate(text_clean = str_replace_all(text, "https://t.co/[:alnum:]+|http://[:alnum:]+", censor) %>% # links
           str_replace_all("(:)([:punct:]|[A-z])+", censor) %>% # smileys
           str_replace_all("(&amp;|#)", censor) %>% # hashtags
           str_replace_all("(&commat;|@)", censor) %>% # mentions
           str_replace_all("[^[:ascii:]]", censor) %>% # unicode stuff
           str_replace_all("[^[:alnum:]-\\s]", censor) %>% # punctuation except -
           str_replace_all("\\s\\s", censor)
         ) ->
  tweets

tweets %>%
  unnest_tokens(word, text_clean, drop = FALSE, token = "regex", pattern = "\\s") -> 
  tokens

top_size = 30 

tokens %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  anti_join(y = tibble(word = stopwords("nl")), by = "word") %>%
  filter(nchar(word) > 2) %>%
  top_n(top_size) %>%
  filter(!word %in% c("widm", "widm2018", "wieisdemol")) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  annotation_custom(rast) +
  geom_bar(aes(alpha = n, fill = factor(word %in% kan)), stat = "identity") +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = c("grey", "green")) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  theme(legend.position = "none") + 
  labs(y = NULL, x = NULL) -> 
  plot_words 

plot_words +
  labs(title = paste("Top", top_size, "meest gebruikte woorden"),
       subtitle = subtitle_data_date,
       y = "Aantal tweets", x = NULL,
       caption = "paulvanderlaken.com")
save_ggplot("woorden_gebruikt", width = 3.5, height = 7)

tokens %>%
  mutate(split = ifelse(created_at < afl$afl1$start, 1, 2)) %>%
  group_by(split, word) %>%
  count() %>%
  group_by(split) %>%
  arrange(desc(n)) %>%
  anti_join(y = tibble(word = stopwords("nl")), by = "word") %>%
  filter(!word %in% c("widm", "widm2018", "wieisdemol")) %>%
  filter(nchar(word) > 2) %>%
  top_n(top_size, n) %>%
  ungroup() -> 
  tokens_split

tokens_split %>%
  mutate(split = factor(split, 
                        labels = c("Voorafgaand aan aflevering 1",
                                   "Tijdens/na aflevering 1"),
                        ordered = TRUE)) %>%
  ggplot(aes(x = reorder_within(word, n, split), y = n)) +
  annotation_custom(rast) +
  geom_bar(aes(alpha = n, fill = factor(word %in% kan)), stat = "identity") +
  coord_flip() +
  theme_light() +
  scale_x_reordered() +
  scale_fill_manual(values = c("grey", "green")) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  facet_wrap(~ split, scales = "free_y") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size = 11)) +
  labs(x = NULL, y = NULL) ->
  plot_words_split
plot_words_split +
  labs(title = paste("Top", top_size, "meest gebruikte woorden"),
       subtitle = subtitle_data_date,
       y = "Aantal tweets", x = NULL,
       caption = "paulvanderlaken.com")
save_ggplot("woorden_gebruikt_split_afl1", width = 6, height = 7)










