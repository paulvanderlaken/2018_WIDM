### 2018/05/01
### WIE IS DE MOL
### TWITTER ANALYSIS
### PART 2: MENTION NETWORK
### PAULVANDERLAKEN.COM

### SETUP
pkg <- c("here", "tidyverse", "igraph", "ggraph", "extrafont", "png", "grid")
sapply(pkg, function(x){
  if(!x %in% installed.packages()) install.packages(x)
  library(x, character.only = TRUE)
  return(x)
})

options(scipen = 999)

file_name = "WIDM.csv"

tweets <- read_csv(here(paste("data", file_name, sep = "/")))

subtitle_data_date <- paste("#WIDM twitter data tot",
                            format.POSIXct(Sys.time(), "%d %B %H:%M"))

### MENTION NETWORK
map(tweets$mentions_screen_name, function(x) {
  str_split(x, pattern = ", ") %>% 
    unlist()
}) %>%
  set_names(tweets$screen_name) %>% 
  map_df(~ data_frame(mentioned = .x), .id = "user") %>%
  filter(mentioned != "NA") %>% 
  arrange(mentioned) ->
  mention_df



mention_graph <- graph_from_edgelist(as.matrix(mention_df), directed = TRUE) 
V(mention_graph)$degree <- degree(mention_graph)
V(mention_graph)$betweenness <- betweenness(mention_graph, v = V(mention_graph))
V(mention_graph)$closeness <- closeness(mention_graph, v = V(mention_graph))
V(mention_graph)$name <- names(V(mention_graph))
V(mention_graph)$size <- log(V(mention_graph)$degree + 1)


# unname small nodes
rm_threshold <- 30
V(mention_graph)$name_label <- unname(ifelse(degree(mention_graph) > rm_threshold, names(V(mention_graph)), "")) 
V(mention_graph)$size <- unname(ifelse(degree(mention_graph) > rm_threshold, degree(mention_graph), 0))



ggraph(mention_graph, layout = "linear", circular = TRUE) +
  annotation_custom(rast) +
  geom_edge_arc(edge_width = 0.125, alpha = 0.1) +
  geom_node_label(aes(label = name_label, size = size), 
                  label.size = NA, segment.size = 0.5, repel = TRUE,
                  fill = "#ffffff99", segment.colour = "lightgreen", 
                  colour = "green", fontface = "bold") +
  scale_size(trans = "sqrt", range = c(2, 7)) + 
  coord_fixed() +
  theme_graph() +
  guides(size = FALSE) +
  labs(title = "Netwerk van user-verwijzingen in tweets",
       subtitle = paste(subtitle_data_date, paste0(
         "Namen > ", rm_threshold, " geprint"), 
         "Grootte naar aantal verwijzingen",
         sep = "; "),
       caption = "paulvanderlaken.com")
ggsave(filename = here(paste0("images/netwerk_",Sys.Date(),".png")), width = 7.5, height = 8.5, dpi = 300)





