
### OPTIONS ####

options(scipen = 999)

### TWITTER DATA ####

file_name = "WIDM.csv"

tweets <- readr::read_csv(here::here(paste("data", file_name, sep = "/")))
# add hour to get CET
tweets$created_at <- as.POSIXct(format.POSIXct(tweets$created_at, 
                                               tz = Sys.timezone()), 
                                tz = Sys.timezone())

subtitle_data_date <- paste("#WIDM tweets tot",
                            format.POSIXct(max(tweets$created_at), "%d %B %H:%M"))


### KANDIDATEN ####

kan <- str_split(c("bella jan loes ron simone emilio jean-marc olcay ruben stine"), " ") %>% 
  unlist() %>%
  sort()


### AFLEVERINGEN ####

afl <- list(
  afl1 = list(
    start = as.POSIXct("2018-01-06 20:30:00", tz = Sys.timezone())
  )
)

### STANDAARDPLOT ####

img <- png::readPNG(paste(here::here(), "images", "widm_transparent.png", sep = "/"))
rast <- grid::rasterGrob(img, interpolate = TRUE)

save_ggplot <- function(filename, width = 6, height = 6){
  ggplot2::ggsave(here::here(paste0("images/", filename, "_", Sys.Date(), ".png")), 
                  dpi = 300, width = width, height = height)
}


### ORDER WITHIN BARPLOT ####

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}