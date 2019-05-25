library("ggplot2")
library("lintr")
library("knitr")

# Source summary information
source("summaryInformation.R")

# Group songs by artists
get_grouped_artists <- function(dataset) {
  grouped_artists <- dataset %>%
    filter(popularity >= 97) %>%
    group_by(artist_name) %>%
    summarize(songs = n())
  return(grouped_artists)
}

# Apply function to data
get_grouped_artists(filtered_info)

# Get Pie chart
pie_chart <- ggplot(grouped_artist, aes(x = "", y = songs, fill = artist_name)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = "Dark2") +
  blank_theme +
  theme(axis.text.x = element_blank())