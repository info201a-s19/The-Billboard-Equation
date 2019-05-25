library("dplyr")
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