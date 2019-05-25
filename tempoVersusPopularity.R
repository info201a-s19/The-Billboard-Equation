library("dplyr")
library("ggplot2")
library("lintr")

# Source summary information
source("summaryInformation.R")

# Function to categoricalize tempo
cat_tempo <- function(dataset) {
  tempo_category <- cut(dataset$tempo,
                        breaks = seq(0, 250, by = 10),
                        labels = seq(0, 240, by = 10),
                        right = FALSE)
  return(tempo_category)
}

# Apply function to data
filtered_info$tempo_category <- cat_tempo(filtered_info)

# Function to average popularity by tempo groups
get_tempo_info <- function(dataset) {
  grouped_tempo <- dataset %>%
    group_by(tempo_category) %>%
    summarize(avg_popularity = mean(popularity))
  return(grouped_tempo)
}

# Apply function to data
group_tempo <- get_tempo_info(filtered_info)

# Construct a plot to present the summarized data
ggplot(group_tempo, aes(x = tempo_category, y = avg_popularity, group = 1)) +
  geom_line() +
  geom_point()