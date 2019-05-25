library("lintr")
library("dplyr")
library("tidyr")
library("ggplot2")

# load data of Spotify Audio features in April 2019
my_df <- read.csv("SpotifyAudioFeaturesApril2019.csv", stringsAsFactors = FALSE)

#extract 50 most popular artists information for making plot
avg_popularity_of_artists <- my_df %>%
  group_by(artist_name) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE)) %>%
  arrange(-avg_popularity) %>%
  head(50)

# plot
artist_plot <- ggplot(data = avg_popularity_of_artists) +
  geom_point(mapping = aes(x = artist_name,
                           y = avg_popularity,
                           color = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 1,
    size = 5, hjust = 1)) +
  ggtitle("The Top 50 Popular Artists")
artist_plot
