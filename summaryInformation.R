library("dplyr")
library("lintr")

information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")

# Actual filtered info in case function doesn't work
filtered_info <- information %>%
  select(artist_name, track_name, energy, tempo, valence,
         popularity, speechiness, key, mode) %>%
  filter(popularity != 0, speechiness < 0.66, key != -1) %>%
  distinct(artist_name, track_name, .keep_all = TRUE)
filtered_info

# Function for filtering the info
filter_info <- function(dataframe){
  filtered_info <- dataframe %>%
    select(artist_name, track_name, energy, tempo, valence,
           popularity, speechiness, key, mode) %>%
    filter(popularity != 0, speechiness < 0.66, key != -1) %>%
    distinct(artist_name, track_name, .keep_all = TRUE)
  filtered_info
}
  
# A function that summarizes information of the dataset 
get_summary_info <- function(dataset) {
  summary <- list()
  summary$number_of_songs <- nrow(dataset)
  summary$number_of_unique_artists <- length(unique(dataset$artist_name))
  summary$mean_popularity <- mean(dataset$popularity)
  summary$lower_quartile <- quantile(dataset$popularity, 0.25)
  summary$upper_quartile <- quantile(dataset$popularity, 0.75)
  return (summary)
<<<<<<< HEAD
}

# A function that calculates summary information about the key-mode combinations and frequency
# Create new variable -- Key and Mode

=======
}
>>>>>>> f7ceeecc81bed0c8a1c9ab02d50ec166cac3f826
