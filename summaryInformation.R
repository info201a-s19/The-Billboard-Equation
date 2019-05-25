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
}

# A function that calculates summary information about the key-mode combinations and frequency
# Create new variable -- Key and Mode
get_grouped_info <- function(dataset) {
  # Group info into key and mode
  grouped_info <- dataset %>%
    mutate(key_and_mode = key * 10 + mode) %>%
    group_by(key_and_mode) %>%
    summarize(freq = n()) %>%
    arrange(desc(freq))
  # Categoricalize key and mode
  Key_Mode_Categories <- cut(grouped_info$key_and_mode,
                             breaks = c(0, 1, 10, 11, 20, 21, 30, 31,
                                        40, 41, 50, 51, 60, 61, 70, 71,
                                        80, 81, 90, 91, 100, 101, 110,
                                        111, 120),
                             labels = c("C Minor", "C Major",
                                        "D Minor", "D Major",
                                        "E Minor", "E Major",
                                        "F Minor", "F Major",
                                        "G Minor", "G Major",
                                        "A Minor", "A Major",
                                        "B Minor", "B Major",
                                        "C1 Minor", "C1 Major",
                                        "D1 Minor", "D1 Major",
                                        "E1 Minor", "E1 Major",
                                        "F1 Minor", "F1 Major",
                                        "G1 Minor", "G1 Major"), right = FALSE)
  # Final Summary for Key and Mode vs Frequency
  Key_Mode_Freq <- data.frame("Key and Mode" = Key_Mode_Categories,
                            "Number of Songs" = grouped_info$freq)
  # Return desired data
  return(Key_Mode_Freq)
}
