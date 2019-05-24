library("dplyr")
library("lintr")

# Load csv file
information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")

## A function that calculates summary information to be included in your report
# Filter undesired information such as podcasts
filtered_info <- information %>%
  select(artist_name, track_name, energy, tempo, valence,
         popularity, speechiness, key, mode) %>%
  filter(popularity != 0, speechiness < 0.66, key != -1) %>%
  distinct(artist_name, track_name, .keep_all = TRUE)

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

# Apply function to data
summary_information <- get_summary_info(filtered_info)

# A function that calculates summary information about the key-mode combinations and frequency
# Create new variable -- Key and Mode
key_and_mode_info <- filtered_info %>%
  mutate(key_and_mode = paste0(key, mode))

# Group the data by Key and Mode and arranging the data by frequency
grouped_info <- key_and_mode_info %>%
  group_by(paste0(key, mode)) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq))

grouped_info$`paste0(key, mode)` <- c("C Major", "D Major",
                                      "C1 Major", "E Major",
                                      "E1 Major", "D1 Major",
                                      "G1 Minor", "A Major",
                                      "E1 Minor", "A Minor",
                                      "G Minor", "F1 Minor",
                                      "B Minor", "D Minor",
                                      "B Major", "F1 Major",
                                      "G1 Major", "G Major",
                                      "C1 Minor", "C Minor",
                                      "E Minor", "D1 Minor",
                                      "F Major", "F Minor")

# Final Summary Table for Key and Mode vs Frequency
Key.Mode.Freq <- data.frame("Key and Mode" = grouped_info$`paste0(key, mode)`,
                            "Number of Songs" = grouped_info$freq)