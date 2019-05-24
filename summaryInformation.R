library("dplyr")

# Load csv file
information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")
## A function that calculates summary information to be included in your report
# Filter undesired information such as podcasts
reduced_info <- information %>%
  select(artist_name, track_name, energy, tempo, valence, popularity, speechiness, key, mode) %>% 
  filter(popularity != 0, speechiness < 0.66, key != -1)

# Filter doubled songs
filtered_info <- reduced_info %>%
  distinct(artist_name, track_name, .keep_all = TRUE)

# Number of songs in data frame
Num_Songs <- nrow(filtered_info)

# Number of artists in data frame
Num_Artists <- length(unique(filtered_info$artist_name))

# Average Popularity, Lower Quartile, Upper Quartilecalculations
Avg_Popularity <- mean(filtered_info$popularity)
Quartiles <- quantile(filtered_info$popularity)
Lower_Quart <- Quartiles[2]
Upper_Quart <- Quartiles[4]
# Final Summary of Information 
table_summary <- data.frame("Information" = c("Number of Songs", "Number of Artists", "Average Popularity",
                            "Lower Quartile of Popularity", "Upper Quartile of Popularity"),
                            "Value" = c(Num_Songs, Num_Artists, Avg_Popularity, Lower_Quart, Upper_Quart))

## A function that calculates summary information about the key-mode combinations and frequency
# Create new variable -- Key and Mode
key_and_mode_info <- filtered_info %>%
  mutate(key_and_mode = paste0(key, mode))

# Group the data by Key and Mode and arranging the data by frequency
grouped_info <- key_and_mode_info %>%
  group_by(paste0(key, mode)) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq))

grouped_info$`paste0(key, mode)` <- c("C Major", "D Major", "C1 Major", "E Major", "E1 Major", "D1 Major",
                              "G1 Minor", "A Major", "E1 Minor", "A Minor", "G Minor", "F1 Minor",
                              "B Minor", "D Minor", "B Major", "F1 Major", "G1 Major", "G Major",
                              "C1 Minor", "C Minor", "E Minor", "D1 Minor", "F Major", "F Minor")

# Final Summary Table for Key and Mode vs Frequency
Key.Mode.Freq <- data.frame("Key and Mode" = grouped_info$`paste0(key, mode)`,
                            "Number of Songs" = grouped_info$freq)

## ignore below for now
# Get summary information about the dataset
get_summary_info <- function(dataset) {
    ret <- list()
    
    ret$number_of_songs <- nrow(dataset)
    
    ret$number_of_unique_artists <- length(unique(dataset$artist_name))
    
    ret$mean_popularity <- mean(dataset$popularity)
    
    ret$lower_quartile  <- quantile(dataset$popularity, 0.25)
    
    ret$upper_quartile  <- quantile(dataset$popularity, 0.75)

    return (ret)
} 

get_summary_info(summary_info)

doubles_filtered <- summary_info %>%
  distinct(artist_name, track_name, .keep_all = TRUE)

