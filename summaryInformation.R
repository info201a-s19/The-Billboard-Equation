library("dplyr")

# Load csv file
information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")



# Filter undesired information such as podcasts and create new variable -- Key and Mode
filtered_info <- information %>% 
  select(artist_name, track_name, energy, tempo, valence, popularity, speechiness, key, mode) %>% 
  filter(popularity != 0, speechiness < 0.66, key != -1) %>% 
  mutate(paste0(key, mode))

# Filter doubled songs
filtered_info <- filtered_info %>%
  distinct(artist_name, track_name, .keep_all = TRUE)

# Group the data by Key and Mode and arranging the data by frequency
data <- filtered_info %>%
  group_by(paste0(key, mode)) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq))

data$`paste0(key, mode)` <- c("C Major", "D Major", "C1 Major", "E Major", "E1 Major", "D1 Major",
                              "G1 Minor", "A Major", "E1 Minor", "A Minor", "G Minor", "F1 Minor",
                              "B Minor", "D Minor", "B Major", "F1 Major", "G1 Major", "G Major",
                              "C1 Minor", "C Minor", "E Minor", "D1 Minor", "F Major", "F Minor")

## Final Summary Table for Key and Mode vs Frequency
Key.Mode.Freq <- data.frame("Key and Mode" = data$`paste0(key, mode)`,
                            "Number of Songs" = data$freq)

### ignore below for now
get_summary_info <- function(dataset) {
    ret <- list()
    ret$size <- nrow(dataset)
    
    ret$energyRegression <- lm(dataset$energy ~ dataset$popularity, data = dataset)
    ret$energyRegression <- summary(ret$energyRegression)$r.squared
    
    ret$tempoRegression  <- lm(dataset$tempo ~ dataset$popularity, data = dataset)
    ret$tempoRegression <- summary(ret$tempoRegression)$r.squared
    
    ret$valenceRegression <- lm(dataset$valence ~ dataset$popularity, data = dataset)
    ret$valenceRegression <- summary(ret$valenceRegression)$r.squared
    
    ret$meanPopularity <- mean(dataset$popularity)
    return (ret)
} 

get_summary_info(summary_info)