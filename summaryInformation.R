library("dplyr")

information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")


summary_info <- information %>% 
  select(artist_name, track_name, energy, tempo, valence, popularity, speechiness, key, mode) %>% 
  filter(popularity != 0, speechiness < 0.66, key != -1) %>% 
  mutate(key_and_mode = paste0(key, " ", mode))

  
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
