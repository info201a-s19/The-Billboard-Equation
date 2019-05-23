library("dplyr")

information <- read.csv("csvFiles/SpotifyAudioFeaturesApril2019.csv")


summary_info <- information %>% 
  select(artist_name, track_name, energy, tempo, valence, popularity, speechiness) %>% 
  filter(popularity != 0, speechiness < 0.66)

  
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



filtering_doubles <- function(dataset) {
  f <- data.frame()
  f <- duplicated(dataset$artist_name)
  return(f)
}

filtering_doubles(summary_info)
