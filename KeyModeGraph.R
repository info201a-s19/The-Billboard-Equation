# This function returns a bar graph representing the average song popularity of
# different combinations of key and mode, including the average popularity of
# songs with no key detected

library("dplyr")

key_mode_popularity_grapher <- function(data_frame) {
  # create a new column holding strings of the song's key/mode combination
  df_with_key_mode_string <- spotify_data %>%
    # replace the double digit numbers representing keys first so gsub() can
    # differetiate between "11" and "1""2" as well as "12" and "1""2"
    mutate(key = gsub("10", "A#", key),
           key = gsub("11", "B", key),
           key = gsub("-1", "NA", key)
    ) %>%
    # replace the rest of the key numbers with strings of the keys
    mutate(key = gsub("0", "C", key),
           key = gsub("1", "C#", key),
           key = gsub("2", "D", key),
           key = gsub("3", "D#", key),
           key = gsub("4", "E", key),
           key = gsub("5", "F", key),
           key = gsub("6", "F#", key),
           key = gsub("7", "G", key),
           key = gsub("8", "G#", key),
           key = gsub("9", "A", key)
    ) %>%
    # replace the numbers representing mode with strings "Major" or "Minor"
    mutate(mode = gsub("1", "Major", mode),
           mode = gsub("0", "Minor", mode)
    ) %>%
    #create a new column by combining the key and mode strings
    mutate(key_and_mode = paste(key, mode))

  # Create a new data frame where the instances are key/mode combinations
  # and features are average song popularity, key, and mode
  key_n_mode_popularity <- df_with_key_mode_string %>%
    group_by(key_and_mode) %>%
    summarize(mode = sample(mode, 1),
              key = sample(key, 1),
              average_song_popularity = mean(popularity)
              )

  # Create a bar chart displaying the average song popularity of each key/mode
  # combination, color coded by mode, ordered by song popularity
  ggplot(data = key_n_mode_popularity) +
    geom_col(
      mapping = aes(x = reorder(key_and_mode, average_song_popularity),
                    y = average_song_popularity,
                    fill = mode)
    ) +
    coord_flip() +
    labs (
      title = "Key and Mode Combination versus Average Song Popularity",
      x = "Key and Mode",
      y = "Average Song Popularity",
      color = "Mode"
    )
}
