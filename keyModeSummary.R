library("dplyr")
library("lintr")

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
                                        "C# Minor", "C# Major",
                                        "D Minor", "D Major",
                                        "D# Minor", "D# Major",
                                        "E Minor", "E Major",
                                        "F Minor", "F Major",
                                        "F# Minor", "F# Major",
                                        "G Minor", "G Major",
                                        "G# Minor", "G# Major",
                                        "A Minor", "A Major",
                                        "A# Minor", "A# Major",
                                        "B Minor", "B Major"), right = FALSE)
  # Final Summary for Key and Mode vs Frequency
  Key_Mode_Freq <- data.frame("Key and Mode" = Key_Mode_Categories,
                              "Number of Songs" = grouped_info$freq)
  # Return desired data
  return(Key_Mode_Freq)
}

create_table <- function(data){
  kable(Key_Mode_Freq)
}
