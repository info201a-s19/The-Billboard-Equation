# install.packages("shiny")
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("plotly")

# DEFINE UI
ui <- navbarPage(
  tabPanel(
    "Key and Mode",
    titlePanel("Key and Mode versus Average Song Popularity"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "combination",
                    label = "View Song Popularity By:",
                    choices = list("key", "mode", "both"),
                    selected = "key",
                    multiple = FALSE
        )
      ),
      mainPanel(
        plotlyOutput("key_and_mode_graph")
      )
    )
  )
)


# WRANGLE DATA (GOES IN SERVER FILE)
df_with_key_mode_string <- data_frame %>%
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
  mutate(both = paste(key, mode)) %>%
  select(artist_name, track_name, both, key, mode, popularity)

both_df <- df_with_key_mode_string %>%
  group_by(both) %>%
  summarize(average_song_popularity = mean(popularity))

key_df <- df_with_key_mode_string %>%
  group_by(key) %>%
  summarize(average_song_popularity = mean(popularity))

mode_df <- df_with_key_mode_string %>%
  group_by(mode) %>%
  summarize(average_song_popularity = mean(popularity))


# Wrangle data to identify top ten songs for each categorgy

# DEFINE SERVER
server <- function(input, output) {
  
  # Render bar charts comparing average song popularity versus key, mode, or both
  output$key_and_mode_graph <- renderPlotly({
    
    # Create strig matching column name using input variable
    df_name <- paste0(input$combination, "_df")
    data_frame = get(df_name)

    
    # Create bar chart
    ggplot(data = data_frame) +
      geom_col(
        mapping = aes(x = reorder(data_frame[[input$combination]], average_song_popularity),
                      y = average_song_popularity)
      ) +
      coord_flip() +
      labs (
        title = "Key and Mode Combination versus Average Song Popularity",
        x = input$combination,
        y = "Average Song Popularity"
      )
  })
}

shinyApp(ui = ui, server = server)
 