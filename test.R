library("lintr")
library("shiny")
library("ggplot2")
library("dplyr")

# Load data
information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")

# Actual filtered info in case function doesn't work
filtered_info <- information %>%
  select(artist_name, track_name, energy, tempo, valence,
         popularity, speechiness, key, mode) %>%
  filter(popularity != 0, speechiness < 0.66) %>%
  distinct(artist_name, track_name, .keep_all = TRUE)

# Group by Key
filtered_info$key <- cut(filtered_info$key,
                         breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8,
                                    9, 10, 11, 12),
                         labels = c("NA", "C","C#", "D", "D#", "E",
                                    "F", "F#", "G", "G#", "A", "A#",
                                    "B"), right = F)

# Group by Mode
filtered_info$mode <- cut(filtered_info$mode,
                          breaks = c(0, 1, 2),
                          labels = c("Minor", "Major"), right = F)

# Greoup by Both
filtered_info$key_and_mode <- paste(filtered_info$key, filtered_info$mode)

# ui
ui <- navbarPage(
  tabPanel(
    "Key and Mode",
    titlePanel("Key and Mode vs. Popularity"),
    sidebarLayout(
      sidebarPanel(
        selectInput("Sort",
                    label = "View Song Popularity By:",
                    choices = list("Key" = "key",
                                   "Mode" = "mode",
                                   "Key and Mode" = "key_and_mode"),
                    selected = "Key",
                    multiple = FALSE
        )
      ),
      mainPanel(
        plotOutput("Bar_Graph")
      )
    )
  )
)

# server
server <- function(input, output) {
  # Select Box (Page 2)
  Sort_By <- reactive({
    sorted_data <- filtered_info %>%
      group_by(input$Sort) %>%
      summarize(avg_popularity = mean(popularity))
    return(sorted_data)
  })
  # Bar Graph
  output$bar_Graph <- renderPlot({
    if(input$Sort == "key") {
      bar_graph <- ggplot(data = Sort_By(),
                          aes(x = key,
                              y = avg_popularity)) +
        geom_bar(stat = "Identity") +
        coord_flip() +      
        labs (
          title = paste("Key vs. Popularity"),
          x = "Key",
          y = "Average Song Popularity")
    } else if(input$Sort == "mode") {
      bar_graph <- ggplot(data = Sort_By(),
                          aes(x = mode,
                              y = avg_popularity)) +
        geom_bar(stat = "Identity") +
        coord_flip() +      
        labs (
          title = paste("Mode vs. Popularity"),
          x = "Mode",
          y = "Average Song Popularity")
    } else if(input$Sort == "key_and_mode") {
      bar_graph <- ggplot(data = Sort_By(),
                          aes(x = key_and_mode,
                              y = avg_popularity)) +
        geom_bar(stat = "Identity") +
        coord_flip() +      
        labs (
          title = paste("Key Mode Combo vs. Popularity"),
          x = "Key Mode Combo",
          y = "Average Song Popularity")
    }
    return(bar_graph)
  })
}

# Run App
shinyApp(ui = ui, server = server)