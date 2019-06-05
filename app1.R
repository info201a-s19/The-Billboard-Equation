# app_ui
library("lintr")
library("shiny")
library("ggplot2")
library("dplyr")

#test_Sort_By <- filtered_info %>%
#    group_by(key_and_mode) %>%
#    summarize(avg_popularity = mean(popularity))

#test_bar_graph <- 

#ggplot(data = test_Sort_By) +
#  geom_col(aes(x = key_and_mode,
#              y = avg_popularity)) +
#  coord_flip() +      
#  labs (
#    title = paste("Key Mode Combo vs. Popularity"),
#    x = "Key Mode Combo",
#    y = "Average Song Popularity")



# Load data
information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")
tibble(information)
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

# Group by Both
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
                    choices = list("Key1" = "key",
                                   "Mode1" = "mode",
                                   "Key and Mode1" = "key_and_mode"),
                    selected = "Key1",
                    multiple = FALSE
        ),
        radioButtons("Combo",
                           label = ("Select Key Mode Combo"),
                           choices = list("C Minor", "C Major",
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
                                          "B Minor", "B Major"),
                           selected = "C Minor")
      ),
      mainPanel(
        plotOutput("boundless_bar_Graph"),
        dataTableOutput("table")
      )
    )
  )
)

# server
server <- function(input, output) {
  # Select Box (Page 2)
  Sort_By <- reactive ({
    sorted_data <- filtered_info %>%
      group_by(input$Sort) %>%
      summarize(avg_popularity = mean(popularity))
    sorted_data
  })
  
  # Radio Buttons (Page 2)
  Combo <- reactive({
    combo <- filtered_info %>%
      filter(key_and_mode == input$Combo) %>%
      arrange(desc(popularity)) %>%
      top_n(10)
    return(combo)
  })
  
  # Bar Graph
  output$boundless_bar_Graph <- renderPlot({
    if(input$Sort == "Key1") {
    bar_graph <- ggplot(data = Sort_By()) +
      geom_col(aes(x = key,
                   y = avg_popularity)) +
      coord_flip() +      
      labs (
        title = paste("Key vs. Popularity"),
        x = "Key",
        y = "Average Song Popularity")
    } else if (input$Sort == "Mode1") {
      bar_graph <- ggplot(data = Sort_By()) +
        geom_col(aes(x = mode,
                     y = avg_popularity)) +
        coord_flip() +      
        labs (
          title = paste("Mode vs. Popularity"),
          x = "Mode",
          y = "Average Song Popularity")
    } else if (input$Sort == "Key_and_mode1") {
      bar_graph <- ggplot(data = Sort_By()) +
        geom_col(aes(x = key_and_mode,
                     y = avg_popularity)) +
        coord_flip() +      
        labs (
          title = paste("Key Mode Combo vs. Popularity"),
          x = "Key Mode Combo",
          y = "Average Song Popularity")
    }
    bar_graph
  })
  
  output$table <- renderDataTable({
    Combo()
  })
}

# Run App
shinyApp(ui = ui, server = server)
