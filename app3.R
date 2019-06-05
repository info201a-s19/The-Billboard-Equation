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
  filter(popularity != 0, speechiness < 0.66, key != -1) %>%
  distinct(artist_name, track_name, .keep_all = TRUE)

# ui
ui <- navbarPage(
  "The Billboard Equation",
  tabPanel("Popular Artists"),
  sidebarLayout(
    sliderInput("Popularity",
                label = h4("Select Popularity Range of Interest"),
                min = 0,
                max = 100,
                value = c(97, 100)
    ),
    mainPanel(
      plotOutput("Pie_Chart")
    )
  )
)

# Server
server <- function(input, output) {
  # Slider
  Popularity <- reactive({
    pop <- filtered_info %>%
      filter(popularity >= input$Popularity[[1]],
             popularity <= input$Popularity[[2]]) %>%
      group_by(artist_name) %>%
      summarize(songs = n())
    return(pop)
  })
  output$Pie_Chart <- renderPlot({
    pie_chart <- ggplot(Popularity(), aes(x = "",
                                          y = songs,
                                          fill = artist_name)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      coord_polar("y", start = 0) +
      theme(axis.text.x = element_blank()) +
      guides(fill = guide_legend(override.aes = list(color = "black")))
    return(pie_chart)
  })
}

# app
shinyApp(ui = ui, server = server)