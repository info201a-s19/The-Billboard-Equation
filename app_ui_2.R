# app_ui
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

# Select top artists
top_artists <- filtered_info %>%
  group_by(artist_name) %>%
  summarize(avg_popularity = mean(popularity)) %>%
  arrange(desc(avg_popularity)) %>%
  top_n(50) %>%
  select(artist_name)

artists <- as.vector(t(top_artists))

choices <- as.list(artists)

# Function to categoricalize tempo
cat_tempo <- function(dataset) {
  tempo_category <- cut(dataset$tempo,
                        breaks = seq(0, 250, by = 10),
                        labels = seq(0, 240, by = 10),
                        right = FALSE)
  return(tempo_category)
}

# Apply function to data
filtered_info$tempo_category <- cat_tempo(filtered_info)

# ui Page
ui <- navbarPage(
  "The Billboard Equation",
  tabPanel("Tempo vs. Popularity"),
  sidebarLayout(
    sidebarPanel(h4("Select Artist(s) of Interest"),
                  checkboxGroupInput("Artists",
                                     label = h3("Select Artist(s)"),
                                     choices = choices,
                                     selected = artists)
    ),
    mainPanel(
      plotOutput("line_graph")
    )
  )
)

# ui Server
server <- function(input, output) {
  # Checkbox Group (Widget)
  grouped_tempo <- reactive({
    grouped_tempo <- filtered_info %>%
      filter(artist_name %in% input$Artists) %>%
      group_by(tempo_category) %>%
      summarize(avg_popularity = mean(popularity))
    return(grouped_tempo)
  })

  # Construct Line Graph
  output$line_graph <- renderPlot({
    ggplot(grouped_tempo(), aes(x = tempo_category,
                                y = avg_popularity,
                                group = 1)) +
      labs(title = "Tempo vs. Popularity",
           x = "Tempo Category",
           y = "Average Popularity") +
      geom_line() +
      geom_point()
  })
}

# app
shinyApp(ui = ui, server = server)
