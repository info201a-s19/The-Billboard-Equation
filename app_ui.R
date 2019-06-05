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

# Apply function to data
filtered_info$tempo_category <- cut(filtered_info$tempo,
                                    breaks = seq(0, 250, by = 10),
                                    labels = seq(0, 240, by = 10),
                                    right = FALSE)

# ui Page
ui <- navbarPage(
  "The Billboard Equation",
  # Overview Page (Page 1)
  tabPanel(
    ("Overview"),
  h3("Purpose:"),
  p("Our app,", strong("The Billboard Equation"),
    ", aims to help users better understand music trends of our",
    "generation by highlight snapshots of the industry as a",
    "whole. In addressing these questions, characteristics",
    "useful for describing each song:", em("artist name,"),
    em("track name, energy, tempo, valence, speechiness,"),
    em("key, mode,"), "and", em("popularity"), "."),
  p("Amongst these factors,", em("popularity"), "stood out as",
    "the sole dependent/responding variable of interest,",
    "whereas the other eight variables were studied as",
    "independent/manipulated variables--some categorical and",
    "some continuous by nature. Specifically, this app",
    "features insights pertaining to three main categories of",
    "interest:", em("tempo vs. popularity, artist name vs."),
    em("popularity"), "and", em("key-mode combo vs."),
    em("popularity"), "."),
  h3("Effect of Key-Mode Combo on Song Popularity:"),
  p("Specifically, in the tab for", em("key-mode"),
    em("combination vs. popularity"), "the user can view a",
    "ranking of the key-mode combinations by average",
    "popularity. In addition, the user has the option to",
    "view listings of popular songs within each key-mode",
    "group, which can help to better understand what that",
    "key-mode sounds like through recognition of patterns."),
  h3("Effect of Tempo on Song Popularity:"),
  p("In the tab for", em("tempo vs."),
    em("popularity"), ", the user can examine the effect that",
    "a song's tempo has on the popularity of that song,",
    "which can resolve hypotheses that slower or faster",
    "songs are more likely to gain traction within the",
    "audience worldwide."),
  h3("Artists' Effect on Song Popularity:"),
  p("Finally, in the tab for", em("artist vs. popularity"),
    ", the user can study popularity as a function of",
    "specific artists, which gives insight into", em("who"),
    "controls", em("how much"), "of the competitive music",
    "industry as of recent. Since we assume most users of",
    "our app (although it's okay if you aren't!) will",
    "recognize most (if not all) of the names that appear",
    "within the top of this list, this can help a user",
    "see how the most popular artists rank against one",
    "another in this space."),
  h3("Rationale:"),
  p("Overall, although this app does not explore every",
    "combination of popularity versus an independent",
    "variable, it focuses on the ones that we find have",
    "the most most logical connections. Although",
    "relationships could also have been drawn between",
    "popularity and variables such as Spotify ID, song",
    "duration, energy, and track name, this app avoids",
    "pushing forward these relationships, since a lack",
    "of logical rationale for such connections might",
    "imply false conclusions based in confounding",
    "factors inherent within such such a nuanced set",
    "of data points."),
  p("Hopefully, this app helps the user not only to",
    "notice the relationship between these factors and",
    "a song's popularity, but also to visualize and see",
    "information within certain ranges that the user",
    "can input in each tab.")
  ),

  # Page 3
  tabPanel(
    ("Tempo vs. Popularity"),
  sidebarLayout(
    numericInput("Percent",
                 label = h4("Select Percent of Songs"),
                 value = 100),
    mainPanel(
      plotOutput("line_graph"),
      p("This graph shows the relationship between groups",
        "sorted by", em("tempo"), "and the",
        em("average popularity"), "of songs within those",
        "groups. To change the range of the data displayed,",
        "enter a percentage between", strong("0"), "and",
        strong("100"), ", which will determine the fraction",
        "of songs included in the data set. For instance,",
        "an input value of", strong("100"), ", will include",
        strong("every"), "single song within the data set,",
        "while an input value of", strong("1"), "will only",
        "show the top", strong("1%"), "of songs (by",
        "popularity). This graph uses visual to show the",
        "overall trend depicting this relationship.")
      )
    )
  ),

  # Page 4
  tabPanel(
    ("Popular Artists"),
  sidebarLayout(
    sliderInput("Popularity",
                label = h4("Select Popularity Range"),
                min = 0,
                max = 100,
                value = c(97, 100)
    ),
    mainPanel(
      plotOutput("Pie_Chart"),
      p("This pie chart demonstrates the market share that each",
        "that each artist remains at the very highest levels of",
        "popularity, the user can increase or decrease the pool",
        "of artists to include in the pie chart total. It is",
        strong(em("strongly suggested")), "that the user choose",
        "a", strong(em("small")), "range each time, in order to",
        "focus on group sizes that can be differentiated",
        "between by the human eye.")
    )
  )
)
)
