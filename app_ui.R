# app_ui
library("lintr")
library("shiny")
library("ggplot2")
library("dplyr")
source("app_server.R")

# Load data
information <- read.csv("data/SpotifyAudioFeaturesApril2019.csv")

# Actual filtered info in case function doesn't work
filtered_info <- information %>%
  select(artist_name, track_name, energy, tempo, valence,
         popularity, speechiness, key, mode) %>%
  filter(popularity != 0, speechiness < 0.66, key != -1) %>%
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
filtered_info$both <- paste(filtered_info$key, filtered_info$mode)

# Create dataframes of average song popularity by key/mode/both 
both_df <- filtered_info %>%
  group_by(both) %>%
  summarize(average_song_popularity = mean(popularity))
key_df <- filtered_info %>%
  group_by(key) %>%
  summarize(average_song_popularity = mean(popularity))
mode_df <- filtered_info %>%
  group_by(mode) %>%
  summarize(average_song_popularity = mean(popularity))

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

#Energy vs. Popularity Linear Regression

energy_pop_all <<- filtered_info %>%
  select(energy, popularity)

#Get a sample of 100 random values
set.seed(2)
energy_pop <<- energy_pop_all[sample(nrow(energy_pop_all), 100), ]

#Find R2 and p-values
energy_intercept <<- lm(popularity~energy, data = energy_pop)
energy_pop_r <<- summary(energy_intercept)
r2_energy <<- energy_pop_r$r.squared
my.p_energy <<- energy_pop_r$coefficients[2, 4]

#Show R2 and p-value next to graph
rp_energy <<- c("expression", 2)
rp_energy[1] <- paste("R2: ", format(r2_energy, digits = 3))
rp_energy[2] <- paste("p-value: ", format(my.p_energy, digits = 2))

#Find correlation
e <<- energy_pop$energy
e_pop <<- energy_pop$popularity
energy_correlation <- cor(e, e_pop)


#Tempo vs. Popularity Linear Regression
tempo_pop_all <- filtered_info %>%
  select(tempo, popularity)

#Get a sample of 100 random values
set.seed(14)
tempo_pop <- tempo_pop_all[sample(nrow(tempo_pop_all), 100), ]

#Find R2 and p-values
tempo_intercept <- lm(popularity~tempo, data = tempo_pop)
tempo_pop_r <- summary(tempo_intercept)
r2_tempo <- tempo_pop_r$adj.r.squared
my.p_tempo <- tempo_pop_r$coefficients[2, 4]

#Show R2 and p-value next to graph
rp_tempo <- vector("expression", 2)
rp_tempo[1] <- paste("R2: ", format(r2_tempo, digits = 3))
rp_tempo[2] <- paste("p-value: ", format(my.p_tempo, digits = 2))

#Find correlation
tm <- tempo_pop$tempo
t_pop <- tempo_pop$popularity
tempo_correlation <- cor(tm, t_pop)


#Speech vs. Popularity Linear Regression
speech_pop_all <- filtered_info %>%
  select(speechiness, popularity)

#Get a sample of 100 random values
set.seed(24)
speech_pop <- speech_pop_all[sample(nrow(speech_pop_all),
                                    100, replace = FALSE), ]

#Find R2 and p-values
speech_intercept <- lm(popularity~speechiness, data = speech_pop)
speech_pop_r <- summary(speech_intercept)
r2_speech <- speech_pop_r$adj.r.squared
my.p_speech <- speech_pop_r$coefficients[2, 4]

#Show R2 and p-value next to graph
rp_speech <- vector("expression", 2)
rp_speech[1] <- paste("R2: ", format(r2_speech, digits = 3))
rp_speech[2] <- paste("p-value: ", format(my.p_speech, digits = 2))

#Find correlation
s <- speech_pop$speechiness
s_pop <- speech_pop$popularity
speech_correlation <- cor(s, s_pop)

# ui Page
ui <- navbarPage(
  "The Billboard Equation",
  # Overview Page (Page 1)
  tabPanel(
    title = "Overview",
    fluidPage(
      includeCSS("styles.css"),
      h1("The Billboard Equation"),
      h3("Adam Bi, Jueqi Liu, Monica Hong, Sonali Chandra, Nancy Han"),
      HTML("<hr"),
      includeHTML("Image.html")
    )
  ),
  # Page 2
  tabPanel(
  "Key and Mode",
  titlePanel("Key and Mode vs. Popularity"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "combination_average",
                  label = "View Average Song Popularity By:",
                  choices = list("key", "mode", "both"),
                  selected = "key",
                  multiple = FALSE),
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
      plotOutput("key_mode_bar_graph"),
      dataTableOutput("table"),
      p("This page of the app contains two functions for",
        "the user to navigate:"),
      p("The first is a simple sort based upon the user's",
        "selection. For example, sorting by", em("key"),
        "would give groupings named", em("C, C#, D, D#,"),
        em("etc."), ", while sorting by", em("mode"),
        "would give groupings of either", em("Minor"),
        "or", em("Major"), ". A third choice that the user",
        "can choose is to sort by the combination of the",
        "two, grouping the data by a key-mode pairing (for",
        "example", em("C# Minor"), ")."),
      p("The second function is a choice of key-mode",
        "combination by the user, the selection of which",
        "will prompt a display of all the songs within",
        "that key-mode group.")
    )
  )
),

# Page 3
tabPanel(
  ("Tempo vs. Popularity"),
  sidebarLayout(
    numericInput("Percent",
                 label = h4("Select Percent of Songs"),
                 h3(em("Please select a number under 100")),
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
        "that each artist retains at the very highest levels of",
        "popularity, the user can increase or decrease the pool",
        "of artists to include in the pie chart total. It is",
        strong(em("strongly suggested")), "that the user choose",
        "a", strong(em("small")), "range each time, in order to",
        "focus on group sizes that can be differentiated",
        "between by the human eye.")
    )
  )
),


# Page 5
tabPanel(
  ("Summary Takeaways"),
  ui <- fluidPage(
    titlePanel("Plots"),
    mainPanel(
      h5("DISCLAIMER: These graphs are only a random sample of the
         original data, with 100 points shown.
         The R2 and P values shown will be different than the
         values of the whole dataset, as this is only a sample.
         However, the significance of this sample was made to
         emulate the significance of the whole dataset
         (e.g. the dataset is not significant, so what is shown by this
         sample is not significant either).
         This was done for visualization and analysis purposes."),
      h3("Energy vs. Popularity Linear Regression"),
      plotOutput(outputId = "energy_plot"),
      h4(rp_energy[1]),
      h4(rp_energy[2]),
      h4(paste("Correlation: ", energy_correlation)),
      p("Since the p-value > 0.05, we cannot reject
        the null hypothesis
        that the energy of a song does not affect the
        popularity of the song."),
      p("There is also a very weak positive correlation between the energy
        and the popularity of a song(when one variable increases,
        the other increases as well),
        meaning as the energy of a song increases or decreases,
        there is a low likelihood of there being a relationship with the
        popularity of the song."),
      br(),
      h3("Tempo vs. Popularity Linear Regression"),
      plotOutput(outputId = "tempo_plot"),
      h4(rp_tempo[1]),
      h4(rp_tempo[2]),
      h4(paste("Correlation: ", tempo_correlation)),
      p("Since the p-value < 0.05, we can reject the null hypothesis
        that the tempo of a song does not affect the popularity of
        the song."),
      p("There is also a very weak, but stronger than energy,
        positive correlation between the tempo and the popularity of a song
        (when one variable increases, the other variable increases),
        meaning as the tempo of a song increases or decreases,
        there is a low likelihood of there being a relationship
        with the popularity of the song."),
      br(),
      h3("Speechiness vs. Popularity Linear Regression"),
      plotOutput(outputId = "speech_plot"),
      h4(rp_speech[1]),
      h4(rp_speech[2]),
      h4(paste("Correlation: ", speech_correlation)),
      p("Since the p-value > 0.05, we cannot reject the null
        hypothesis that
        the speechiness of a song does not affect the popularity of
        the song."),
      p("There is a very weak negative correlation between the speechiness
        and the popularity of a song (when one variable increases,
        the other variable decreases),
        meaning that as the speechiness of a song increases or decreases,
        there is a very low
        likelihood of there being a relationship with the
        popularity of the song."),
      br(),
      h2("Summary"),
      p("Overall, there are fairly weak correlations between the tempo,
        energy, speechiness, and popularity of a song."),
      p("As for which of the characteristics is the main driver
        behind the song's popularity,
        through the analysis of the statistics, we have concluded that",
        strong(" tempo "),
        "is one of the main drivers behind the song's popularity,
        although it is not the only one.")
      )
    )
  )
)
