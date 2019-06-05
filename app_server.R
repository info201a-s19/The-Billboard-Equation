# app_ui
library("lintr")
library("shiny")
library("ggplot2")
library("dplyr")
# Data Wrangling
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

# ui Server
server <- function(input, output) {
  # Page 2
  # Render bar charts comparing average song
  # popularity versus key, mode, or both
  output$key_and_mode_graph <- renderPlotly({
    # Create strig matching column name using input variable
    df_name <- paste0(input$combination, "_df")
    data_frame <- get(df_name)
    # Create bar chart
    ggplot(data = data_frame) +
      geom_col(
        mapping = aes(x = reorder(data_frame[[input$combination]],
                                  average_song_popularity),
                      y = average_song_popularity)
      ) +
      coord_flip() +
      labs (
        title = "Key and Mode Combination versus Average Song Popularity",
        x = input$combination,
        y = "Average Song Popularity")
  })
  # Table
  Combo <- reactive({
    combo <- filtered_info %>%
      filter(key_and_mode == input$Combo) %>%
      arrange(desc(popularity)) %>%
      top_n(10)
    return(combo)
  })
  # Display Table
  output$table <- renderDataTable({
    Combo()
  })
  # Page 3
  # Numeric Filter
  grouped_tempo <- reactive({
    percent_display <- filtered_info %>%
      arrange(desc(popularity)) %>%
      top_n(as.integer(nrow(filtered_info) / 100 * input$Percent)) %>%
      group_by(tempo_category) %>%
      summarize(avg_popularity = mean(popularity))
    return(percent_display)
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
  # Page 4
  # Slider
  Popularity <- reactive({
    pop <- filtered_info %>%
      filter(popularity >= input$Popularity[[1]],
             popularity <= input$Popularity[[2]]) %>%
      group_by(artist_name) %>%
      summarize(songs = n())
    return(pop)
  })

  # Pie Chart
  output$Pie_Chart <- renderPlot({
    pie_chart <- ggplot(Popularity(), aes(x = "",
                                          y = songs,
                                          fill = artist_name)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      ggtitle("Artists with songs in Selected Popularity Range") +
      coord_polar("y", start = 0) +
      theme(axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.title = element_blank()) +
      guides(fill = guide_legend(override.aes = list(color = "black")))
    return(pie_chart)
  })

  # Summary Statistical Analysis

  #Energy vs. Popularity Plot
  output$energy_plot <- renderPlot(
    ggplot(energy_pop, aes_string(x = energy_pop$energy,
                                  y = energy_pop$popularity)) +
      geom_point(shape = 1) +
      xlab("Energy") +
      ylab("Popularity") +
      geom_smooth(method = "lm")

  )

  #Tempo vs. Popularity Plot
  output$tempo_plot <- renderPlot(
    ggplot(tempo_pop, aes_string(x = tempo_pop$tempo,
                                 y = tempo_pop$popularity)) +
      geom_point(shape = 1) +
      xlab("Tempo") +
      ylab("Popularity") +
      geom_smooth(method = "lm")
  )

  #Speech vs. Popularity Plot
  output$speech_plot <- renderPlot(
    ggplot(speech_pop, aes_string(x = speech_pop$speechiness,
                                  y = speech_pop$popularity)) +
      geom_point(shape = 1) +
      xlab("Speechiness") +
      ylab("Popularity") +
      geom_smooth(method = "lm")
  )
}
