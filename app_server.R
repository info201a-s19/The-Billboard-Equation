# app_ui
library("lintr")
library("shiny")
library("ggplot2")
library("dplyr")

# ui Server
server <- function(input, output) {
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

