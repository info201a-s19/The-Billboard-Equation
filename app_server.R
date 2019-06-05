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
  
  #Energy vs. Popularity Linear Regression
  
  energy_pop_all <- filtered_info %>% 
    select(energy, popularity)
  
  energy_pop <- energy_pop[sample(nrow(energy_pop), 100, replace = FALSE), ]
  
  energy_intercept <- lm(popularity~energy, data = energy_pop)
  energy_pop_r <- summary(energy_intercept)
  r2_energy <- energy_pop_r$r.squared
  my.p_energy <- energy_pop_r$coefficients[2,4]
  
  rp_energy <- c('expression', 2)
  rp_energy[1] <- paste("R2: ", format(r2_energy, digits = 3))
  rp_energy[2] <- paste("p-value: ", format(my.p_energy, digits = 2))
  
  e <- energy_pop$energy
  e_pop <- energy_pop$popularity
  energy_correlation <- cor(e, e_pop)
  
  
  output$energy_plot <- renderPlot(
    ggplot(energy_pop, aes_string(x = energy_pop$energy, 
                                  y = energy_pop$popularity)) +
      geom_point(shape = 1) +
      xlab("Energy") +
      ylab("Popularity") +
      geom_smooth(method = "lm") 
    
  )
  
  #Tempo vs. Popularity Linear Regression
  tempo_pop_all <- filtered_info %>%
    select(tempo, popularity)
  
  tempo_pop <- tempo_pop[sample(nrow(tempo_pop), 100, replace = FALSE), ]
  
  
  tempo_intercept <- lm(popularity~tempo, data = tempo_pop)
  tempo_pop_r <- summary(tempo_intercept)
  r2_tempo <- tempo_pop_r$adj.r.squared
  my.p_tempo <- tempo_pop_r$coefficients[2,4]
  
  rp_tempo = vector('expression',2)
  rp_tempo[1] = paste("R2: ", format(r2_tempo , digits = 3))
  rp_tempo[2] = paste("p-value: ", format(my.p_tempo, digits = 2))
  
  t <- tempo_pop$tempo
  t_pop <- tempo_pop$popularity
  tempo_correlation <- cor(t, t_pop)
  
  output$tempo_plot <- renderPlot(
    ggplot(tempo_pop, aes_string(x = tempo_pop$tempo, 
                                 y = tempo_pop$popularity)) +
      geom_point(shape = 1) +
      xlab("Tempo") +
      ylab("Popularity") +
      geom_smooth(method = "lm")
  )
  
  
  #Speech vs. Popularity Linear Regression
  speech_pop_all <- filtered_info %>%
    select(speechiness, popularity)
  
  
  speech_pop <- speech_pop[sample(nrow(speech_pop), 100, replace = FALSE), ]
  
  
  speech_intercept <- lm(popularity~speechiness, data = speech_pop)
  speech_pop_r <- summary(speech_intercept)
  r2_speech <- speech_pop_r$adj.r.squared
  my.p_speech <- speech_pop_r$coefficients[2,4]
  
  rp_speech = vector('expression',2)
  rp_speech[1] = paste("R2: ", format(r2_speech , digits = 3))
  rp_speech[2] = paste("p-value: ", format(my.p_speech, digits = 2))
  
  s <- speech_pop$speechiness
  s_pop <- speech_pop$popularity
  speech_correlation <- cor(s, s_pop)
  
  output$speech_plot <- renderPlot(
    ggplot(speech_pop, aes_string(x = speech_pop$speechiness, 
                                  y = speech_pop$popularity)) +
      geom_point(shape = 1) +
      xlab("Speechiness") +
      ylab("Popularity") +
      geom_smooth(method = "lm")
  )
}
