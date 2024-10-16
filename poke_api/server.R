# server.R

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(plotly)
library(DT)

# Definición del Servidor
server <- function(input, output, session) {
  
  # Función para obtener datos de un Pokémon desde la API
  getPokemonData <- function(pokemonName) {
    url <- paste0("https://pokeapi.co/api/v2/pokemon/", tolower(pokemonName))
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- content(response, as = "parsed", type = "application/json")
      stats <- data.frame(
        Estadística = sapply(data$stats, function(stat) stat$stat$name),
        Valor = sapply(data$stats, function(stat) stat$base_stat)
      )
      return(stats)
    } else {
      return(NULL)
    }
  }
  
  # Obtener la lista de todos los Pokémon
  pokemonList <- reactiveVal()
  
  # Al iniciar, carga la lista de Pokémon y actualiza el selectInput
  observe({
    response <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0")
    if (status_code(response) == 200) {
      data <- content(response, as = "parsed", type = "application/json")
      names <- sapply(data$results, function(p) p$name)
      pokemonList(names)
      updateSelectInput(session, 'pokemon_select', choices = names)
    } else {
      showNotification("No se pudo obtener la lista de Pokémon.", type = "error")
    }
  })
  
  # Evento para obtener y mostrar datos del Pokémon
  observeEvent(input$fetch, {
    stats <- getPokemonData(input$pokemon)
    if (!is.null(stats)) {
      # Actualiza la tabla de datos
      output$pokemonTable <- renderDT({
        datatable(stats, options = list(pageLength = 5))
      })
      
      # Actualiza la gráfica al cambiar las estadísticas seleccionadas
      observe({
        req(input$selected_stats)
        filtered_stats <- stats %>% filter(Estadística %in% input$selected_stats)
        
        output$pokemonPlot <- renderPlotly({
          plot_ly(filtered_stats, x = ~Estadística, y = ~Valor, type = 'bar', 
                  marker = list(color = 'rgba(55, 128, 191, 0.6)')) %>%
            layout(title = paste("Estadísticas de", input$pokemon))
        })
      })
      
    } else {
      showNotification("Pokémon no encontrado. Intenta con otro nombre.", type = "error")
    }
  })
  
  # Evento para comparar dos Pokémon
  observeEvent(input$compare, {
    stats1 <- getPokemonData(input$pokemon1)
    stats2 <- getPokemonData(input$pokemon2)
    
    if (!is.null(stats1) && !is.null(stats2)) {
      # Combina las estadísticas de ambos Pokémon
      combinedStats <- merge(stats1, stats2, by = "Estadística", suffixes = c('_1', '_2'))
      
      # Actualiza la gráfica de comparación al cambiar las estadísticas seleccionadas
      observe({
        req(input$selected_stats_compare)
        filtered_combinedStats <- combinedStats %>% filter(Estadística %in% input$selected_stats_compare)
        
        output$comparisonPlot <- renderPlotly({
          plot_ly(filtered_combinedStats, x = ~Estadística) %>%
            add_trace(y = ~Valor_1, name = input$pokemon1, type = 'bar') %>%
            add_trace(y = ~Valor_2, name = input$pokemon2, type = 'bar') %>%
            layout(barmode = 'group', title = paste("Comparación entre", input$pokemon1, "y", input$pokemon2))
        })
      })
      
    } else {
      showNotification("Uno o ambos Pokémon no fueron encontrados. Intenta con otros nombres.", type = "error")
    }
  })
  
  # Evento para mostrar estadísticas del Pokémon seleccionado de la lista
  observeEvent(input$show_stats, {
    req(input$pokemon_select)
    stats <- getPokemonData(input$pokemon_select)
    if (!is.null(stats)) {
      output$selectedPokemonPlot <- renderPlotly({
        plot_ly(stats, x = ~Estadística, y = ~Valor, type = 'bar', 
                marker = list(color = 'rgba(55, 128, 191, 0.6)')) %>%
          layout(title = paste("Estadísticas de", input$pokemon_select))
      })
      
      output$selectedPokemonTable <- renderDT({
        datatable(stats, options = list(pageLength = 5))
      })
    } else {
      showNotification("Pokémon no encontrado. Intenta con otro nombre.", type = "error")
    }
  })
}
