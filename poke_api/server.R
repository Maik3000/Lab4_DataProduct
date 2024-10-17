# server.R

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)
library(geosphere)

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
        Valor = sapply(data$stats, function(stat) stat$base_stat),
        stringsAsFactors = FALSE
      )
      return(list(stats = stats, data = data))
    } else {
      return(NULL)
    }
  }
  
  # Obtener la lista de todos los Pokémon con detalles
  allPokemonData <- reactiveVal()
  
  observe({
    showModal(modalDialog("Cargando datos de todos los Pokémon, por favor espera...", footer = NULL))
    # Obtener la lista de Pokémon
    response <- GET("https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0")
    if (status_code(response) == 200) {
      data <- content(response, as = "parsed", type = "application/json")
      pokemon_urls <- sapply(data$results, function(p) p$url)
      # Limitar a los primeros 150 Pokémon para evitar tiempos de carga largos
      pokemon_urls <- pokemon_urls[1:150]
      
      # Obtener detalles de cada Pokémon
      pokemon_list <- lapply(pokemon_urls, function(url) {
        response <- GET(url)
        if (status_code(response) == 200) {
          data <- content(response, as = "parsed", type = "application/json")
          types <- sapply(data$types, function(t) t$type$name)
          stats <- sapply(data$stats, function(stat) stat$base_stat)
          c(name = data$name, type1 = types[1], type2 = ifelse(length(types) > 1, types[2], NA),
            hp = stats[1], attack = stats[2], defense = stats[3],
            special_attack = stats[4], special_defense = stats[5], speed = stats[6])
        } else {
          NULL
        }
      })
      # Convertir a data frame
      pokemon_df <- do.call(rbind, pokemon_list)
      pokemon_df <- as.data.frame(pokemon_df, stringsAsFactors = FALSE)
      # Convertir las columnas numéricas
      num_cols <- c("hp", "attack", "defense", "special_attack", "special_defense", "speed")
      pokemon_df[num_cols] <- lapply(pokemon_df[num_cols], as.numeric)
      allPokemonData(pokemon_df)
      removeModal()
    } else {
      showNotification("No se pudo obtener la lista de Pokémon.", type = "error")
      removeModal()
    }
  })
  
  # Mostrar la tabla de todos los Pokémon
  output$allPokemonTable <- renderDT({
    req(allPokemonData())
    datatable(allPokemonData(), selection = 'multiple', options = list(pageLength = 10))
  })
  
  # Generar selectores de variables para el gráfico
  output$variableSelectors <- renderUI({
    req(allPokemonData())
    num_vars <- c("hp", "attack", "defense", "special_attack", "special_defense", "speed")
    fluidRow(
      column(6, selectInput('xVariable', 'Variable en el eje X', choices = num_vars, selected = 'attack')),
      column(6, selectInput('yVariable', 'Variable en el eje Y', choices = num_vars, selected = 'defense'))
    )
  })
  
  # Generar el gráfico de dispersión
  observeEvent(input$updatePlot, {
    req(input$allPokemonTable_rows_selected)
    req(input$xVariable)
    req(input$yVariable)
    data <- allPokemonData()
    selected_data <- data[input$allPokemonTable_rows_selected, ]
    
    # Preparar los datos para el gráfico
    selected_data$type1 <- as.factor(selected_data$type1)
    
    output$scatterPlot <- renderPlotly({
      plot_ly(selected_data, x = as.formula(paste0("~", input$xVariable)), y = as.formula(paste0("~", input$yVariable)),
              type = 'scatter', mode = 'markers', color = ~type1, colors = "Set1",
              text = ~paste("Nombre:", name, "<br>Tipo:", type1, ifelse(!is.na(type2), paste("/", type2), ""))) %>%
        layout(title = "Gráfico de Dispersión de Pokémon",
               xaxis = list(title = input$xVariable),
               yaxis = list(title = input$yVariable))
    })
  })
  
  # Las demás funcionalidades permanecen igual...
  
  ### Funcionalidad del mapa con selección de ubicaciones desde la tabla ###
  
  # Variable reactiva para almacenar las áreas de ubicación y sus coordenadas
  locationAreas <- reactiveVal()
  
  # Al iniciar, obtener las áreas de ubicación y asignar coordenadas aleatorias
  observe({
    showModal(modalDialog("Cargando datos de ubicaciones, por favor espera...", footer = NULL))
    response <- GET("https://pokeapi.co/api/v2/location-area?limit=200&offset=0")
    if (status_code(response) == 200) {
      data <- content(response, as = "parsed", type = "application/json")
      areas <- data$results
      # Crear un data frame con las áreas y asignar coordenadas aleatorias
      area_names <- sapply(areas, function(a) a$name)
      n_areas <- length(area_names)
      set.seed(123) # Para reproducibilidad
      latitudes <- runif(n_areas, min = -90, max = 90)
      longitudes <- runif(n_areas, min = -180, max = 180)
      locations_df <- data.frame(
        area = area_names,
        lat = latitudes,
        lon = longitudes,
        stringsAsFactors = FALSE
      )
      locationAreas(locations_df)
      removeModal()
    } else {
      showNotification("No se pudo obtener la lista de áreas de ubicación.", type = "error")
      removeModal()
    }
  })
  
  # Renderizar el mapa con las ubicaciones
  output$pokemonMap <- renderLeaflet({
    req(locationAreas())
    locations <- locationAreas()
    leaflet(locations) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, label = ~area, layerId = ~area, radius = 5)
  })
  
  # Mostrar la tabla de ubicaciones
  output$locationTable <- renderDT({
    req(locationAreas())
    datatable(locationAreas()[, c("area")], selection = 'single', options = list(pageLength = 5))
  })
  
  # Variable reactiva para almacenar los Pokémon del área seleccionada
  selectedAreaPokemon <- reactiveVal()
  
  # Evento al seleccionar una ubicación en la tabla
  observeEvent(input$locationTable_rows_selected, {
    req(locationAreas())
    selected_row <- input$locationTable_rows_selected
    if (!is.null(selected_row)) {
      selected_location <- locationAreas()[selected_row, ]
      area_name <- selected_location$area
      # Centrar el mapa en la ubicación seleccionada y resaltar el marcador
      leafletProxy('pokemonMap') %>%
        setView(lng = selected_location$lon, lat = selected_location$lat, zoom = 8) %>%
        clearGroup('selected') %>%
        addCircleMarkers(lng = selected_location$lon, lat = selected_location$lat, color = 'red', radius = 10, group = 'selected', label = area_name)
      
      # Obtener los Pokémon que se encuentran en esta área
      url <- paste0("https://pokeapi.co/api/v2/location-area/", area_name)
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- content(response, as = "parsed", type = "application/json")
        pokemon_encounters <- data$pokemon_encounters
        if (length(pokemon_encounters) > 0) {
          pokemon_names <- sapply(pokemon_encounters, function(p) p$pokemon$name)
          pokemon_df <- data.frame(Pokémon = pokemon_names, stringsAsFactors = FALSE)
          selectedAreaPokemon(pokemon_df)
          output$locationPokemonTable <- renderDT({
            datatable(pokemon_df, options = list(pageLength = 5))
          })
        } else {
          pokemon_df <- data.frame(Pokémon = character(0), stringsAsFactors = FALSE)
          output$locationPokemonTable <- renderDT({
            datatable(pokemon_df, options = list(pageLength = 5))
          })
          showNotification("No hay Pokémon en esta ubicación.", type = "warning")
        }
      } else {
        showNotification("No se pudo obtener los Pokémon de esta ubicación.", type = "error")
      }
    }
  })
  
  # Inicializar locationPokemonTable para evitar errores
  output$locationPokemonTable <- renderDT({
    datatable(data.frame(Pokémon = character(0)), options = list(pageLength = 5))
  })
  
  # Evento al seleccionar un Pokémon de la tabla
  observeEvent(input$locationPokemonTable_rows_selected, {
    pokemon_df <- selectedAreaPokemon()
    selected_row <- input$locationPokemonTable_rows_selected
    if (length(selected_row) && !is.null(pokemon_df)) {
      pokemon_name <- pokemon_df$Pokémon[selected_row]
      # Obtener y mostrar las estadísticas del Pokémon seleccionado
      stats_result <- getPokemonData(pokemon_name)
      if (!is.null(stats_result)) {
        stats <- stats_result$stats
        showModal(modalDialog(
          title = paste("Estadísticas de", pokemon_name),
          renderPlotly({
            plot_ly(stats, x = ~Estadística, y = ~Valor, type = 'bar',
                    marker = list(color = 'rgba(55, 128, 191, 0.6)')) %>%
              layout(title = paste("Estadísticas de", pokemon_name))
          }),
          easyClose = TRUE,
          size = "l"
        ))
      } else {
        showNotification("No se pudieron obtener las estadísticas del Pokémon seleccionado.", type = "error")
      }
    }
  })
  
}

