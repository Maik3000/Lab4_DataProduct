# ui.R

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(DT)
library(shinycssloaders)
library(leaflet)

# Definición de la UI
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Pokémon"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-bar")),
      menuItem("Comparación", tabName = "comparacion", icon = icon("balance-scale")),
      menuItem("Datos", tabName = "datos", icon = icon("table")),
      menuItem("Análisis Avanzado", tabName = "analisis_avanzado", icon = icon("chart-line")),  # Nuevo menú
      menuItem("Mapa de Pokémon", tabName = "mapa_pokemon", icon = icon("map"))
    ),
    textInput('pokemon', 'Ingresa el nombre de un Pokémon', value = 'pikachu'),
    actionButton('fetch', 'Consultar Pokémon')
  ),
  dashboardBody(
    tabItems(
      # Pestaña de Inicio
      tabItem(tabName = "inicio",
              h2("Bienvenido al Análisis de Pokémon"),
              p("Utiliza esta aplicación para explorar y comparar estadísticas de distintos Pokémon.")),
      
      # Pestaña de Gráficos
      tabItem(tabName = "graficos",
              h2("Gráficos Interactivos"),
              checkboxGroupInput('selected_stats', 'Selecciona las estadísticas a mostrar:',
                                 choices = c("hp", "attack", "defense", "special-attack", "special-defense", "speed"),
                                 selected = c("hp", "attack", "defense", "special-attack", "special-defense", "speed"),
                                 inline = TRUE),
              plotlyOutput('pokemonPlot', height = 400) %>% withSpinner(color="#0dc5c1"),
              DTOutput('statDetailTable') %>% withSpinner(color="#0dc5c1")
      ),
      
      # Pestaña de Comparación
      tabItem(tabName = "comparacion",
              h2("Comparación de Pokémon"),
              fluidRow(
                column(6, textInput('pokemon1', 'Pokémon 1', value = 'bulbasaur')),
                column(6, textInput('pokemon2', 'Pokémon 2', value = 'charmander'))
              ),
              actionButton('compare', 'Comparar'),
              checkboxGroupInput('selected_stats_compare', 'Selecciona las estadísticas a comparar:',
                                 choices = c("hp", "attack", "defense", "special-attack", "special-defense", "speed"),
                                 selected = c("hp", "attack", "defense", "special-attack", "special-defense", "speed"),
                                 inline = TRUE),
              plotlyOutput('comparisonPlot') %>% withSpinner(color="#0dc5c1")),
      
      # Pestaña de Datos
      tabItem(tabName = "datos",
              h2("Datos Interactivos"),
              DTOutput('pokemonTableEditable') %>% withSpinner(color="#0dc5c1")),
      
      # Pestaña de Análisis Avanzado
      tabItem(tabName = "analisis_avanzado",
              h2("Análisis Avanzado de Pokémon"),
              DTOutput('allPokemonTable') %>% withSpinner(color="#0dc5c1"),
              uiOutput('variableSelectors'),
              actionButton('updatePlot', 'Actualizar Gráfico'),
              plotlyOutput('scatterPlot') %>% withSpinner(color="#0dc5c1")),
      
      # Pestaña de Mapa de Pokémon
      tabItem(tabName = "mapa_pokemon",
              h2("Mapa de Pokémon"),
              fluidRow(
                column(8, leafletOutput('pokemonMap') %>% withSpinner(color="#0dc5c1")),
                column(4, DTOutput('locationTable') %>% withSpinner(color="#0dc5c1"))
              ),
              DTOutput('locationPokemonTable') %>% withSpinner(color="#0dc5c1"))
    )
  )
)
