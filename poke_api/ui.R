# ui.R

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(DT)
library(shinycssloaders)

# Definición de la UI
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Pokémon"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-bar")),
      menuItem("Comparación", tabName = "comparacion", icon = icon("balance-scale")),
      menuItem("Datos", tabName = "datos", icon = icon("table")),
      menuItem("Lista de Pokémon", tabName = "lista_pokemon", icon = icon("list"))
    ),
    textInput('pokemon', 'Ingresa el nombre de un Pokémon', value = 'pikachu'),
    actionButton('fetch', 'Consultar Pokémon')
  ),
  dashboardBody(
    shinythemes::themeSelector(), 
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
              plotlyOutput('pokemonPlot') %>% withSpinner(color="#0dc5c1")),
      
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
              DTOutput('pokemonTable') %>% withSpinner(color="#0dc5c1")),
      
      # Pestaña de Lista de Pokémon
      tabItem(tabName = "lista_pokemon",
              h2("Lista de Pokémon"),
              selectInput('pokemon_select', 'Selecciona un Pokémon:', choices = NULL),
              actionButton('show_stats', 'Mostrar Estadísticas'),
              plotlyOutput('selectedPokemonPlot') %>% withSpinner(color="#0dc5c1"),
              DTOutput('selectedPokemonTable') %>% withSpinner(color="#0dc5c1"))
    )
  )
)
