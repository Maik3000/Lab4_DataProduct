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
      menuItem("Datos", tabName = "datos", icon = icon("table"))
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
              plotlyOutput('pokemonPlot') %>% withSpinner(color="#0dc5c1")),
      
      # Pestaña de Comparación
      tabItem(tabName = "comparacion",
              h2("Comparación de Pokémon"),
              fluidRow(
                column(6, textInput('pokemon1', 'Pokémon 1', value = 'bulbasaur')),
                column(6, textInput('pokemon2', 'Pokémon 2', value = 'charmander'))
              ),
              actionButton('compare', 'Comparar'),
              plotlyOutput('comparisonPlot') %>% withSpinner(color="#0dc5c1")),
      
      # Pestaña de Datos
      tabItem(tabName = "datos",
              h2("Datos Interactivos"),
              DTOutput('pokemonTable') %>% withSpinner(color="#0dc5c1"))
    )
  )
)
