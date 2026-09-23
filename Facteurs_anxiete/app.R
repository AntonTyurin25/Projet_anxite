library(shiny)

# Charger tous les fichiers R du dossier R/
source("R/page_accueil.R")
source("R/page_donnees.R")
source("R/page_analyse.R")

ui <- fluidPage(
  
  titlePanel("Mon application"),
  
  tabsetPanel(
    
    tabPanel(
      "Accueil",
      page_accueil_ui("accueil")
    ),
    
    tabPanel(
      "Données",
      page_donnees_ui("donnees")
    ),
    
    tabPanel(
      "Analyse",
      page_analyse_ui("analyse")
    )
  )
)

server <- function(input, output, session) {
  
  page_accueil_server("accueil")
  
  page_donnees_server("donnees")
  
  page_analyse_server("analyse")
}

shinyApp(ui, server)