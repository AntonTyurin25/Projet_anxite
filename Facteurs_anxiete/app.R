library(shiny)
library(readr)
donnees <- read_csv("data/enhanced_anxiety_dataset.csv")
# Charger tous les fichiers R du dossier R/
source("R/page_accueil.R")
source("R/page_donnees.R")
source("R/page_analyse.R")
source("R/page_modele.R")

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
    )    ,
    
    tabPanel(
      "Modele",
      page_analyse_ui("modele")
    )
  )
)

server <- function(input, output, session) {
  
  page_accueil_server("accueil")
  
  page_donnees_server("donnees",
                      donnees = donnees)
  
  page_analyse_server("analyse",
                      donnees = donnees)
  
  page_donnees_server("modele",
                      donnees = donnees)
}

shinyApp(ui, server)