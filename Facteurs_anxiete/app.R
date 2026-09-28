library(shiny)
library(readr)
library(tidyverse)
library(shinydashboard)
library(skimr)
donnees <- read_csv("data/enhanced_anxiety_dataset.csv")
donnees <- donnees |>
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )
# Charger tous les fichiers R du dossier R/
source("R/page_accueil.R")
source("R/page_donnees.R")
source("R/page_analyse.R")
source("R/page_modele.R")

ui <- dashboardPage(
  
  # --------------------------------------------------
  # HEADER
  # --------------------------------------------------
  
  dashboardHeader(
    
    title = tags$img(
      src = "logo.png",
      height = "45px",
      style = "margin-top: 2px;"
    )
    ,
    
    titleWidth = 230,
    tags$li(
      class = "dropdown",
      
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        `data-toggle` = "dropdown",
        
        icon("users"),
        
        span(
          class = "label label-primary",
          "3"
        )
      ),
      
      tags$ul(
        class = "dropdown-menu",
        
        tags$li(
          tags$h4("Créateurs")
        ),
        
        tags$li(
          tags$a(icon("fa-solid fa-user"),"Anton Tyurin")
        ),
        
        tags$li(
          tags$a(icon("fa-solid fa-user"),"Augustin Barnerias")
        ),
        
        tags$li(
          tags$a(icon("fa-solid fa-user"),"Ines Bouzida")
        )
      )
    )
  )
  ,
  
  
  # --------------------------------------------------
  # MENU GAUCHE
  # --------------------------------------------------
  
  dashboardSidebar(
    collapsed = F,
    sidebarMenu(
      
      menuItem(
        "Accueil",
        tabName = "accueil",
        icon = icon("house")
      ),
      
      menuItem(
        "Données",
        tabName = "donnees",
        icon = icon("database")
      ),
      
      menuItem(
        "Analyse",
        tabName = "analyse",
        icon = icon("chart-column")
      ),
      
      menuItem(
        "Modélisation",
        tabName = "modele",
        icon = icon("chart-line")
      )
      
    )
  ),
  
  
  # --------------------------------------------------
  # CONTENU
  # --------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      )
    ),
    
    tabItems(
      
      tabItem(
        tabName = "accueil",
        page_accueil_ui("accueil")
      ),
      
      tabItem(
        tabName = "donnees",
        page_donnees_ui("donnees")
      ),
      
      tabItem(
        tabName = "analyse",
        page_analyse_ui("analyse")
      ),
      
      tabItem(
        tabName = "modele",
        page_modele_ui("modele")
      )
      
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