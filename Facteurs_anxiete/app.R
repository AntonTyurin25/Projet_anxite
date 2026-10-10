library(shiny)
library(readr)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)
library(skimr)
library(DT)
library(GGally)
library(ggcorrplot)
library(car)
library(glmnet)
library(randomForest)
library(FactoMineR)
library(factoextra)
library(effectsize)
library(scales)
# un graphique fixe possible a mettre en pdf pour expliquer le choix et en quoi c'est interessant

donnees <- read_csv(
  "data/enhanced_anxiety_dataset.csv",
  show_col_types = FALSE
)
donnees <- donnees |>
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )
names(donnees) <- c(
  "Âge",
  "Sexe",
  "Profession",
  "Heures de sommeil",
  "Activité physique",
  "Caféine consommée",
  "Consommation d'alcool",
  "Tabagisme",
  "Antécédents familiaux d'anxiété",
  "Niveau de stress",
  "Fréquence cardiaque",
  "Fréquence respiratoire",
  "Niveau de transpiration",
  "Vertiges",
  "Médication",
  "Séances de thérapie",
  "Événement de vie récent",
  "Qualité de l'alimentation",
  "Niveau d'anxiété"
)
# Charger tous les fichiers R du dossier R/
source("R/page_accueil.R")
source("R/page_donnees.R")
source("R/page_structure.R")
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
          tags$a(icon("user"),"Anton Tyurin")
        ),
        
        tags$li(
          tags$a(icon("user"),"Augustin Barnerias")
        ),
        
        tags$li(
          tags$a(icon("user"),"Ines Bouzida")
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
        "Structure",
        tabName = "structure",
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
        tabName = "structure",
        page_structure_ui("structure")
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
  
  page_donnees_server(
    "donnees",
    donnees = donnees
  )
  
  page_structure_server(
    "structure",
    donnees = donnees
  )
  
  page_modele_server(
    "modele",
    donnees = donnees
  )
}

shinyApp(ui, server)