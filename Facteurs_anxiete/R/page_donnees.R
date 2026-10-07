library(readr)
library(dplyr)
donnees <- read_csv("data/enhanced_anxiety_dataset.csv")
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

description_variables <- data.frame(
  Variable = names(donnees),
  Type = sapply(donnees, function(x) {
    if (is.numeric(x)) {
      "Quantitative"
    } else {
      "Qualitative"
    }
  }),
  Description = c("Âge de l'individu (années)",
                  "Homme/Femme/Autre",
                  "",
                  "Heures de sommeil par nuit",
                  "Heures d'activité physique par semaine",
                  "Quantité de caféine ingérée (mg/jour)",
                  "Quantité de verres d'alcool consommés par semaine",
                  "L'individu fume: Oui/Non",
                  "Des membres de la famille sont affectés par l'anxiété: Oui/Non",
                  "De 1 à 10",
                  "Nombre de battements par minute",
                  "Nombre d'inspirations-expirations par minute",
                  "De 1 à 5",
                  "L'individu est atteint de vertiges: Oui/Non",
                  "L'individu suit un traitement: Oui/Non",
                  "Quantité de séances de thérapie par mois",
                  "L'individu a vécu un événement majeur récemment: Oui/Non",
                  "De 1 à 10",
                  "De 1 à 10"),
  stringsAsFactors = FALSE
)

page_donnees_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(
        width = 8,
        div(
          style = "width: 100%; overflow-x: auto; overflow-y: auto",
          DTOutput(ns("Table_Donnees"))
        )
      ),
      
      column(
        width = 4,
        div(
          style = "height: 500px; overflow-y: auto;",
          verbatimTextOutput(ns("skim"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("var"),
          label = "Variable :",
          choices = NULL
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        plotOutput(ns("distribution"))
      ),
      
      column(
        width = 6,
        DTOutput(ns("Description"))
      )
    )
  )
}


page_donnees_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    output$Table_Donnees <- renderDT({
      donnees
    }, options = list(
      scrollX = TRUE, scrollY = TRUE
    ))
    
    
    output$skim <- renderPrint({
      skim(donnees)
    })
    
    
    observe({
      
      
      updateSelectInput(
        session = session,
        inputId = "var",
        choices = names(donnees)
      )
      
    })
    
    
    output$distribution <- renderPlot({
      
      req(input$var)
      
      x <- donnees[[input$var]]
      
      if (is.numeric(x)) {
        
        hist(
          x,
          col = "steelblue",
          main = paste("Distribution de", input$var),
          xlab = input$var,
          ylab = "Effectif"
        )
        
      } else {
        
        effectifs <- table(x, useNA = "ifany")
        
        barplot(
          effectifs,
          col = "steelblue",
          main = paste("Répartition de", input$var),
          xlab = input$var,
          ylab = "Effectif",
          las = 2
        )
        
      }
      
    })
    
    output$Description <- renderDT({
      description_variables
    }, options = list(
      scrollX = TRUE, scrollY = TRUE
    ))
    
  })
}