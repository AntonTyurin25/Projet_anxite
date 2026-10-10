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
  
  tabBox(
    id = ns("tabs"),
    width = 12,
    
    # --------------------------------------------------
    # ONGLET 1 : DONNÉES
    # --------------------------------------------------
    tabPanel(
      title = "Données",
      
      box(
        width = 12,
        title = "Tableau des données",
        status = "primary",
        solidHeader = TRUE,
        
        div(
          style = "width: 100%; overflow-x: auto;",
          DTOutput(ns("Table_Donnees"))
        )
      )
    ),
    
    
    # --------------------------------------------------
    # ONGLET 2 : RÉSUMÉ
    # --------------------------------------------------
    tabPanel(
      title = "Résumé",
      
      box(
        width = 12,
        title = "Vue d'ensemble des données",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(
          valueBoxOutput(ns("nb_individus"), width = 3),
          valueBoxOutput(ns("age_moyen"), width = 3),
          valueBoxOutput(ns("anxiete_moyenne"), width = 3),
          valueBoxOutput(ns("anxiete_elevee"), width = 3)
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          box(
            width = 12,
            title = "Répartition par sexe",
            status = "primary",
            solidHeader = TRUE,
            plotOutput(ns("resume_sexe"), height = "300px")
          )
        ),
        
        column(
          width = 6,
          box(
            width = 12,
            title = "Niveau d'anxiété",
            status = "primary",
            solidHeader = TRUE,
            plotOutput(ns("resume_anxiete"), height = "300px")
          )
        )
      )
    ),
    
    
    # --------------------------------------------------
    # ONGLET 3 : DISTRIBUTION
    # --------------------------------------------------
    tabPanel(
      title = "Distribution",
      
      box(
        width = 12,
        title = "Distribution d'une variable",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(
          
          column(
            width = 4,
            
            selectInput(
              inputId = ns("var"),
              label = "Variable :",
              choices = NULL
            )
          )
          
        ),
        
        hr(),
        
        plotOutput(
          ns("distribution"),
          height = "500px"
        )
      )
    ),
    
    
    # --------------------------------------------------
    # ONGLET 4 : DESCRIPTION
    # --------------------------------------------------
    tabPanel(
      title = "Description des variables",
      
      box(
        width = 12,
        title = "Description des variables",
        status = "primary",
        solidHeader = TRUE,
        
        div(
          style = "width: 100%; overflow-x: auto;",
          DTOutput(ns("Description"))
        )
      )
    )
  )
}


page_donnees_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # --------------------------------------------------
    # TABLEAU DES DONNÉES
    # --------------------------------------------------
    
    output$Table_Donnees <- renderDT({
      
      donnees
      
    }, options = list(
      scrollX = TRUE,
      pageLength = 10,
      autoWidth = TRUE
    ))
    
    
    # --------------------------------------------------
    # RÉSUMÉ
    # --------------------------------------------------
    
    # Nombre d'individus
    output$nb_individus <- renderValueBox({
      valueBox(
        value = format(nrow(donnees), big.mark = " "),
        subtitle = "Individus",
        icon = icon("users"),
        color = "blue"
      )
    })

    # Âge moyen
    output$age_moyen <- renderValueBox({
      age <- donnees[["Âge"]]
      
      moyenne <- if (is.numeric(age) && any(!is.na(age))) {
        round(mean(age, na.rm = TRUE), 1)
      } else {
        "Non disponible"
      }
      
      valueBox(
        value = moyenne,
        subtitle = "Âge moyen (ans)",
        icon = icon("user"),
        color = "purple"
      )
    })
    
    # Niveau moyen d'anxiété
    output$anxiete_moyenne <- renderValueBox({
      
      anxiete <- donnees[["Niveau d'anxiété"]]
      
      moyenne <- if (is.numeric(anxiete) &&
                     any(!is.na(anxiete))) {
        round(mean(anxiete, na.rm = TRUE), 1)
      } else {
        NA_real_
      }
      
      valueBox(
        value = if (is.na(moyenne)) "N/D" else paste0(moyenne, "/10"),
        subtitle = "Niveau moyen d'anxiété",
        icon = icon("heartbeat"),
        color = "purple"
      )
    })
    
    
    # Pourcentage de niveaux d'anxiété élevés
    output$anxiete_elevee <- renderValueBox({
      
      anxiete <- donnees[["Niveau d'anxiété"]]
      
      if (!is.numeric(anxiete) || all(is.na(anxiete))) {
        resultat <- "N/D"
      } else {
        resultat <- paste0(
          round(mean(anxiete >= 8, na.rm = TRUE) * 100, 1),
          "%"
        )
      }
      
      valueBox(
        value = resultat,
        subtitle = "Anxiété élevée (score ≥ 8)",
        icon = icon("chart-line"),
        color = "yellow"
      )
    })
    
    # Répartition par sexe
    output$resume_sexe <- renderPlot({
      sexe <- donnees[["Sexe"]]
      
      req(!is.null(sexe))
      
      effectifs <- table(sexe, useNA = "no")
      req(length(effectifs) > 0)
      
      barplot(
        effectifs,
        col = "steelblue",
        border = "white",
        main = "",
        xlab = "Sexe",
        ylab = "Nombre d'individus",
        las = 1
      )
    })
    
    # Répartition du niveau d'anxiété
    output$resume_anxiete <- renderPlot({
      anxiete <- donnees[["Niveau d'anxiété"]]
      
      req(!is.null(anxiete))
      
      anxiete <- anxiete[!is.na(anxiete)]
      req(length(anxiete) > 0)
      
      if (is.numeric(anxiete)) {
        hist(
          anxiete,
          col = "steelblue",
          border = "white",
          main = "",
          xlab = "Niveau d'anxiété",
          ylab = "Nombre d'individus"
        )
      } else {
        effectifs <- table(anxiete)
        
        barplot(
          effectifs,
          col = "steelblue",
          border = "white",
          main = "",
          xlab = "Niveau d'anxiété",
          ylab = "Nombre d'individus",
          las = 1
        )
      }
    })
    
    
    # --------------------------------------------------
    # LISTE DES VARIABLES
    # --------------------------------------------------
    
    observe({
      
      updateSelectInput(
        session = session,
        inputId = "var",
        choices = names(donnees),
        selected = names(donnees)[1]
      )
      
    })
    
    
    # --------------------------------------------------
    # GRAPHIQUE DE DISTRIBUTION
    # --------------------------------------------------
    
    output$distribution <- renderPlot({
      
      req(input$var)
      
      x <- donnees[[input$var]]
      
      
      # Variable quantitative
      if (is.numeric(x)) {
        
        hist(
          x,
          col = "steelblue",
          border = "white",
          main = paste("Distribution", input$var),
          xlab = input$var,
          ylab = "Effectif"
        )
        
        
        # Variable qualitative
      } else {
        
        effectifs <- table(x, useNA = "ifany")
        
        barplot(
          effectifs,
          col = "steelblue",
          border = "white",
          main = paste("Répartition", input$var),
          xlab = input$var,
          ylab = "Effectif",
          las = 2
        )
        
      }
      
    })
    
    
    # --------------------------------------------------
    # DESCRIPTION DES VARIABLES
    # --------------------------------------------------
    
    output$Description <- renderDT({
      
      description_variables
      
    }, options = list(
      scrollX = TRUE,
      pageLength = 19,
      autoWidth = TRUE
    ))
    
  })
}

