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
  
  Description = c(
    "Âge de l'individu (années)",
    "Homme/Femme/Autre",
    paste(
      "Artiste/Athlète/Avocat/Chef-cuisinier/",
      "Etudiant/Freelance/Infirmier/Ingénieur/",
      "Médecin/Musicien/Professeur/Scientifique/Autre",
      sep = ""
    ),
    "Heures de sommeil par nuit",
    "Heures d'activité physique par semaine",
    "Quantité de caféine ingérée (mg/jour)",
    "Quantité de verres d'alcool consommés par semaine",
    "L'individu fume : Oui/Non",
    "Des membres de la famille sont affectés par l'anxiété : Oui/Non",
    "De 1 à 10",
    "Nombre de battements par minute",
    "Nombre d'inspirations-expirations par minute",
    "De 1 à 5",
    "L'individu est atteint de vertiges : Oui/Non",
    "L'individu suit un traitement : Oui/Non",
    "Quantité de séances de thérapie par mois",
    "L'individu a vécu un événement majeur récemment : Oui/Non",
    "De 1 à 10",
    "De 1 à 10"
  ),
  
  stringsAsFactors = FALSE
)


# ==========================================================
# FONCTION POUR LES GRAPHIQUES
# ==========================================================

# Thème graphique commun à tous les graphiques
theme_graphique <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 19, face = "bold", hjust = 0
      ),
      axis.title.x = ggplot2::element_text(
        size = 16, face = "bold",
        margin = ggplot2::margin(t = 12)
      ),
      axis.title.y = ggplot2::element_text(
        size = 16, face = "bold",
        margin = ggplot2::margin(r = 12)
      ),
      axis.text = ggplot2::element_text(
        size = 13, colour = "black"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 15, r = 20, b = 25, l = 20
      )
    )
}


# Fonction pour compter les modalités d'une variable
compter_modalites <- function(x) {
  
  valeurs <- as.character(x)
  
  valeurs[is.na(valeurs)] <- "Valeurs manquantes"
  
  effectifs <- as.data.frame(
    table(valeurs),
    stringsAsFactors = FALSE
  )
  
  names(effectifs) <- c("Modalite", "Effectif")
  
  effectifs <- effectifs |>
    arrange(desc(Effectif))
  
  effectifs
}


# Fonction pour rendre un graphique interactif
rendre_interactif <- function(graphique) {
  
  ggplotly(
    graphique,
    tooltip = "text"
  ) |>
    layout(
      hoverlabel = list(
        font = list(size = 14)
      ),
      
      margin = list(
        l = 75,
        r = 25,
        t = 45,
        b = 80
      )
    ) |>
    config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}


# ==========================================================
# INTERFACE UTILISATEUR
# ==========================================================

page_donnees_ui <- function(id) {
  
  ns <- NS(id)
  
  tabBox(
    id = ns("tabs"),
    width = 12,
    
    # ------------------------------------------------------
    # ONGLET 1 : RÉSUMÉ
    # ------------------------------------------------------
    
    tabPanel(
      title = "Résumé",
      
      box(
        width = 12,
        title = "Vue d'ensemble des données",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(
          valueBoxOutput(
            ns("nb_individus"),
            width = 3
          ),
          
          valueBoxOutput(
            ns("age_moyen"),
            width = 3
          ),
          
          valueBoxOutput(
            ns("anxiete_moyenne"),
            width = 3
          ),
          
          valueBoxOutput(
            ns("anxiete_elevee"),
            width = 3
          )
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
            
            plotlyOutput(
              ns("resume_sexe"),
              height = "350px"
            )
          )
        ),
        
        column(
          width = 6,
          
          box(
            width = 12,
            title = "Répartition des professions",
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              ns("resume_professions"),
              height = "400px"
            )
          )
        )
      )
    ),
    
    
    # ------------------------------------------------------
    # ONGLET 2 : DISTRIBUTION
    # ------------------------------------------------------
    
    tabPanel(
      title = "Distribution",
      
      box(
        width = 12,
        title = "Distribution d'une variable",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(
          
          column(
            width = 6,
            
            selectInput(
              inputId = ns("var"),
              label = "Variable à étudier :",
              choices = NULL
            )
          )
        ),
        
        hr(),
        
        plotlyOutput(
          ns("distribution"),
          height = "550px"
        )
      )
    ),
    
    
    # ------------------------------------------------------
    # ONGLET 3 : DESCRIPTION DES VARIABLES
    # ------------------------------------------------------
    
    tabPanel(
      title = "Description des variables",
      
      box(
        width = 12,
        title = "Description des variables",
        status = "primary",
        solidHeader = TRUE,
        
        div(
          style = "width: 100%; overflow-x: auto;",
          
          DTOutput(
            ns("Description")
          )
        )
      )
    ),
    
    
    # ------------------------------------------------------
    # ONGLET 4 : DONNÉES (EN DERNIER)
    # ------------------------------------------------------
    
    tabPanel(
      title = "Données",
      
      box(
        width = 12,
        title = "Tableau des données",
        status = "primary",
        solidHeader = TRUE,
        
        div(
          style = "width: 100%; overflow-x: auto;",
          
          DTOutput(
            ns("Table_Donnees")
          )
        )
      )
    )
  )
}


# ==========================================================
# SERVEUR
# ==========================================================

page_donnees_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # ------------------------------------------------------
    # TABLEAU DES DONNÉES
    # ------------------------------------------------------
    
    output$Table_Donnees <- renderDT({
      
      donnees
      
    }, options = list(
      scrollX = TRUE,
      pageLength = 10,
      autoWidth = TRUE
    ))
    
    
    # ------------------------------------------------------
    # VUE D'ENSEMBLE : NOMBRE D'INDIVIDUS
    # ------------------------------------------------------
    
    output$nb_individus <- renderValueBox({
      
      valueBox(
        value = format(
          nrow(donnees),
          big.mark = " "
        ),
        
        subtitle = "Individus",
        
        icon = icon("users"),
        
        color = "blue"
      )
    })
    
    
    # ------------------------------------------------------
    # VUE D'ENSEMBLE : ÂGE MOYEN
    # ------------------------------------------------------
    
    output$age_moyen <- renderValueBox({
      
      age <- donnees[["Âge"]]
      
      moyenne <- if (
        is.numeric(age) &&
        any(!is.na(age))
      ) {
        round(mean(age, na.rm = TRUE), 1)
      } else {
        NA_real_
      }
      
      valueBox(
        value = if (is.na(moyenne)) {
          "N/D"
        } else {
          moyenne
        },
        
        subtitle = "Âge moyen (ans)",
        
        icon = icon("user"),
        
        color = "purple"
      )
    })
    
    
    # ------------------------------------------------------
    # VUE D'ENSEMBLE : ANXIÉTÉ MOYENNE
    # ------------------------------------------------------
    
    output$anxiete_moyenne <- renderValueBox({
      
      anxiete <- donnees[["Niveau d'anxiété"]]
      
      moyenne <- if (
        is.numeric(anxiete) &&
        any(!is.na(anxiete))
      ) {
        round(mean(anxiete, na.rm = TRUE), 1)
      } else {
        NA_real_
      }
      
      valueBox(
        value = if (is.na(moyenne)) {
          "N/D"
        } else {
          paste0(moyenne, "/10")
        },
        
        subtitle = "Niveau moyen d'anxiété",
        
        icon = icon("brain"),
        
        color = "fuchsia"
      )
    })
    
    
    # ------------------------------------------------------
    # VUE D'ENSEMBLE : ANXIÉTÉ ÉLEVÉE
    # ------------------------------------------------------
    
    output$anxiete_elevee <- renderValueBox({
      
      anxiete <- donnees[["Niveau d'anxiété"]]
      
      resultat <- if (
        !is.numeric(anxiete) ||
        all(is.na(anxiete))
      ) {
        "N/D"
      } else {
        paste0(
          round(
            mean(anxiete >= 8, na.rm = TRUE) * 100,
            1
          ),
          "%"
        )
      }
      
      valueBox(
        value = resultat,
        
        subtitle = "Anxiété élevée (score ≥ 8)",
        
        icon = icon("gauge"),
        
        color = "yellow"
      )
    })
    
    
    # ------------------------------------------------------
    # RÉSUMÉ : RÉPARTITION PAR SEXE
    # ------------------------------------------------------
    
    output$resume_sexe <- renderPlotly({
      
      sexe <- donnees[["Sexe"]]
      
      req(!is.null(sexe))
      
      effectifs <- compter_modalites(sexe)
      
      graphique <- ggplot(
        effectifs,
        aes(
          x = reorder(Modalite, -Effectif),
          y = Effectif,
          text = paste0(
            "Sexe : ", Modalite,
            "<br>Effectif : ", Effectif
          )
        )
      ) +
        
        geom_col(
          fill = "steelblue",
          width = 0.70
        ) +
        
        labs(
          x = "Sexe",
          y = "Nombre d'individus"
        ) +
        
        theme_graphique() +
        
        theme(
          axis.text.x = element_text(
            angle = 0,
            hjust = 0.5
          )
        )
      
      rendre_interactif(graphique)
    })
    
    
    # ------------------------------------------------------
    # RÉSUMÉ : RÉPARTITION DES PROFESSIONS
    # ------------------------------------------------------
    
    output$resume_professions <- renderPlotly({
      
      profession <- donnees[["Profession"]]
      
      req(!is.null(profession))
      
      effectifs <- compter_modalites(profession) |>
        arrange(Effectif)
      
      graphique <- ggplot(
        effectifs,
        aes(
          x = Effectif,
          y = reorder(Modalite, Effectif),
          text = paste0(
            "Profession : ", Modalite,
            "<br>Effectif : ", Effectif
          )
        )
      ) +
        
        geom_col(
          fill = "steelblue",
          width = 0.70
        ) +
        
        labs(
          x = "Nombre d'individus",
          y = NULL
        ) +
        
        theme_graphique() +
        
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 12),
          
          axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(t = 15)
          )
        )
      
      rendre_interactif(graphique)
    })
    
    
    # ------------------------------------------------------
    # LISTE DES VARIABLES
    # ------------------------------------------------------
    
    observe({
      
      updateSelectInput(
        session = session,
        inputId = "var",
        choices = names(donnees),
        selected = "Niveau d'anxiété"
      )
      
    })
    
    
    # ------------------------------------------------------
    # GRAPHIQUE DE DISTRIBUTION
    # ------------------------------------------------------
    
    output$distribution <- renderPlotly({
      
      req(input$var)
      
      x <- donnees[[input$var]]
      
      nom_variable <- input$var
      
      
      # ----------------------------------------------------
      # VARIABLES QUANTITATIVES ORDINALES
      #
      # Les scores entiers de stress et d'anxiété sont
      # représentés par des barres distinctes.
      # Cela évite de coller les barres de l'histogramme.
      # ----------------------------------------------------
      
      variables_ordinales <- c(
        "Niveau de stress",
        "Niveau d'anxiété",
        "Niveau de transpiration",
        "Qualité de l'alimentation"
      )
      
      if (
        is.numeric(x) &&
        nom_variable %in% variables_ordinales
      ) {
        
        valeurs <- x[!is.na(x)]
        
        req(length(valeurs) > 0)
        
        effectifs <- as.data.frame(
          table(valeurs),
          stringsAsFactors = FALSE
        )
        
        names(effectifs) <- c(
          "Modalite",
          "Effectif"
        )
        
        effectifs$Modalite <- as.numeric(
          as.character(effectifs$Modalite)
        )
        
        effectifs <- effectifs |>
          arrange(Modalite)
        
        graphique <- ggplot(
          effectifs,
          aes(
            x = factor(Modalite),
            y = Effectif,
            text = paste0(
              nom_variable, " : ", Modalite,
              "<br>Effectif : ", Effectif
            )
          )
        ) +
          
          geom_col(
            fill = "steelblue",
            width = 0.70
          ) +
          
          labs(
            title = paste(
              "Répartition de", nom_variable
            ),
            x = nom_variable,
            y = "Effectif"
          ) +
          
          theme_graphique()
        
        
        # ----------------------------------------------------
        # AUTRES VARIABLES QUANTITATIVES
        # ----------------------------------------------------
        
      } else if (is.numeric(x)) {
        
        valeurs <- x[!is.na(x)]
        
        req(length(valeurs) > 0)
        
        graphique <- ggplot(
          data.frame(Valeur = valeurs),
          aes(
            x = Valeur,
            text = after_stat(
              paste0(
                "Intervalle : ",
                round(xmin, 2),
                " à ",
                round(xmax, 2),
                "<br>Effectif : ",
                count
              )
            )
          )
        ) +
          
          geom_histogram(
            aes(y = after_stat(count)),
            bins = 30,
            fill = "steelblue",
            colour = "white",
            closed = "right"
          ) +
          
          labs(
            title = paste(
              "Distribution de", nom_variable
            ),
            x = nom_variable,
            y = "Effectif"
          ) +
          
          theme_graphique()
        
        
        # ----------------------------------------------------
        # VARIABLES QUALITATIVES
        # ----------------------------------------------------
        
      } else {
        
        effectifs <- compter_modalites(x) |>
          arrange(Effectif)
        
        graphique <- ggplot(
          effectifs,
          aes(
            x = Effectif,
            y = reorder(Modalite, Effectif),
            text = paste0(
              nom_variable, " : ", Modalite,
              "<br>Effectif : ", Effectif
            )
          )
        ) +
          
          geom_col(
            fill = "steelblue",
            width = 0.70
          ) +
          
          labs(
            title = paste(
              "Répartition de", nom_variable
            ),
            x = "Effectif",
            y = nom_variable
          ) +
          
          theme_graphique() +
          
          theme(
            axis.text.y = element_text(size = 12)
          )
      }
      
      
      # Affichage interactif avec infobulles
      rendre_interactif(graphique)
      
    })
    
    
    # ------------------------------------------------------
    # DESCRIPTION DES VARIABLES
    # ------------------------------------------------------
    
    output$Description <- renderDT({
      
      description_variables
      
    }, options = list(
      scrollX = TRUE,
      pageLength = 19,
      autoWidth = TRUE
    ))
    
  })
}
