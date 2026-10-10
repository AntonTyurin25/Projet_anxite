# ============================================================
# PAGE MODELISATION
# ============================================================


# ============================================================
# UI
# ============================================================

page_modele_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # --------------------------------------------------------
    # INTRODUCTION
    # --------------------------------------------------------
    
    fluidRow(
      
      box(
        width = 12,
        title = "Estimation du niveau d'anxiété",
        status = "primary",
        solidHeader = TRUE,
        
        p(
          "Renseignez votre profil pour obtenir une estimation
          du niveau d'anxiété associé à des profils similaires
          dans notre jeu de données."
        ),
        
        p(
          "Le modèle utilisé est une régression linéaire multiple
          construite à partir de neuf caractéristiques facilement
          renseignables."
        )
      )
    ),
    
    
    # ========================================================
    # PROFIL UTILISATEUR
    # ========================================================
    
    fluidRow(
      
      box(
        width = 5,
        title = "Votre profil",
        status = "primary",
        solidHeader = TRUE,
        
        p("Valeurs initiales : moyennes observées dans notre jeu de données "
          , "(et modalités les plus fréquentes pour les réponses Oui/Non)."),
        
        
        # ----------------------------------------------------
        # AGE
        # ----------------------------------------------------
        
        sliderInput(
          ns("age"),
          "Âge",
          min = 0,
          max = 100,
          value = 25,
          step = 1
        ),
        
        uiOutput(
          ns("info_age")
        ),
        
        
        # ----------------------------------------------------
        # SOMMEIL
        # ----------------------------------------------------
        
        sliderInput(
          ns("sommeil"),
          "Heures de sommeil par nuit",
          min = 0,
          max = 15,
          value = 7,
          step = 0.5
        ),
        
        uiOutput(
          ns("info_sommeil")
        ),
        
        
        # ----------------------------------------------------
        # ACTIVITE PHYSIQUE
        # ----------------------------------------------------
        
        numericInput(
          ns("activite"),
          "Activité physique (heures/semaine)",
          value = 3,
          min = 0,
          max = 50,
          step = 0.5
        ),
        
        uiOutput(
          ns("info_activite")
        ),
        
        
        # ----------------------------------------------------
        # CAFEINE
        # ----------------------------------------------------
        
        sliderInput(
          ns("cafeine"),
          "Caféine consommée (mg/jour)",
          min = 0,
          max = 1000,
          value = 200,
          step = 10
        ),
        
        helpText(
          icon("mug-hot"),
          "Repères approximatifs : espresso ≈ 60–80 mg •
          café filtre ≈ 80–120 mg/tasse •
          thé ≈ 20–50 mg •
          boisson énergisante (250 mL) ≈ 80 mg."
        ),
        
        uiOutput(
          ns("info_cafeine")
        ),
        
        
        # ----------------------------------------------------
        # ALCOOL
        # ----------------------------------------------------
        
        sliderInput(
          ns("alcool"),
          "Consommation d'alcool (verres/semaine)",
          min = 0,
          max = 50,
          value = 5,
          step = 1
        ),
        
        uiOutput(
          ns("info_alcool")
        ),
        
        
        # ----------------------------------------------------
        # ALIMENTATION
        # ----------------------------------------------------
        
        sliderInput(
          ns("alimentation"),
          "Qualité de l'alimentation",
          min = 1,
          max = 10,
          value = 7,
          step = 1
        ),
        
        
        # ----------------------------------------------------
        # STRESS
        # ----------------------------------------------------
        
        sliderInput(
          ns("stress"),
          "Niveau de stress",
          min = 1,
          max = 10,
          value = 5,
          step = 1
        ),
        
        
        # ----------------------------------------------------
        # TABAGISME
        # ----------------------------------------------------
        
        radioButtons(
          ns("tabagisme"),
          "Tabagisme",
          choices = c(
            "Non" = "No",
            "Oui" = "Yes"
          ),
          selected = "No",
          inline = TRUE
        ),
        
        
        # ----------------------------------------------------
        # EVENEMENT DE VIE
        # ----------------------------------------------------
        
        radioButtons(
          ns("evenement"),
          "Événement de vie majeur récent",
          choices = c(
            "Non" = "No",
            "Oui" = "Yes"
          ),
          selected = "No",
          inline = TRUE
        ),
        
        
        br(),
        
        actionButton(
          ns("predire"),
          "Estimer mon niveau d'anxiété",
          icon = icon("calculator"),
          class = "btn-primary",
          width = "100%"
        )
      ),
      
      
      # ======================================================
      # RESULTAT
      # ======================================================
      
      box(
        width = 7,
        title = "Résultat",
        status = "primary",
        solidHeader = TRUE,
        
        br(),
        
        h4(
          "Niveau d'anxiété estimé",
          style = "text-align:center;"
        ),
        
        
        # Score
        uiOutput(
          ns("prediction")
        ),
        
        
        br(),
        
        
        # Jauge
        plotOutput(
          ns("jauge"),
          height = "250px"
        ),
        
        
        # Avertissement extrapolation
        uiOutput(
          ns("alerte_extrapolation")
        ),
        
        
        br(),
        
        
        # Interprétation
        uiOutput(
          ns("interpretation")
        )
      )
    ),
    
    
    # ========================================================
    # PERFORMANCE DU MODELE
    # ========================================================
    
    # ========================================================
    # PERFORMANCE DU MODELE
    # ========================================================
    
    fluidRow(
      column(
        width = 6,
        offset = 3,
        
        valueBox(
          value = "0,666",
          subtitle = "R² sur les données de test",
          icon = icon("chart-line"),
          color = "green",
          width = 12
        )
      )
    ),
    
    fluidRow(
      column(
        width = 10,
        offset = 1,
        
        tags$p(
          "Le R² mesure la capacité du modèle à expliquer les variations du niveau d'anxiété. ",
          "Ici, le modèle explique environ 66,6 % de la variabilité du niveau d'anxiété ",
          "sur les données de test.",
          style = "text-align: center; color: #555; font-size: 15px; margin-top: 10px;"
        )
      )
    ),
    # ========================================================
    # EXPLICATION DU MODELE
    # ========================================================
    
    fluidRow(
      
      box(
        width = 12,
        title = "Comment fonctionne le modèle ?",
        status = "primary",
        solidHeader = TRUE,
        
        p(
          "Le modèle utilisé est une régression linéaire multiple.
          Il a été retenu afin d'obtenir un compromis entre
          performance prédictive, interprétabilité et simplicité
          d'utilisation."
        ),
        
        p(
          "Les neuf informations utilisées sont : l'âge, le sommeil,
          l'activité physique, la consommation de caféine,
          la consommation d'alcool, la qualité de l'alimentation,
          le niveau de stress, le tabagisme et la survenue récente
          d'un événement de vie majeur."
        ),
        
        p(
          strong("Performance sur le jeu test : "),
          "R² = 0,666"
        ),
        
        p(
          strong("Attention : "),
          "cette estimation est produite à partir du jeu de données
          étudié dans le cadre de ce projet. Elle ne constitue pas
          un diagnostic médical."
        )
      )
    )
  )
}

# ============================================================


# ============================================================
# SERVER
# ============================================================

page_modele_server <- function(id, donnees) {
  
  moduleServer(
    
    id,
    
    function(input, output, session) {moyenne <- function(nom) {
      mean(donnees[[nom]], na.rm = TRUE)
    }
    
    modalite_frequente <- function(nom) {
      x <- na.omit(as.character(donnees[[nom]]))
      names(which.max(table(x)))
    }
    
    valeurs_initiales <- list(
      age = round(moyenne("Âge")),
      sommeil = round(moyenne("Heures de sommeil") * 2) / 2,
      activite = round(moyenne("Activité physique"), 1),
      cafeine = round(moyenne("Caféine consommée") / 10) * 10,
      alcool = round(moyenne("Consommation d'alcool")),
      alimentation = round(moyenne("Qualité de l'alimentation")),
      stress = round(moyenne("Niveau de stress")),
      tabagisme = modalite_frequente("Tabagisme"),
      evenement = modalite_frequente("Événement de vie récent")
    )
    
    observeEvent(TRUE, {
      updateSliderInput(session, "age", value = valeurs_initiales$age)
      updateSliderInput(session, "sommeil", value = valeurs_initiales$sommeil)
      updateNumericInput(session, "activite", value = valeurs_initiales$activite)
      updateSliderInput(session, "cafeine", value = valeurs_initiales$cafeine)
      updateSliderInput(session, "alcool", value = valeurs_initiales$alcool)
      updateSliderInput(session, "alimentation", value = valeurs_initiales$alimentation)
      updateSliderInput(session, "stress", value = valeurs_initiales$stress)
      updateRadioButtons(session, "tabagisme", selected = valeurs_initiales$tabagisme)
      updateRadioButtons(session, "evenement", selected = valeurs_initiales$evenement)
    }, once = TRUE)
    
    
    # ======================================================
    # 1. MODELE
    # ======================================================
    
    modele <- reactive({
      
      lm(
        `Niveau d'anxiété` ~
          Âge +
          `Heures de sommeil` +
          `Activité physique` +
          `Caféine consommée` +
          `Consommation d'alcool` +
          `Qualité de l'alimentation` +
          `Niveau de stress` +
          Tabagisme +
          `Événement de vie récent`,
        
        data = donnees
      )
    })
    
    
    
    
    # ======================================================
    # 2. BORNES REELLEMENT OBSERVEES DANS LE DATASET
    # ======================================================
    
    borne_age <- range(
      donnees$Âge,
      na.rm = TRUE
    )
    
    borne_sommeil <- range(
      donnees$`Heures de sommeil`,
      na.rm = TRUE
    )
    
    borne_activite <- range(
      donnees$`Activité physique`,
      na.rm = TRUE
    )
    
    borne_cafeine <- range(
      donnees$`Caféine consommée`,
      na.rm = TRUE
    )
    
    borne_alcool <- range(
      donnees$`Consommation d'alcool`,
      na.rm = TRUE
    )
    
    
    # ======================================================
    # 3. AFFICHAGE DES BORNES DU DATASET
    # ======================================================
    
    output$info_age <- renderUI({
      
      helpText(
        paste0(
          "Dans notre jeu de données : ",
          round(borne_age[1], 0),
          " à ",
          round(borne_age[2], 0),
          " ans"
        )
      )
    })
    
    
    output$info_sommeil <- renderUI({
      
      helpText(
        paste0(
          "Dans notre jeu de données : ",
          round(borne_sommeil[1], 1),
          " à ",
          round(borne_sommeil[2], 1),
          " h/nuit"
        )
      )
    })
    
    
    output$info_activite <- renderUI({
      
      helpText(
        paste0(
          "Dans notre jeu de données : ",
          round(borne_activite[1], 1),
          " à ",
          round(borne_activite[2], 1),
          " h/semaine"
        )
      )
    })
    
    
    output$info_cafeine <- renderUI({
      
      helpText(
        paste0(
          "Dans notre jeu de données : ",
          round(borne_cafeine[1], 0),
          " à ",
          round(borne_cafeine[2], 0),
          " mg/jour"
        )
      )
    })
    
    
    output$info_alcool <- renderUI({
      
      helpText(
        paste0(
          "Dans notre jeu de données : ",
          round(borne_alcool[1], 0),
          " à ",
          round(borne_alcool[2], 0),
          " verres/semaine"
        )
      )
    })
    
    
    # ======================================================
    # 4. PROFIL RENSEIGNE PAR L'UTILISATEUR
    # ======================================================
    
    nouveau_profil <- eventReactive(
      input$predire,
      {
        v <- if (input$predire == 0) {
          valeurs_initiales
        } else {
          list(
            age = input$age,
            sommeil = input$sommeil,
            activite = input$activite,
            cafeine = input$cafeine,
            alcool = input$alcool,
            alimentation = input$alimentation,
            stress = input$stress,
            tabagisme = input$tabagisme,
            evenement = input$evenement
          )
        }
        
        data.frame(
          `Âge` = v$age,
          `Heures de sommeil` = v$sommeil,
          `Activité physique` = v$activite,
          `Caféine consommée` = v$cafeine,
          `Consommation d'alcool` = v$alcool,
          `Qualité de l'alimentation` = v$alimentation,
          `Niveau de stress` = v$stress,
          
          Tabagisme = factor(
            v$tabagisme,
            levels = levels(donnees$Tabagisme)
          ),
          
          `Événement de vie récent` = factor(
            v$evenement,
            levels = levels(donnees$`Événement de vie récent`)
          ),
          
          check.names = FALSE
        )
      },
      ignoreNULL = FALSE
    )
    # ======================================================
    # 5. PREDICTION
    # ======================================================
    
    prediction <- reactive({
      
      pred <- predict(
        modele(),
        newdata = nouveau_profil()
      )
      
      pred <- as.numeric(pred)
      
      
      # Le niveau d'anxiété du dataset est compris entre 1 et 10
      
      pred <- max(
        1,
        min(
          10,
          pred
        )
      )
      
      pred
    })
    
    
    # ======================================================
    # 6. SCORE
    # ======================================================
    
    output$prediction <- renderUI({
      
      req(prediction())
      
      score <- round(
        prediction(),
        1
      )
      
      
      div(
        
        style = "
          text-align: center;
          font-size: 60px;
          font-weight: bold;
          margin-top: 20px;
          margin-bottom: 20px;
          ",
        
        paste0(
          score,
          " / 10"
        )
      )
    })
    
    
    # ======================================================
    # 7. JAUGE
    # ======================================================
    
    output$jauge <- renderPlot({
      
      req(prediction())
      
      score <- as.numeric(prediction())
      
      # Données pour les 10 niveaux
      jauge_data <- data.frame(
        niveau = 1:10,
        y = rep(1, 10)
      )
      
      # Jauge colorée
      ggplot(jauge_data, aes(x = niveau, y = y)) +
        
        geom_tile(
          aes(fill = niveau),
          width = 0.98,
          height = 0.45
        ) +
        
        scale_fill_gradientn(
          colours = c("#5CB85C", "#F0AD4E", "#D9534F"),
          limits = c(1, 10)
        ) +
        
        # Trait noir indiquant le score
        geom_vline(
          xintercept = score,
          color = "#222222",
          linewidth = 1.5
        ) +
        
        # Point blanc indiquant le score
        geom_point(
          data = data.frame(score = score, y = 1),
          mapping = aes(x = score, y = y),
          inherit.aes = FALSE,
          shape = 21,
          size = 5,
          fill = "white",
          color = "#222222",
          stroke = 1.5
        ) +
        
        scale_x_continuous(
          breaks = 1:10,
          limits = c(0.5, 10.5)
        ) +
        
        coord_cartesian(
          ylim = c(0.6, 1.4)
        ) +
        
        labs(
          x = "Niveau d'anxiété estimé",
          y = NULL
        ) +
        
        theme_minimal() +
        
        theme(
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank()
        )
      
    })
    # ======================================================
    # 8. DETECTION D'UNE EXTRAPOLATION
    # ======================================================
    extrapolation <- reactive({
      
      # Profil réellement utilisé pour la prédiction
      profil <- nouveau_profil()
      
      problemes <- character(0)
      
      if (
        profil$Âge < borne_age[1] ||
        profil$Âge > borne_age[2]
      ) {
        problemes <- c(problemes, "âge")
      }
      
      if (
        profil$`Heures de sommeil` < borne_sommeil[1] ||
        profil$`Heures de sommeil` > borne_sommeil[2]
      ) {
        problemes <- c(problemes, "sommeil")
      }
      
      if (
        profil$`Activité physique` < borne_activite[1] ||
        profil$`Activité physique` > borne_activite[2]
      ) {
        problemes <- c(problemes, "activité physique")
      }
      
      if (
        profil$`Caféine consommée` < borne_cafeine[1] ||
        profil$`Caféine consommée` > borne_cafeine[2]
      ) {
        problemes <- c(problemes, "consommation de caféine")
      }
      
      if (
        profil$`Consommation d'alcool` < borne_alcool[1] ||
        profil$`Consommation d'alcool` > borne_alcool[2]
      ) {
        problemes <- c(problemes, "consommation d'alcool")
      }
      
      problemes
    })
    
    
    # ======================================================
    # 9. AVERTISSEMENT SI HORS DATASET
    # ======================================================
    
    output$alerte_extrapolation <- renderUI({
      
      
      problemes <- extrapolation()
      
      
      if (length(problemes) == 0) {
        
        return(NULL)
        
      }
      
      
      div(
        
        class = "alert alert-warning",
        
        icon("triangle-exclamation"),
        
        strong(
          " Attention : "
        ),
        
        paste0(
          "certaines valeurs renseignées sont en dehors des ",
          "valeurs observées dans notre jeu de données : ",
          paste(
            problemes,
            collapse = ", "
          ),
          ". La prédiction est donc une extrapolation et doit ",
          "être interprétée avec davantage de prudence."
        )
      )
    })
    
    
    # ======================================================
    # 10. INTERPRETATION
    # ======================================================
    
    output$interpretation <- renderUI({
      
      req(prediction())
      
      score <- prediction()
      
      
      if (score < 4) {
        
        texte <- paste0(
          "Dans notre jeu de données, ce profil est associé ",
          "à un niveau d'anxiété relativement faible."
        )
        
        
      } else if (score < 8) {
        
        texte <- paste0(
          "Dans notre jeu de données, ce profil est associé ",
          "à un niveau d'anxiété intermédiaire."
        )
        
        
      } else {
        
        texte <- paste0(
          "Dans notre jeu de données, ce profil se rapproche ",
          "des individus présentant les niveaux d'anxiété ",
          "les plus élevés."
        )
      }
      
      
      wellPanel(
        
        h4(
          icon("circle-info"),
          " Interprétation"
        ),
        
        p(texte),
        
        p(
          strong(
            "Cette estimation ne constitue pas un diagnostic médical."
          )
        )
      )
    })
    }
  )
}
