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
        
        
        # ----------------------------------------------------
        # AGE
        # ----------------------------------------------------
        
        sliderInput(
          ns("age"),
          "Âge",
          min = 18,
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
        
        
        # ----------------------------------------------------
        # BOUTON PREDICTION
        # ----------------------------------------------------
        
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
    
    fluidRow(
      
      valueBox(
        value = "0,666",
        subtitle = "R² sur les données test",
        icon = icon("chart-line"),
        color = "blue",
        width = 4
      ),
      
      valueBox(
        value = "1,225",
        subtitle = "RMSE",
        icon = icon("bullseye"),
        color = "green",
        width = 4
      ),
      
      valueBox(
        value = "0,966",
        subtitle = "MAE",
        icon = icon("chart-bar"),
        color = "yellow",
        width = 4
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
          "R² = 0,666 ; RMSE = 1,225 ; MAE = 0,966."
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
    
    function(input, output, session) {
      
      
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
          
          data.frame(
            
            `Âge` =
              input$age,
            
            `Heures de sommeil` =
              input$sommeil,
            
            `Activité physique` =
              input$activite,
            
            `Caféine consommée` =
              input$cafeine,
            
            `Consommation d'alcool` =
              input$alcool,
            
            `Qualité de l'alimentation` =
              input$alimentation,
            
            `Niveau de stress` =
              input$stress,
            
            Tabagisme =
              factor(
                input$tabagisme,
                levels = levels(donnees$Tabagisme)
              ),
            
            `Événement de vie récent` =
              factor(
                input$evenement,
                levels = levels(
                  donnees$`Événement de vie récent`
                )
              ),
            
            check.names = FALSE
          )
        }
      )
      
      
      # ======================================================
      # 5. PREDICTION
      # ======================================================
      
      prediction <- eventReactive(
        
        input$predire,
        
        {
          
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
        }
      )
      
      
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
        
        score <- prediction()
        
        
        ggplot(
          
          data.frame(
            x = 1:10
          ),
          
          aes(
            x = x,
            y = 1
          )
          
        ) +
          
          geom_tile(
            aes(fill = x),
            height = 0.45,
            width = 0.95
          ) +
          
          scale_fill_gradient(
            low = "#5CB85C",
            high = "#D9534F"
          ) +
          
          geom_vline(
            xintercept = score,
            linewidth = 2,
            color = "#333333"
          ) +
          
          geom_point(
            aes(
              x = score,
              y = 1
            ),
            size = 6,
            shape = 21,
            fill = "white",
            color = "#333333"
          ) +
          
          scale_x_continuous(
            breaks = 1:10,
            limits = c(0.5, 10.5)
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
      
      extrapolation <- eventReactive(
        
        input$predire,
        
        {
          
          problemes <- character(0)
          
          
          if (
            input$age < borne_age[1] ||
            input$age > borne_age[2]
          ) {
            
            problemes <- c(
              problemes,
              "âge"
            )
          }
          
          
          if (
            input$sommeil < borne_sommeil[1] ||
            input$sommeil > borne_sommeil[2]
          ) {
            
            problemes <- c(
              problemes,
              "sommeil"
            )
          }
          
          
          if (
            input$activite < borne_activite[1] ||
            input$activite > borne_activite[2]
          ) {
            
            problemes <- c(
              problemes,
              "activité physique"
            )
          }
          
          
          if (
            input$cafeine < borne_cafeine[1] ||
            input$cafeine > borne_cafeine[2]
          ) {
            
            problemes <- c(
              problemes,
              "consommation de caféine"
            )
          }
          
          
          if (
            input$alcool < borne_alcool[1] ||
            input$alcool > borne_alcool[2]
          ) {
            
            problemes <- c(
              problemes,
              "consommation d'alcool"
            )
          }
          
          
          problemes
        }
      )
      
      
      # ======================================================
      # 9. AVERTISSEMENT SI HORS DATASET
      # ======================================================
      
      output$alerte_extrapolation <- renderUI({
        
        req(input$predire > 0)
        
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
