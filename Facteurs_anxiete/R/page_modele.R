# ============================================================

# PAGE MODELISATION

# ============================================================

page_modele_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      # Saisie des caractéristiques ----------------------------
      
      box(
        title = "Estimer un niveau d'anxiété",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        
        numericInput(
          ns("age"),
          "Âge",
          value = 30,
          min = 18
        ),
        
        numericInput(
          ns("sommeil"),
          "Heures de sommeil",
          value = 7,
          min = 0
        ),
        
        numericInput(
          ns("activite"),
          "Activité physique (heures/semaine)",
          value = 3,
          min = 0
        ),
        
        numericInput(
          ns("cafeine"),
          "Caféine consommée (mg/jour)",
          value = 100,
          min = 0
        ),
        
        numericInput(
          ns("alcool"),
          "Consommation d'alcool (verres/semaine)",
          value = 1,
          min = 0
        ),
        
        numericInput(
          ns("alimentation"),
          "Qualité de l'alimentation (1 à 10)",
          value = 5,
          min = 1,
          max = 10
        ),
        
        numericInput(
          ns("stress"),
          "Niveau de stress (1 à 10)",
          value = 5,
          min = 1,
          max = 10
        ),
        
        selectInput(
          ns("tabagisme"),
          "Tabagisme",
          choices = NULL
        ),
        
        selectInput(
          ns("evenement"),
          "Événement de vie récent",
          choices = NULL
        ),
        
        actionButton(
          ns("predire"),
          "Calculer la prédiction",
          icon = icon("calculator"),
          class = "btn-primary"
        )
      ),
      
      # Résultat de la prédiction -------------------------------
      
      box(
        title = "Résultat de la prédiction",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        
        h3(
          textOutput(ns("prediction")),
          style = "text-align: center; margin-top: 30px;"
        ),
        
        p(
          "Cette estimation repose sur le modèle de régression linéaire utilisateur.",
          style = "text-align: center;"
        ),
        
        hr(),
        
        p(
          "Il s'agit d'une estimation statistique exploratoire, et non d'un diagnostic médical."
        )
      )
    ),
    
    # Coefficients du modèle ------------------------------------
    
    fluidRow(
      
      box(
        title = "Variables associées au niveau d'anxiété",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        plotOutput(
          ns("graphique_coefficients"),
          height = "400px"
        ),
        
        p(
          "Les coefficients représentent les associations estimées par le modèle, toutes choses égales par ailleurs. Pour les variables qualitatives, ils sont interprétés par rapport à une modalité de référence."
        )
      )
    ),
    
    # Comparaison des modèles -----------------------------------
    
    fluidRow(
      
      box(
        title = "Comparaison des performances",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        tableOutput(ns("tableau_resultats")),
        
        p(
          "Les performances sont calculées sur l'échantillon test, qui n'a pas servi à l'entraînement des modèles."
        )
      )
    )
    
  )
}

# ============================================================

# SERVEUR

# ============================================================

page_modele_server <- function(id, donnees, modeles) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # Récupération des niveaux des variables qualitatives -----
    
    updateSelectInput(
      session,
      "tabagisme",
      choices = levels(donnees$Tabagisme),
      selected = levels(donnees$Tabagisme)[1]
    )
    
    updateSelectInput(
      session,
      "evenement",
      choices = levels(donnees$`Événement de vie récent`),
      selected = levels(donnees$`Événement de vie récent`)[1]
    )
    
    # Construction des nouvelles données ----------------------
    
    nouvelles_donnees <- eventReactive(input$predire, {
      
      req(
        input$age,
        input$sommeil,
        input$activite,
        input$cafeine,
        input$alcool,
        input$alimentation,
        input$stress,
        input$tabagisme,
        input$evenement
      )
      
      data.frame(
        
        `Âge` = input$age,
        
        `Heures de sommeil` = input$sommeil,
        
        `Activité physique` = input$activite,
        
        `Caféine consommée` = input$cafeine,
        
        `Consommation d'alcool` = input$alcool,
        
        `Qualité de l'alimentation` = input$alimentation,
        
        `Niveau de stress` = input$stress,
        
        Tabagisme = factor(
          input$tabagisme,
          levels = modeles$mod_user$xlevels[["Tabagisme"]]
        ),
        
        `Événement de vie récent` = factor(
          input$evenement,
          levels = modeles$mod_user$xlevels[["Événement de vie récent"]]
        ),
        
        check.names = FALSE
      )
    })
    
    # Prédiction ------------------------------------------------
    
    prediction <- eventReactive(input$predire, {
      
      nouvelles_donnees <- nouvelles_donnees()
      
      predict(
        modeles$mod_user,
        newdata = nouvelles_donnees
      )
    })
    
    output$prediction <- renderText({
      
      req(prediction())
      
      paste0(
        round(as.numeric(prediction()), 2),
        " / 10"
      )
    })
    
    # Graphique des coefficients -------------------------------
    
    output$graphique_coefficients <- renderPlot({
      
      coefficients <- coef(modeles$mod_user)
      
      coef_df <- data.frame(
        Variable = names(coefficients),
        Coefficient = as.numeric(coefficients)
      )
      
      coef_df <- coef_df |>
        dplyr::filter(
          Variable != "(Intercept)"
        )
      
      ggplot2::ggplot(
        coef_df,
        ggplot2::aes(
          x = reorder(Variable, Coefficient),
          y = Coefficient,
          fill = Coefficient > 0
        )
      ) +
        
        ggplot2::geom_col() +
        
        ggplot2::coord_flip() +
        
        ggplot2::scale_fill_manual(
          values = c(
            "TRUE" = "#E15759",
            "FALSE" = "#4E79A7"
          ),
          labels = c(
            "TRUE" = "Association positive",
            "FALSE" = "Association négative"
          )
        ) +
        
        ggplot2::labs(
          title = "Coefficients de la régression linéaire",
          x = NULL,
          y = "Coefficient",
          fill = NULL
        ) +
        
        ggplot2::theme_minimal()
    })
    
    # Tableau comparatif ---------------------------------------
    
    output$tableau_resultats <- renderTable({
      
      modeles$resultats |>
        dplyr::mutate(
          dplyr::across(
            where(is.numeric),
            ~ round(.x, 3)
          )
        )
      
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    
  })
}
