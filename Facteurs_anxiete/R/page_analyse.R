
# ============================================================
# UI
# ============================================================

page_analyse_ui <- function(id) {
  
  ns <- NS(id)
  tabsetPanel(
  tabPanel(
    title = "Corrélations avec le niveau d'anxiété",
    
    br(),
    
    h3("Analyse des corrélations"),
    
    p(
      "Cette analyse permet d'étudier les relations entre les variables numériques ",
      "et leur association avec le niveau d'anxiété."
    ),
    
    br(),
    
    radioButtons(
      ns("type_correlation"),
      label = "Type de corrélation à afficher :",
      choices = c(
        "Corrélations avec le niveau d'anxiété" = "anxiete",
        "Corrélations entre variables explicatives" = "general"
      ),
      selected = "anxiete",
      inline = TRUE
    ),
    
    br(),
    
    box(
      title = textOutput(ns("titre_correlation")),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      
      uiOutput(ns("commentaire_correlation")),
      
      plotOutput(
        ns("graphique_correlation"),
        height = "320px"
      )
    )
  ),
  
  
  # --------------------------------------------------------
  # Analyses 2 à 2
  # --------------------------------------------------------
  
  tabPanel(
    title = "Analyses 2 à 2",
    
    br(),
    
    h3("Analyses bivariées"),
    
    p(
      "Sélectionnez deux variables afin d'étudier leur relation. ",
      "Le type de graphique est automatiquement adapté à leur nature."
    ),
    
    br(),
    
    checkboxInput(
      ns("forcer_anxiete"),
      label = "Forcer le niveau d'anxiété comme l'une des variables",
      value = FALSE
    ),
    
    fluidRow(
      
      column(
        width = 6,
        
        selectInput(
          ns("variable_x"),
          label = "Première variable",
          choices = NULL
        )
      ),
      
      column(
        width = 6,
        
        selectInput(
          ns("variable_y"),
          label = "Deuxième variable",
          choices = NULL
        )
      )
    ),
    
    br(),
    
    uiOutput(
      ns("type_graphique")
    ),
    
    br(),
    
    box(
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      
      plotOutput(
        ns("graphique_bivarie"),
        height = "650px"
      )
    )
  )
  
  
  )
}


# ============================================================
# SERVER
# ============================================================

page_analyse_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    
    output$titre_correlation <- renderText({
      
      if (input$type_correlation == "anxiete") {
        "Corrélations avec le niveau d'anxiété"
      } else {
        "Corrélations entre variables explicatives"
      }
    })
    
    
    output$commentaire_correlation <- renderUI({
      
      if (input$type_correlation == "anxiete") {
        
        HTML(
          paste0(
            "<p style='margin-bottom: 15px;'>",
            "<b>Interprétation :</b> ce graphique présente le coefficient ",
            "de corrélation de Pearson entre chaque variable numérique ",
            "et le niveau d'anxiété. ",
            "Une valeur proche de <b>1</b> indique une association linéaire positive, ",
            "une valeur proche de <b>-1</b> une association négative, ",
            "et une valeur proche de <b>0</b> une faible association linéaire.",
            "</p>"
          )
        )
        
      } else {
        
        HTML(
          paste0(
            "<p style='margin-bottom: 15px;'>",
            "<b>Interprétation :</b> la matrice montre les corrélations ",
            "entre les différentes variables explicatives numériques. ",
            "Les couleurs permettent d'identifier les associations positives ",
            "et négatives. Ici, l'absence de couleurs marquées indique que ",
            "les variables explicatives sont globalement peu corrélées entre elles.",
            "</p>"
          )
        )
      }
    })
    
    
    output$graphique_correlation <- renderPlot({
      
      req(input$type_correlation)
      
      # ============================================================
      # 1. Corrélations avec le niveau d'anxiété
      # ============================================================
      
      if (input$type_correlation == "anxiete") {
        
        validate(
          need(
            "Niveau d'anxiété" %in% names(donnees),
            "La variable 'Niveau d'anxiété' est absente."
          )
        )
        
        validate(
          need(
            is.numeric(donnees[["Niveau d'anxiété"]]),
            "La variable 'Niveau d'anxiété' doit être numérique."
          )
        )
        
        variables_numeriques <- names(
          donnees[
            sapply(donnees, is.numeric)
          ]
        )
        
        variables_explicatives <- setdiff(
          variables_numeriques,
          "Niveau d'anxiété"
        )
        
        req(length(variables_explicatives) >= 1)
        
        correlations <- sapply(
          variables_explicatives,
          function(variable) {
            cor(
              donnees[[variable]],
              donnees[["Niveau d'anxiété"]],
              use = "pairwise.complete.obs",
              method = "pearson"
            )
          }
        )
        
        df_cor <- data.frame(
          variable = names(correlations),
          correlation = as.numeric(correlations)
        )
        
        df_cor <- df_cor |>
          dplyr::filter(!is.na(correlation)) |>
          dplyr::arrange(correlation) |>
          dplyr::mutate(
            variable = factor(
              variable,
              levels = variable
            )
          )
        
        ggplot(
          df_cor,
          aes(
            x = correlation,
            y = variable,
            fill = correlation
          )
        ) +
          geom_col() +
          
          geom_vline(
            xintercept = 0,
            linewidth = 0.7
          ) +
          
          scale_fill_gradient2(
            low = "#2166AC",
            mid = "white",
            high = "#B2182B",
            midpoint = 0,
            limits = c(-1, 1)
          ) +
          
          scale_x_continuous(
            limits = c(-1, 1)
          ) +
          
          labs(
            x = "Coefficient de corrélation de Pearson",
            y = NULL
          ) +
          
          theme_minimal(
            base_size = 12
          ) +
          
          theme(
            legend.position = "none",
            axis.text.y = element_text(size = 10),
            panel.grid.major.y = element_blank()
          )
        
        
        # ============================================================
        # 2. Corrplot général
        # ============================================================
        
      } else {
        
        donnees_num <- donnees[
          sapply(donnees, is.numeric)
        ]
        
        # Retirer la variable cible
        if ("Niveau d'anxiété" %in% names(donnees_num)) {
          donnees_num[["Niveau d'anxiété"]] <- NULL
        }
        
        validate(
          need(
            ncol(donnees_num) >= 2,
            "Il faut au moins deux variables numériques explicatives."
          )
        )
        
        matrice_cor <- cor(
          donnees_num,
          use = "pairwise.complete.obs",
          method = "pearson"
        )
        
        df_cor <- as.data.frame(
          as.table(matrice_cor)
        )
        
        names(df_cor) <- c(
          "variable_x",
          "variable_y",
          "correlation"
        )
        
        # Garder uniquement la moitié supérieure
        df_cor <- df_cor |>
          dplyr::filter(
            as.numeric(variable_x) < as.numeric(variable_y)
          ) |>
          dplyr::mutate(
            variable_y = factor(
              variable_y,
              levels = rev(levels(variable_y))
            )
          )
        
        ggplot(
          df_cor,
          aes(
            x = variable_x,
            y = variable_y,
            fill = correlation
          )
        ) +
          geom_tile(
            color = "white",
            linewidth = 0.3
          ) +
          
          scale_fill_gradient2(
            low = "#2166AC",
            mid = "white",
            high = "#B2182B",
            midpoint = 0,
            limits = c(-1, 1),
            name = "Corrélation"
          ) +
          
          labs(
            x = NULL,
            y = NULL
          ) +
          
          theme_minimal(
            base_size = 12
          ) +
          
          theme(
            axis.text.x = element_text(
              angle = 45,
              hjust = 1,
              vjust = 1
            ),
            axis.text.y = element_text(
              size = 10
            ),
            panel.grid = element_blank(),
            plot.margin = margin(10, 10, 10, 10)
          )
      }
    })
    
    
    type_variable <- function(x) {
      
      if (is.numeric(x)) {
        return("numerique")
      }
      
      if (is.factor(x) || is.character(x)) {
        return("qualitative")
      }
      
      return("autre")
    }
    
    
    # ============================================================
    # Initialisation des menus
    # ============================================================
    
    observe({
      
      variables <- names(donnees)
      
      updateSelectInput(
        session,
        "variable_x",
        choices = variables,
        selected = variables[1]
      )
      
      updateSelectInput(
        session,
        "variable_y",
        choices = variables,
        selected = variables[2]
      )
    })
    
    
    # ============================================================
    # Texte indiquant le type d'analyse
    # ============================================================
    
    output$type_graphique <- renderUI({
      
      req(
        input$variable_x,
        input$variable_y
      )
      
      # ==========================================================
      # ANALYSE À 3 VARIABLES
      # ==========================================================
      
      if (input$forcer_anxiete) {
        
        # Vérifier que l'anxiété n'est pas déjà sélectionnée
        validate(
          need(
            input$variable_x != "Niveau d'anxiété" &&
              input$variable_y != "Niveau d'anxiété",
            
            "Lorsque l'analyse à 3 variables est activée, sélectionnez deux variables explicatives."
          )
        )
        
        x <- donnees[[input$variable_x]]
        y <- donnees[[input$variable_y]]
        
        type_x <- type_variable(x)
        type_y <- type_variable(y)
        
        # --------------------------------------------------------
        # NUMERIQUE + QUALITATIVE + ANXIETE
        # --------------------------------------------------------
        
        if (
          (type_x == "numerique" && type_y == "qualitative") ||
          (type_x == "qualitative" && type_y == "numerique")
        ) {
          
          titre <- "Analyse à 3 variables : numérique, qualitative et niveau d'anxiété"
          
          commentaire <- paste0(
            "<b>Lecture :</b> le niveau d'anxiété est représenté en fonction ",
            "de la variable qualitative. La variable numérique permet de ",
            "mettre en évidence les différences entre les observations au sein ",
            "de chaque catégorie."
          )
          
          # --------------------------------------------------------
          # QUALITATIVE + QUALITATIVE + ANXIETE
          # --------------------------------------------------------
          
        } else if (
          type_x == "qualitative" &&
          type_y == "qualitative"
        ) {
          
          titre <- "Analyse à 3 variables : deux qualitatives et niveau d'anxiété"
          
          commentaire <- paste0(
            "<b>Lecture :</b> le niveau d'anxiété est comparé entre les ",
            "différentes combinaisons des deux variables qualitatives. ",
            "Cette représentation permet d'observer si l'association entre ",
            "les catégories varie selon le niveau d'anxiété."
          )
          
        } else {
          
          titre <- "Combinaison non disponible"
          
          commentaire <- paste0(
            "Pour l'analyse à 3 variables, sélectionnez une combinaison ",
            "de variables qualitatives et/ou numériques compatible."
          )
        }
        
        
        div(
          
          style = paste(
            "margin-bottom: 20px;",
            "padding: 12px 15px;",
            "background-color: #f7f7f7;",
            "border-left: 4px solid #3c8dbc;"
          ),
          
          tags$div(
            style = "font-size: 16px; margin-bottom: 5px;",
            tags$b(titre)
          ),
          
          tags$div(
            style = "font-size: 13px;",
            HTML(commentaire)
          )
        )
        
        
        # ==========================================================
        # ANALYSE À 2 VARIABLES
        # ==========================================================
        
      } else {
        
        x <- donnees[[input$variable_x]]
        y <- donnees[[input$variable_y]]
        
        type_x <- type_variable(x)
        type_y <- type_variable(y)
        
        if (
          type_x == "numerique" &&
          type_y == "numerique"
        ) {
          
          titre <- "Nuage de points"
          
          commentaire <- paste0(
            "<b>Lecture :</b> chaque point représente une observation. ",
            "La droite représente la tendance linéaire entre les deux variables."
          )
          
        } else if (
          type_x != type_y
        ) {
          
          titre <- "Comparaison entre groupes"
          
          commentaire <- paste0(
            "<b>Lecture :</b> la distribution de la variable numérique ",
            "est comparée entre les différentes catégories."
          )
          
        } else {
          
          titre <- "Répartition des catégories"
          
          commentaire <- paste0(
            "<b>Lecture :</b> les proportions des catégories de la seconde ",
            "variable sont représentées pour chaque catégorie de la première."
          )
        }
        
        
        div(
          
          style = paste(
            "margin-bottom: 20px;",
            "padding: 12px 15px;",
            "background-color: #f7f7f7;",
            "border-left: 4px solid #3c8dbc;"
          ),
          
          tags$div(
            style = "font-size: 16px; margin-bottom: 5px;",
            tags$b(titre)
          ),
          
          tags$div(
            style = "font-size: 13px;",
            HTML(commentaire)
          )
        )
      }
    })
    
    
    # ============================================================
    # GRAPHIQUE
    # ============================================================
    
    output$graphique_bivarie <- renderPlot({
      
      req(
        input$variable_x,
        input$variable_y
      )
      
      
      # ==========================================================
      # ANALYSE À 3 VARIABLES
      # ==========================================================
      
      if (input$forcer_anxiete) {
        
        # --------------------------------------------------------
        # Vérifications
        # --------------------------------------------------------
        
        validate(
          need(
            "Niveau d'anxiété" %in% names(donnees),
            "La variable 'Niveau d'anxiété' est absente."
          )
        )
        
        validate(
          need(
            input$variable_x != "Niveau d'anxiété" &&
              input$variable_y != "Niveau d'anxiété",
            
            "Les deux variables sélectionnées doivent être différentes du niveau d'anxiété."
          )
        )
        
        
        x <- donnees[[input$variable_x]]
        y <- donnees[[input$variable_y]]
        
        type_x <- type_variable(x)
        type_y <- type_variable(y)
        
        
        # ========================================================
        # NUMERIQUE + QUALITATIVE + ANXIETE
        # ========================================================
        
        if (
          (type_x == "numerique" && type_y == "qualitative") ||
          (type_x == "qualitative" && type_y == "numerique")
        ) {
          
          # Identifier automatiquement les deux types
          
          if (type_x == "numerique") {
            
            variable_num <- input$variable_x
            variable_cat <- input$variable_y
            
          } else {
            
            variable_num <- input$variable_y
            variable_cat <- input$variable_x
          }
          
          
          # Données nécessaires
          
          df_plot <- donnees |>
            dplyr::select(
              anxiete = `Niveau d'anxiété`,
              numerique = dplyr::all_of(variable_num),
              categorie = dplyr::all_of(variable_cat)
            ) |>
            dplyr::filter(
              !is.na(anxiete),
              !is.na(numerique),
              !is.na(categorie)
            )
          
          
          # Graphique
          
          ggplot(
            df_plot,
            aes(
              x = numerique,
              y = anxiete
            )
          ) +
            
            geom_point(
              aes(
                colour = categorie
              ),
              alpha = 0.45,
              size = 2
            ) +
            
            geom_smooth(
              aes(
                colour = categorie
              ),
              method = "lm",
              se = TRUE,
              linewidth = 0.9
            ) +
            
            labs(
              title = paste(
                "Niveau d'anxiété selon",
                variable_num,
                "et",
                variable_cat
              ),
              
              x = variable_num,
              y = "Niveau d'anxiété",
              colour = variable_cat
            ) +
            
            theme_classic(
              base_size = 13
            ) +
            
            theme(
              plot.title = element_text(
                face = "bold",
                size = 16
              ),
              
              axis.title = element_text(
                face = "bold"
              ),
              
              legend.title = element_text(
                face = "bold"
              )
            )
          
          
          # ========================================================
          # QUALITATIVE + QUALITATIVE + ANXIETE
          # ========================================================
          
        } else if (
          type_x == "qualitative" &&
          type_y == "qualitative"
        ) {
          
          df_plot <- donnees |>
            dplyr::select(
              anxiete = `Niveau d'anxiété`,
              categorie_x = dplyr::all_of(input$variable_x),
              categorie_y = dplyr::all_of(input$variable_y)
            ) |>
            dplyr::filter(
              !is.na(anxiete),
              !is.na(categorie_x),
              !is.na(categorie_y)
            )
          
          
          # Créer une variable combinant les deux catégories
          
          df_plot <- df_plot |>
            dplyr::mutate(
              combinaison = interaction(
                categorie_x,
                categorie_y,
                sep = " × "
              )
            )
          
          
          ggplot(
            df_plot,
            aes(
              x = combinaison,
              y = anxiete
            )
          ) +
            
            geom_boxplot(
              width = 0.65,
              alpha = 0.7,
              outlier.shape = NA
            ) +
            
            geom_jitter(
              width = 0.15,
              alpha = 0.25,
              size = 1.5
            ) +
            
            labs(
              title = "Niveau d'anxiété selon deux variables qualitatives",
              
              subtitle = paste(
                input$variable_x,
                "et",
                input$variable_y
              ),
              
              x = paste(
                input$variable_x,
                "×",
                input$variable_y
              ),
              
              y = "Niveau d'anxiété"
            ) +
            
            theme_classic(
              base_size = 13
            ) +
            
            theme(
              plot.title = element_text(
                face = "bold",
                size = 16
              ),
              
              plot.subtitle = element_text(
                size = 12
              ),
              
              axis.title = element_text(
                face = "bold"
              ),
              
              axis.text.x = element_text(
                angle = 45,
                hjust = 1
              )
            )
          
          
        } else {
          
          validate(
            need(
              FALSE,
              "Cette combinaison de variables n'est pas disponible pour une analyse à 3 variables."
            )
          )
        }
        
        
        # ==========================================================
        # ANALYSE À 2 VARIABLES
        # ==========================================================
        
      } else {
        
        # ========================================================
        # Vérifier que les variables sont différentes
        # ========================================================
        
        validate(
          need(
            input$variable_x != input$variable_y,
            "Veuillez sélectionner deux variables différentes."
          )
        )
        
        
        x <- donnees[[input$variable_x]]
        y <- donnees[[input$variable_y]]
        
        type_x <- type_variable(x)
        type_y <- type_variable(y)
        
        
        # ========================================================
        # NUMERIQUE × NUMERIQUE
        # ========================================================
        
        if (
          type_x == "numerique" &&
          type_y == "numerique"
        ) {
          
          correlation <- cor(
            x,
            y,
            use = "pairwise.complete.obs",
            method = "pearson"
          )
          
          df_plot <- data.frame(
            x = x,
            y = y
          ) |>
            dplyr::filter(
              !is.na(x),
              !is.na(y)
            )
          
          
          ggplot(
            df_plot,
            aes(
              x = x,
              y = y
            )
          ) +
            
            geom_point(
              alpha = 0.45,
              size = 2
            ) +
            
            geom_smooth(
              method = "lm",
              se = TRUE,
              linewidth = 1
            ) +
            
            labs(
              title = paste(
                input$variable_y,
                "en fonction de",
                input$variable_x
              ),
              
              subtitle = paste0(
                "Coefficient de corrélation de Pearson : r = ",
                round(correlation, 2)
              ),
              
              x = input$variable_x,
              y = input$variable_y
            ) +
            
            theme_classic(
              base_size = 13
            ) +
            
            theme(
              plot.title = element_text(
                face = "bold",
                size = 16
              ),
              
              axis.title = element_text(
                face = "bold"
              )
            )
          
          
          # ========================================================
          # NUMERIQUE × QUALITATIVE
          # ========================================================
          
        } else if (
          type_x == "numerique" &&
          type_y == "qualitative"
        ) {
          
          ggplot(
            donnees,
            aes(
              x = .data[[input$variable_y]],
              y = .data[[input$variable_x]]
            )
          ) +
            
            geom_boxplot(
              width = 0.65,
              alpha = 0.7,
              outlier.shape = NA
            ) +
            
            geom_jitter(
              width = 0.15,
              alpha = 0.25,
              size = 1.5
            ) +
            
            labs(
              title = paste(
                input$variable_x,
                "selon",
                input$variable_y
              ),
              
              x = input$variable_y,
              y = input$variable_x
            ) +
            
            theme_classic(
              base_size = 13
            )
          
          
          # ========================================================
          # QUALITATIVE × NUMERIQUE
          # ========================================================
          
        } else if (
          type_x == "qualitative" &&
          type_y == "numerique"
        ) {
          
          ggplot(
            donnees,
            aes(
              x = .data[[input$variable_x]],
              y = .data[[input$variable_y]]
            )
          ) +
            
            geom_boxplot(
              width = 0.65,
              alpha = 0.7,
              outlier.shape = NA
            ) +
            
            geom_jitter(
              width = 0.15,
              alpha = 0.25,
              size = 1.5
            ) +
            
            labs(
              title = paste(
                input$variable_y,
                "selon",
                input$variable_x
              ),
              
              x = input$variable_x,
              y = input$variable_y
            ) +
            
            theme_classic(
              base_size = 13
            )
          
          
          # ========================================================
          # QUALITATIVE × QUALITATIVE
          # ========================================================
          
        } else if (
          type_x == "qualitative" &&
          type_y == "qualitative"
        ) {
          
          ggplot(
            donnees,
            aes(
              x = .data[[input$variable_x]],
              fill = .data[[input$variable_y]]
            )
          ) +
            
            geom_bar(
              position = "fill"
            ) +
            
            scale_y_continuous(
              labels = scales::percent
            ) +
            
            labs(
              title = paste(
                "Répartition de",
                input$variable_y,
                "selon",
                input$variable_x
              ),
              
              x = input$variable_x,
              y = "Proportion",
              fill = input$variable_y
            ) +
            
            theme_classic(
              base_size = 13
            )
        }
      }
    })
    
    
    
  })
}