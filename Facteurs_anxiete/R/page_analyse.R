
# ============================================================
# UI
# ============================================================

page_analyse_ui <- function(id) {
  
  ns <- NS(id)
  
  tabsetPanel(
    
    id = ns("sous_page"),
    
    # --------------------------------------------------------
    # Corrélations
    # --------------------------------------------------------
    
    tabPanel(
      title = "Corrélations avec le niveau d'anxiété",
      
      br(),
      
      h3("Analyse des corrélations"),
      
      p(
        "Cette analyse présente les corrélations entre les variables ",
        "numériques ainsi que leur relation avec le niveau d'anxiété."
      ),
      
      br(),
      
      fluidRow(
        
        # ====================================================
        # CORRPLOT
        # ====================================================
        
        box(
          title = "Corrélations entre variables numériques",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("corrplot_general"),
            height = "550px"
          )
        ),
        
        # ====================================================
        # CORRELATIONS AVEC ANXIETE
        # ====================================================
        
        box(
          title = "Corrélations avec le niveau d'anxiété",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("corr_anxiete"),
            height = "550px"
          )
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
        "Sélectionnez deux variables afin d'étudier leur relation."
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

page_analyse_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # ========================================================
    # 1. CORRPLOT GENERAL
    # ========================================================
    
    output$corrplot_general <- renderPlot({
      
      # Variables numériques
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
          "Il faut au moins deux variables numériques."
        )
      )
      
      
      # Matrice de corrélation
      matrice_cor <- cor(
        donnees_num,
        use = "pairwise.complete.obs",
        method = "pearson"
      )
      
      
      # Transformer la matrice en format long
      df_cor <- as.data.frame(as.table(matrice_cor))
      
      names(df_cor) <- c(
        "variable_x",
        "variable_y",
        "correlation"
      )
      
      
      # Garder uniquement la moitié supérieure
      df_cor <- df_cor |>
        filter(
          as.numeric(variable_x) < as.numeric(variable_y)
        ) |>
        mutate(
          variable_y = forcats::fct_rev(variable_y)
        )
      
      
      # ----------------------------------------------------------
      # Graphique
      # ----------------------------------------------------------
      
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
        
        coord_fixed() +
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
          
          plot.margin = margin(
            10, 10, 10, 10
          )
        )
    })
    
    
    # ========================================================
    # 2. CORRELATIONS AVEC LE NIVEAU D'ANXIETE
    # ========================================================
    
    output$corr_anxiete <- renderPlot({
      
      # Vérifier que la variable existe
      validate(
        need(
          "Niveau d'anxiété" %in% names(donnees),
          "La variable 'Niveau d'anxiété' est absente."
        )
      )
      
      # Vérifier que la variable est numérique
      validate(
        need(
          is.numeric(donnees[["Niveau d'anxiété"]]),
          "La variable 'Niveau d'anxiété' doit être numérique."
        )
      )
      
      
      # Variables numériques
      variables_numeriques <- names(
        donnees[
          sapply(donnees, is.numeric)
        ]
      )
      
      # Retirer la variable cible
      variables_explicatives <- setdiff(
        variables_numeriques,
        "Niveau d'anxiété"
      )
      
      req(length(variables_explicatives) >= 1)
      
      
      # Calcul des corrélations
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
      
      
      # Mise en dataframe
      df_cor <- data.frame(
        variable = names(correlations),
        correlation = as.numeric(correlations)
      )
      
      
      # Supprimer les valeurs manquantes
      df_cor <- df_cor |>
        filter(!is.na(correlation))
      
      
      # Trier du plus négatif au plus positif
      df_cor <- df_cor |>
        arrange(correlation) |>
        mutate(
          variable = factor(
            variable,
            levels = variable
          )
        )
      
      
      # ------------------------------------------------------
      # Graphique
      # ------------------------------------------------------
      
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
          axis.text.y = element_text(size = 9),
          panel.grid.major.y = element_blank()
        )
      
    })
    
  })
}