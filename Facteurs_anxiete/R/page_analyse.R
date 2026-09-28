
page_analyse_ui <- function(id) {

  ns <- NS(id)

  tagList(

    # -----------------------------------------------------
    # Sous-pages
    # -----------------------------------------------------
    tabsetPanel(

      id = ns("sous_page"),

      # ===================================================
      # PAGE 1 : CORRELATIONS
      # ===================================================

      tabPanel(

        title = "Corrélations avec Niveau d'anxiété",

        br(),

        h3("Corrélations entre variables numériques"),

        p(
          "Cette section présente les relations entre les variables numériques ",
          "ainsi que leur corrélation avec la variable cible ",
          strong("Niveau d'anxiété"),
          "."
        ),

        # -----------------------------------------------
        # Choix du nombre de variables
        # -----------------------------------------------

        sliderInput(
          ns("n_variables"),
          "Nombre de variables affichées dans le corrplot :",
          min = 5,
          max = 30,
          value = 15,
          step = 1
        ),

        fluidRow(

          # ---------------------------------------------
          # CORRPLOT
          # ---------------------------------------------

          box(
            title = "Matrice de corrélation",
            width = 7,
            status = "primary",
            solidHeader = TRUE,

            plotOutput(
              ns("corrplot"),
              height = "650px"
            )
          ),

          # ---------------------------------------------
          # CORRELATION AVEC LA CIBLE
          # ---------------------------------------------

          box(
            title = "Corrélations avec Niveau d'anxiété",
            width = 5,
            status = "primary",
            solidHeader = TRUE,

            plotOutput(
              ns("corr_cible"),
              height = "650px"
            )
          )
        )
      ),


      # ===================================================
      # PAGE 2 : ANALYSES 2 À 2
      # ===================================================

      tabPanel(

        title = "Analyses 2 à 2",

        br(),

        h3("Analyses bivariées"),

        p(
          "Sélectionnez deux variables afin d'étudier leur relation."
        )

        # On ajoutera cette partie ensuite
      )
    )
  )
}


# =========================================================
# SERVER
# =========================================================

page_analyse_server <- function(id, donnees) {

  moduleServer(id, function(input, output, session) {


    # =====================================================
    # VARIABLES NUMERIQUES
    # =====================================================

    variables_numeriques <- names(
      donnees[
        sapply(donnees, is.numeric)
      ]
    )


    # Vérification de la présence de la cible
    validate(
      need(
        "Niveau d'anxiété" %in% names(donnees),
        "La variable 'Niveau d'anxiété' est absente des données."
      )
    )


    # Variables numériques hors cible
    variables_predictives <- setdiff(
      variables_numeriques,
      "Niveau d'anxiété"
    )


    # =====================================================
    # CORRPLOT
    # =====================================================

    output$corrplot <- renderPlot({

      req(length(variables_predictives) >= 2)


      # ---------------------------------------------------
      # Calcul de la matrice de corrélation
      # ---------------------------------------------------

      cor_mat <- cor(
        donnees[, variables_predictives, drop = FALSE],
        use = "pairwise.complete.obs",
        method = "pearson"
      )


      # ---------------------------------------------------
      # Sélection des variables les plus corrélées
      # à Niveau d'anxiété
      # ---------------------------------------------------

      cor_cible <- cor(
        donnees[, variables_predictives, drop = FALSE],
        donnees[["Niveau d'anxiété"]],
        use = "pairwise.complete.obs",
        method = "pearson"
      )

      cor_cible <- sort(
        abs(cor_cible),
        decreasing = TRUE
      )


      n <- min(
        input$n_variables,
        length(cor_cible)
      )

      variables_selectionnees <- names(
        cor_cible[1:n]
      )


      # Matrice réduite
      cor_mat_reduit <- cor_mat[
        variables_selectionnees,
        variables_selectionnees,
        drop = FALSE
      ]


      # ---------------------------------------------------
      # CORRPLOT
      # ---------------------------------------------------

      corrplot(
        cor_mat_reduit,
        method = "color",
        type = "upper",

        # Affichage des coefficients
        addCoef.col = "black",
        number.cex = 0.65,

        # Noms
        tl.col = "black",
        tl.cex = 0.75,
        tl.srt = 45,

        # Couleurs
        col = colorRampPalette(
          c(
            "#2166AC",
            "#67A9CF",
            "#F7F7F7",
            "#EF8A62",
            "#B2182B"
          )
        )(200),

        # Échelle
        cl.cex = 0.8,

        # Ne pas afficher la diagonale
        diag = FALSE
      )

    })


    # =====================================================
    # CORRELATIONS AVEC Niveau d'anxiété
    # =====================================================

    output$corr_cible <- renderPlot({

      req(length(variables_predictives) >= 1)


      # ---------------------------------------------------
      # Calcul des corrélations
      # ---------------------------------------------------

      correlations <- sapply(
        variables_predictives,
        function(variable) {

          cor(
            donnees[[variable]],
            donnees[["Niveau d'anxiété"]],
            use = "pairwise.complete.obs",
            method = "pearson"
          )
        }
      )


      # ---------------------------------------------------
      # Dataframe
      # ---------------------------------------------------

      df_cor <- data.frame(
        variable = names(correlations),
        correlation = as.numeric(correlations)
      )


      # Retirer les NA
      df_cor <- df_cor |>
        filter(!is.na(correlation))


      # ---------------------------------------------------
      # Trier par corrélation
      # ---------------------------------------------------

      df_cor <- df_cor |>
        arrange(correlation) |>
        mutate(
          variable = factor(
            variable,
            levels = variable
          )
        )


      # ---------------------------------------------------
      # Graphique
      # ---------------------------------------------------

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

        theme_minimal(base_size = 12) +

        theme(
          legend.position = "none",
          axis.text.y = element_text(
            size = 9
          ),
          panel.grid.major.y = element_blank()
        )

    })

  })
}