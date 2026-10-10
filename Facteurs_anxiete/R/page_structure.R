
page_structure_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    h2(
      "Exploration de la structure des données",
      class = "accueil-titre"
    ),
    
    p(
      "Cette partie explore la structure du jeu de données afin ",
      "d'identifier les principales relations entre les variables ",
      "et de rechercher d'éventuels profils associés au niveau d'anxiété."
    ),
    
    tabsetPanel(
      
      id = ns("sous_page"),
      
      
      # ============================================================
      # 1. STRUCTURE GENERALE
      # ============================================================
      
      tabPanel(
        "Structure générale",
        
        br(),
        
        p(
          "Cette première étape permet d'examiner les relations générales ",
          "entre les variables quantitatives et de comparer directement ",
          "les niveaux d'anxiété selon les différentes catégories professionnelles."
        ),
        
        fluidRow(
          
          # ---- Matrice de corrélation ----
          
          column(
            width = 6,
            
            h4("Corrélations entre variables quantitatives"),
            
            plotOutput(
              ns("correlation"),
              height = "600px"
            ),
            
            p(
              "La matrice permet d'identifier les variables qui évoluent ",
              "de manière similaire ou opposée dans le jeu de données."
            )
          ),
          
          # ---- Boxplots CSP ----
          
          column(
            width = 6,
            
            h4("Niveau d'anxiété selon la profession"),
            plotOutput(
              ns("boxplot_profession"),
              height = "600px"
            ),
            p(
              "La distribution du niveau d’anxiété selon la profession révèle une structure étonnamment régulière : les boîtes à moustaches présentent seulement deux types de médianes (3 et 4) et deux configurations de quartiles, malgré la diversité des catégories professionnelles. Ces profils se répètent d’une profession à l’autre, sans faire apparaître de différences nettes entre les distributions. Cette régularité, particulièrement visible sur ce graphique, a constitué un indice majeur nous conduisant à réorienter notre sujet : plutôt que de nous limiter à l’étude des liens entre profession et anxiété, nous avons choisi d’explorer plus largement la structure des données, afin de déterminer si cette répétition des profils reflète une organisation sous-jacente."
            )
            
            
          )
        )
      ),
      
      
      # ============================================================
      # 2. EVOLUTION AVEC L'ANXIETE
      # ============================================================
      
      tabPanel(
        "Évolution avec l'anxiété",
        
        br(),
        
        p(
          "On étudie ici l'évolution des variables quantitatives et qualitatives ",
          "lorsque le niveau d'anxiété augmente. Une attention particulière est ",
          "portée à la transition entre les niveaux 7 et 8."
        ),
        
        # ---- Evolution de toutes les variables quantitatives ----
        
        fluidRow(
          
          column(
            width = 12,
            
            h4("Évolution des variables quantitatives"),
            
            p(
              "Les variables ont été standardisées afin de pouvoir comparer ",
              "leur évolution sur une même échelle. Chaque courbe représente ",
              "la moyenne standardisée d'une variable pour chaque niveau d'anxiété."
            ),
            
            plotlyOutput(
              ns("evolution_toutes_variables"),
              height = "650px"
            )
          )
        ),
        
        # ---- Rupture quantitative + variation qualitative ----
        
        fluidRow(
          
          column(
            width = 6,
            
            h4("Ruptures entre niveaux d'anxiété"),
            
            p(
              "Ce graphique représente l'amplitude du changement entre deux ",
              "niveaux d'anxiété successifs. Une rupture importante entre 7 et 8 ",
              "indique que plusieurs variables changent fortement au même endroit."
            ),
            
            plotOutput(
              ns("ruptures"),
              height = "500px"
            )
          ),
          
          column(
            width = 6,
            
            h4("Variation des variables qualitatives"),
            
            p(
              "Pour chaque variable qualitative, on mesure la variation de la ",
              "composition des catégories entre deux niveaux d'anxiété successifs. ",
              "Une valeur élevée signifie que la répartition des catégories change fortement."
            ),
            
            plotOutput(
              ns("variation_qualitative"),
              height = "500px"
            )
          )
        ),
        
        br(),
        
        p(
          "La transition 7 → 8 constitue ici un point particulièrement marqué : ",
          "elle correspond à une modification simultanée de plusieurs variables ",
          "quantitatives et, potentiellement, de la composition des variables qualitatives. ",
          "Cette observation a motivé l'étude plus approfondie de deux profils d'anxiété."
        )
      ),
      
      
      # ============================================================
      # 3. PROFILS D'ANXIETE
      # ============================================================
      
      tabPanel(
        "Profils d'anxiété",
        
        br(),
        
        p(
          "À partir de la rupture observée entre les niveaux 7 et 8, ",
          "les observations sont regroupées en deux profils : les niveaux ",
          "1 à 7 et les niveaux 8 à 10."
        ),
        
        fluidRow(
          
          column(
            width = 6,
            
            h4("Comparaison des deux profils (variables quantitatives)"),
            
            p(
              "Les variables sont standardisées afin de comparer leur position ",
              "relative dans les deux groupes. Le graphique permet d'identifier ",
              "les caractéristiques qui différencient le plus les deux profils."
            ),
            
            plotOutput(
              ns("profils_anxiete"),
              height = "600px"
            )
          ),
          
          column(
            width = 6,
            
            h4("Comparaison des deux profils (variables qualitatives)"),
            
            p(
              "Les proportions de chaque catégorie sont comparées entre les deux ",
              "groupes d'anxiété afin d'identifier les caractéristiques qualitatives qui les différencient le plus."
            ),
            
            plotOutput(
              ns("profil_complementaire"),
              height = "600px"
            )
          )
        )
      ),
      
      
      # ============================================================
      # 4. STRUCTURE MULTIVARIEE
      # ============================================================
      
      tabPanel(
        "Structure multivariée",
        
        br(),
        
        p(
          "Une analyse en composantes principales (ACP) est utilisée pour ",
          "étudier la structure multivariée des variables quantitatives et ",
          "visualiser les principales dimensions de variation des individus."
        ),
        
        fluidRow(
          
          # ---- Valeurs propres ----
          
          column(
            width = 6,
            
            h4("Variance expliquée par les axes"),
            
            p(
              "La première dimension explique environ 19,5 % de la variance. ",
              "Les dimensions suivantes expliquent également des proportions importantes, ",
              "autour de 11 %, 10,7 %, 10,4 %, 10,4 %, 10,1 %, etc."
            ),
            
            plotOutput(
              ns("pca_eigen"),
              height = "500px"
            )
          ),
          
          # ---- Variables ----
          
          column(
            width = 6,
            
            h4("Contribution des variables aux axes"),
            
            p(
              "Ce graphique permet d'identifier les variables qui contribuent ",
              "le plus à la construction des principales dimensions de l'ACP."
            ),
            
            plotOutput(
              ns("pca_variables"),
              height = "500px"
            )
          )
        ),
        
        fluidRow(
          
          column(
            width = 12,
            
            h4("Position des individus dans l'espace de l'ACP"),
            
            p(
              "La coloration selon le groupe d'anxiété permet d'observer si ",
              "la première dimension sépare clairement les niveaux d'anxiété 1–7 ",
              "des niveaux 8–10."
            ),
            
            plotOutput(
              ns("pca_individus"),
              height = "650px"
            )
          )
        ),
        
        br(),
        
        p(
          "Un élément particulièrement intéressant est que le premier axe est ",
          "fortement associé à la majorité des variables explicatives et permet ",
          "de distinguer clairement les deux groupes d'anxiété. Cette séparation ",
          "est encore plus visible lorsque les individus sont colorés selon leur groupe."
        ),
        
        p(
          "La répartition de la variance apporte également un élément de réflexion : ",
          "la première dimension explique environ 19,5 % de la variance, tandis que ",
          "les dimensions suivantes expliquent encore chacune autour de 10 à 11 %. ",
          "L'absence de chute très nette après le premier axe constitue un élément ",
          "supplémentaire qui nous pousse à examiner attentivement la structure des données."
        ),
        
        p(
          "Cette analyse pourra être approfondie ultérieurement avec une méthode ",
          "adaptée à la présence simultanée de variables quantitatives et qualitatives, ",
          "comme une analyse factorielle des données mixtes."
        )
      )
    )
  )
}


# ================================================================
# SERVER
# ================================================================

page_structure_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # ============================================================
    # VARIABLES
    # ============================================================
    
    variables_quantitatives <- c(
      "Âge",
      "Heures de sommeil",
      "Activité physique",
      "Caféine consommée",
      "Consommation d'alcool",
      "Niveau de stress",
      "Fréquence cardiaque",
      "Fréquence respiratoire",
      "Niveau de transpiration",
      "Séances de thérapie",
      "Qualité de l'alimentation",
      "Niveau d'anxiété"
    )
    
    
    variables_profils <- c(
      "Heures de sommeil",
      "Activité physique",
      "Caféine consommée",
      "Consommation d'alcool",
      "Niveau de stress",
      "Fréquence cardiaque",
      "Fréquence respiratoire",
      "Niveau de transpiration",
      "Séances de thérapie",
      "Qualité de l'alimentation"
    )
    
    
    # Variables qualitatives utilisées pour étudier les changements
    # de composition entre niveaux d'anxiété.
    
    variables_qualitatives <- c(
      "Sexe",
      "Profession",
      "Tabagisme",
      "Antécédents familiaux d'anxiété",
      "Vertiges",
      "Médication",
      "Événement de vie récent"
    )
    
    
    # ============================================================
    # 1. CORRELATIONS
    # ============================================================
    
    output$correlation <- renderPlot({
      
      vars <- variables_quantitatives
      
      # Mettre le niveau d'anxiété en première position
      vars <- c(
        "Niveau d'anxiété",
        setdiff(vars, "Niveau d'anxiété")
      )
      
      matrice_cor <- stats::cor(
        donnees[, vars, drop = FALSE],
        use = "pairwise.complete.obs"
      )
      
      # Conserver uniquement le triangle inférieur
      matrice_cor[lower.tri(matrice_cor, diag = FALSE)] <- NA
      
      cor_long <- as.data.frame(as.table(matrice_cor)) |>
        dplyr::rename(
          Variable1 = Var1,
          Variable2 = Var2,
          Correlation = Freq
        ) |>
        dplyr::filter(!is.na(Correlation)) |>
        dplyr::mutate(
          Variable1 = factor(Variable1, levels = vars),
          Variable2 = factor(Variable2, levels = rev(vars)),
          Anxiete = Variable1 == "Niveau d'anxiété" |
            Variable2 == "Niveau d'anxiété"
        )
      
      ggplot2::ggplot(
        cor_long,
        ggplot2::aes(
          x = Variable1,
          y = Variable2,
          fill = Correlation
        )
      ) +
        ggplot2::geom_tile(
          color = "white",
          linewidth = 0.5
        ) +
        ggplot2::geom_tile(
          data = dplyr::filter(cor_long, Anxiete),
          fill = NA,
          color = "#E45756",
          linewidth = 1
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = sprintf("%.2f", Correlation),
            fontface = ifelse(Anxiete, "bold", "plain")
          ),
          size = 3
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#3B6FB6",
          mid = "white",
          high = "#E45756",
          midpoint = 0,
          limits = c(-1, 1)
        ) +
        ggplot2::scale_x_discrete(
          limits = vars,
          drop = FALSE
        ) +
        ggplot2::scale_y_discrete(
          limits = rev(vars),
          drop = FALSE
        ) +
        ggplot2::labs(
          title = "Corrélations entre variables quantitatives",
          subtitle = "Triangle inférieur uniquement ; l'anxiété est encadrée en rouge",
          x = NULL,
          y = NULL,
          fill = "Corrélation"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          ),
          axis.text.y = ggplot2::element_text(
            color = "grey20"
          ),
          plot.title = ggplot2::element_text(face = "bold")
        )
      
    }, res = 100)
    
    # ============================================================
    # 2. BOXPLOTS PROFESSION
    # ============================================================
    
    output$boxplot_profession <- renderPlot({
      
      ggplot2::ggplot(
        donnees,
        ggplot2::aes(
          x = Profession,
          y = `Niveau d'anxiété`
        )
      ) +
        
        ggplot2::geom_boxplot(
          outlier.alpha = 0.3
        ) +
        
        ggplot2::scale_y_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::labs(
          title = "Distribution du niveau d'anxiété selon la profession",
          x = "Profession",
          y = "Niveau d'anxiété"
        ) +
        
        ggplot2::theme_minimal(base_size = 12) +
        
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          )
        )
    })
    
    
    # ============================================================
    # 3. EVOLUTION DE TOUTES LES VARIABLES QUANTITATIVES
    # ============================================================
    
    donnees_profils <- donnees |>
      
      dplyr::select(
        dplyr::all_of(variables_profils),
        `Niveau d'anxiété`
      ) |>
      
      tidyr::pivot_longer(
        cols = dplyr::all_of(variables_profils),
        names_to = "Variable",
        values_to = "Valeur"
      ) |>
      
      dplyr::group_by(Variable) |>
      
      dplyr::mutate(
        Valeur_z = as.numeric(
          scale(Valeur)
        )
      ) |>
      
      dplyr::ungroup() |>
      
      dplyr::group_by(
        `Niveau d'anxiété`,
        Variable
      ) |>
      
      dplyr::summarise(
        Moyenne_z = mean(
          Valeur_z,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    
    output$evolution_toutes_variables <- plotly::renderPlotly({
      
      p <- ggplot2::ggplot(
        donnees_profils,
        ggplot2::aes(
          x = `Niveau d'anxiété`,
          y = Moyenne_z,
          color = Variable,
          group = Variable,
          text = paste0(
            "Variable : ", Variable,
            "<br>Niveau d'anxiété : ", `Niveau d'anxiété`,
            "<br>Moyenne standardisée : ", round(Moyenne_z, 2)
          ),
          key = Variable
        )
      ) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_vline(
          xintercept = 7.5,
          linetype = "dashed",
          color = "#E45756",
          linewidth = 0.8
        ) +
        ggplot2::scale_x_continuous(breaks = 1:10) +
        ggplot2::labs(
          title = "Évolution standardisée des variables quantitatives",
          x = "Niveau d'anxiété",
          y = "Moyenne standardisée",
          color = "Variable"
        ) +
        ggplot2::theme_minimal(base_size = 12)
      
      plotly::ggplotly(
        p,
        tooltip = "text"
      ) |>
        plotly::event_register("plotly_legendclick")
    })
    
    # ============================================================
    # 4. RUPTURES QUANTITATIVES
    # ============================================================
    
    donnees_ruptures <- donnees_profils |>
      
      dplyr::arrange(
        Variable,
        `Niveau d'anxiété`
      ) |>
      
      dplyr::group_by(Variable) |>
      
      dplyr::mutate(
        Niveau_precedent = dplyr::lag(
          `Niveau d'anxiété`
        ),
        Changement = Moyenne_z - dplyr::lag(Moyenne_z),
        Amplitude = abs(Changement)
      ) |>
      
      dplyr::ungroup() |>
      
      dplyr::filter(
        !is.na(Amplitude)
      ) |>
      
      dplyr::mutate(
        Transition = paste0(
          Niveau_precedent,
          " → ",
          `Niveau d'anxiété`
        )
      )
    
    
    output$ruptures <- renderPlot({
      
      ggplot2::ggplot(
        donnees_ruptures,
        ggplot2::aes(
          x = Transition,
          y = Variable,
          fill = Amplitude
        )
      ) +
        
        ggplot2::geom_tile() +
        
        ggplot2::geom_text(
          ggplot2::aes(
            label = round(Amplitude, 2)
          ),
          size = 3
        ) +
        
        ggplot2::scale_fill_gradient(
          low = "white",
          high = "red"
        ) +
        
        ggplot2::labs(
          title = "Amplitude des changements entre niveaux successifs",
          x = "Transition",
          y = NULL,
          fill = "Amplitude"
        ) +
        
        ggplot2::theme_minimal(base_size = 11) +
        
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          )
        )+ ggplot2::geom_rect(
          data = data.frame(
            Transition = "7 → 8",
            xmin = 6.5,
            xmax = 7.5,
            ymin = -Inf,
            ymax = Inf
          ),
          ggplot2::aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
          ),
          inherit.aes = FALSE,
          fill = NA,
          color = "#E45756",
          linewidth = 1.2
        )
    })
    
    
    # ============================================================
    # 5. VARIATION DES VARIABLES QUALITATIVES
    # ============================================================
    
    # Pour chaque variable qualitative et chaque transition,
    # on calcule la distance de variation entre les distributions
    # des catégories.
    #
    # Une valeur de 0 signifie que les proportions sont identiques.
    # Une valeur élevée indique une forte modification de la composition.
    
    variations_qualitatives <- purrr::map_dfr(
      variables_qualitatives,
      function(variable) {
        
        x <- donnees[[variable]]
        anx <- donnees$`Niveau d'anxiété`
        
        niveaux_categories <- sort(
          unique(
            as.character(x[!is.na(x)])
          )
        )
        
        resultats <- purrr::map_dfr(
          1:9,
          function(niveau) {
            
            x1 <- as.character(
              x[anx == niveau]
            )
            
            x2 <- as.character(
              x[anx == niveau + 1]
            )
            
            x1 <- x1[!is.na(x1)]
            x2 <- x2[!is.na(x2)]
            
            if (
              length(x1) == 0 ||
              length(x2) == 0
            ) {
              
              return(
                tibble::tibble(
                  Niveau_precedent = niveau,
                  Niveau_suivant = niveau + 1,
                  Variation = NA_real_
                )
              )
            }
            
            prop1 <- table(
              factor(
                x1,
                levels = niveaux_categories
              )
            )
            
            prop2 <- table(
              factor(
                x2,
                levels = niveaux_categories
              )
            )
            
            prop1 <- prop1 / sum(prop1)
            prop2 <- prop2 / sum(prop2)
            
            variation <- 0.5 * sum(
              abs(
                prop1 - prop2
              )
            )
            
            tibble::tibble(
              Niveau_precedent = niveau,
              Niveau_suivant = niveau + 1,
              Variation = variation
            )
          }
        )
        
        resultats |>
          dplyr::mutate(
            Variable = variable,
            Transition = paste0(
              Niveau_precedent,
              " → ",
              Niveau_suivant
            )
          )
      }
    )
    
    
    output$variation_qualitative <- renderPlot({
      
      ggplot2::ggplot(
        variations_qualitatives,
        ggplot2::aes(
          x = Transition,
          y = Variable,
          fill = Variation
        )
      ) +
        
        ggplot2::geom_tile() +
        
        ggplot2::geom_text(
          ggplot2::aes(
            label = round(Variation, 2)
          ),
          size = 3
        ) +
        
        ggplot2::scale_fill_gradient(
          low = "white",
          high = "red",
          limits = c(0, 1)
        ) +
        
        ggplot2::labs(
          title = "Variation de la composition des variables qualitatives",
          x = "Transition",
          y = NULL,
          fill = "Variation"
        ) +
        
        ggplot2::theme_minimal(base_size = 11) +
        
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          )
        )+ ggplot2::geom_rect(
          data = data.frame(
            Transition = "7 → 8",
            xmin = 6.5,
            xmax = 7.5,
            ymin = -Inf,
            ymax = Inf
          ),
          ggplot2::aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
          ),
          inherit.aes = FALSE,
          fill = NA,
          color = "#E45756",
          linewidth = 1.2
        )
    })
    
    
    # ============================================================
    # 6. PROFILS 1-7 VS 8-10
    # ============================================================
    
    donnees_groupes <- donnees |>
      
      dplyr::mutate(
        Groupe_anxiete = dplyr::if_else(
          `Niveau d'anxiété` >= 8,
          "Anxiété 8–10",
          "Anxiété 1–7"
        )
      )
    
    
    profils_groupes <- donnees_groupes |>
      
      dplyr::select(
        Groupe_anxiete,
        dplyr::all_of(variables_profils)
      ) |>
      
      tidyr::pivot_longer(
        cols = dplyr::all_of(variables_profils),
        names_to = "Variable",
        values_to = "Valeur"
      ) |>
      
      dplyr::group_by(Variable) |>
      
      dplyr::mutate(
        Valeur_z = as.numeric(
          scale(Valeur)
        )
      ) |>
      
      dplyr::ungroup() |>
      
      dplyr::group_by(
        Groupe_anxiete,
        Variable
      ) |>
      
      dplyr::summarise(
        Moyenne_z = mean(
          Valeur_z,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    # Préparer les deux valeurs de chaque variable
    profils_wide <- profils_groupes |>
      tidyr::pivot_wider(
        names_from = Groupe_anxiete,
        values_from = Moyenne_z
      ) |>
      dplyr::mutate(
        Difference = abs(`Anxiété 8–10` - `Anxiété 1–7`),
        Variable = reorder(Variable, Difference)
      )
    # Calculer la différence maximale de proportion pour chaque variable
    profils_qualitatifs <- purrr::map_dfr(
      variables_qualitatives,
      function(variable) {
        
        donnees_groupes |>
          dplyr::filter(!is.na(.data[[variable]])) |>
          dplyr::count(
            Groupe_anxiete,
            Categorie = .data[[variable]],
            name = "n"
          ) |>
          dplyr::group_by(Groupe_anxiete) |>
          dplyr::mutate(Proportion = n / sum(n)) |>
          dplyr::ungroup() |>
          tidyr::complete(
            Groupe_anxiete,
            Categorie,
            fill = list(n = 0, Proportion = 0)
          ) |>
          dplyr::select(
            Groupe_anxiete,
            Categorie,
            Proportion
          ) |>
          tidyr::pivot_wider(
            names_from = Groupe_anxiete,
            values_from = Proportion,
            values_fill = 0
          ) |>
          dplyr::mutate(
            Difference = `Anxiété 8–10` - `Anxiété 1–7`,
            Variable = variable
          ) |>
          dplyr::group_by(Variable) |>
          dplyr::slice_max(
            order_by = abs(Difference),
            n = 1,
            with_ties = FALSE
          ) |>
          dplyr::ungroup()
      }
    )
    
    profils_qualitatifs <- profils_qualitatifs |>
      dplyr::mutate(
        Variable = reorder(Variable, Difference)
      )
    
    output$profils_anxiete <- renderPlot({
      
      ggplot2::ggplot(
        profils_wide,
        ggplot2::aes(y = Variable)
      ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = `Anxiété 1–7`,
            xend = `Anxiété 8–10`,
            yend = Variable
          ),
          color = "grey65",
          linewidth = 1
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = `Anxiété 1–7`,
            color = "Anxiété 1–7"
          ),
          size = 3.5
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = `Anxiété 8–10`,
            color = "Anxiété 8–10"
          ),
          size = 3.5
        ) +
        ggplot2::geom_vline(
          xintercept = 0,
          linetype = "dashed",
          color = "grey60"
        ) +
        ggplot2::scale_color_manual(
          values = c(
            "Anxiété 1–7" = "#3B6FB6",
            "Anxiété 8–10" = "#E45756"
          )
        ) +
        ggplot2::labs(
          title = "Différences entre les deux profils",
          x = "Moyenne standardisée",
          y = NULL,
          color = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom"
        )
    })
    
    
    
    output$profil_complementaire <- renderPlot({
      
      ggplot2::ggplot(
        profils_qualitatifs,
        ggplot2::aes(
          x = Difference,
          y = Variable,
          color = Difference > 0
        )
      ) +
        ggplot2::geom_vline(
          xintercept = 0,
          linetype = "dashed",
          color = "grey60"
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = 0,
            xend = Difference,
            yend = Variable
          ),
          color = "grey65",
          linewidth = 1
        ) +
        ggplot2::geom_point(size = 3.5) +
        ggplot2::scale_color_manual(
          values = c(
            "TRUE" = "#E45756",
            "FALSE" = "#3B6FB6"
          ),
          labels = c(
            "TRUE" = "Plus fréquent dans le groupe 8–10",
            "FALSE" = "Moins fréquent dans le groupe 8–10"
          )
        ) +
        ggplot2::labs(
          title = "Différences de proportions entre les groupes",
          subtitle = "Catégorie présentant l'écart de proportion maximal pour chaque variable",
          x = "Proportion (8–10) − proportion (1–7)",
          y = NULL,
          color = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom"
        )
    })
    
    
    # ============================================================
    # 7. ACP
    # ============================================================
    
    donnees_acp <- donnees |>
      
      dplyr::select(
        "Heures de sommeil",
        "Activité physique",
        "Caféine consommée",
        "Consommation d'alcool",
        "Qualité de l'alimentation",
        "Niveau de stress",
        "Fréquence cardiaque",
        "Fréquence respiratoire",
        "Niveau de transpiration"
      )
    
    
    acp <- FactoMineR::PCA(
      donnees_acp,
      ncp = 9,
      scale.unit = TRUE,
      graph = FALSE
    )
    
    
    # ============================================================
    # 8. VALEURS PROPRES
    # ============================================================
    
    output$pca_eigen <- renderPlot({
      
      factoextra::fviz_eig(
        acp,
        addlabels = TRUE,
        ylim = c(0, 25)
      ) +
        
        ggplot2::labs(
          title = "Variance expliquée par les composantes"
        ) +
        
        ggplot2::theme_minimal(base_size = 12)
    })
    
    
    # ============================================================
    # 9. CONTRIBUTION DES VARIABLES
    # ============================================================
    
    output$pca_variables <- renderPlot({
      
      factoextra::fviz_pca_var(
        acp,
        col.var = "contrib",
        repel = TRUE
      ) +
        
        ggplot2::labs(
          title = "Contribution des variables aux axes"
        ) +
        
        ggplot2::theme_minimal(base_size = 12)
    })
    
    
    # ============================================================
    # 10. INDIVIDUS
    # ============================================================
    
    groupe_acp <- donnees |>
      
      dplyr::mutate(
        Groupe_anxiete = dplyr::if_else(
          `Niveau d'anxiété` >= 8,
          "Anxiété 8–10",
          "Anxiété 1–7"
        )
      ) |>
      
      dplyr::pull(
        Groupe_anxiete
      )
    
    
    output$pca_individus <- renderPlot({
    
    factoextra::fviz_pca_ind(
      acp,
      geom = "point",
      label = "none",
      habillage = groupe_acp,
      addEllipses = TRUE,
      ellipse.level = 0.95,
      repel = FALSE,
      legend.title = "Groupe d'anxiété"
    ) +
      ggplot2::labs(
        title = "Individus dans le plan factoriel",
        color = "Groupe d'anxiété",
        fill = "Groupe d'anxiété"
      ) +
      ggplot2::theme_minimal(base_size = 12)
    })
    
  })
}
