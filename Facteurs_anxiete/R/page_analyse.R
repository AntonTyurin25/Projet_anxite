# ============================================================
# PAGE ANALYSE
# Exploration de la structure des données
# ============================================================


page_analyse_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    # ========================================================
    # TITRE GENERAL
    # ========================================================
    
    h2(
      "Exploration de la structure des données",
      class = "accueil-titre"
    ),
    
    p(
      "Cette partie explore la structure du jeu de données afin ",
      "d'identifier les principales relations entre les variables ",
      "et de rechercher d'éventuels profils associés au niveau d'anxiété."
    ),
    
    # ========================================================
    # SOUS-PAGES
    # ========================================================
    
    tabsetPanel(
      
      id = ns("sous_page"),
      
      # ======================================================
      # 1. STRUCTURE GENERALE
      # ======================================================
      
      tabPanel(
        "Structure générale",
        
        br(),
        
        h3("Distribution du niveau d'anxiété"),
        
        p(
          "Cette première visualisation permet d'observer comment ",
          "les individus sont répartis selon leur niveau d'anxiété."
        ),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("distribution_anxiete"),
            width = "100%",
            height = "450px"
          ),
          
          p(
            "[À compléter : décrire ici la forme de la distribution. ",
            "Par exemple : les niveaux d'anxiété sont-ils répartis ",
            "de manière homogène ou certaines valeurs sont-elles ",
            "nettement plus représentées ?]"
          )
        ),
        
        br(),
        
        h3("Relations entre les variables quantitatives"),
        
        p(
          "La matrice de corrélations permet d'étudier simultanément ",
          "les relations linéaires entre les principales variables quantitatives."
        ),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("correlations"),
            width = "100%",
            height = "750px"
          ),
          
          p(
            "[À compléter : identifier les principales corrélations ",
            "positives et négatives observées dans la matrice. ",
            "Indiquer notamment les relations les plus fortes avec ",
            "le niveau d'anxiété.]"
          )
        )
      ),
      
      
      # ======================================================
      # 2. EVOLUTION AVEC L'ANXIETE
      # ======================================================
      
      tabPanel(
        "Évolution avec l'anxiété",
        
        br(),
        
        h3("Évolution de caractéristiques individuelles"),
        
        p(
          "Les graphiques suivants représentent les valeurs moyennes ",
          "de plusieurs caractéristiques pour chaque niveau d'anxiété."
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Sommeil",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              ns("sommeil"),
              width = "100%",
              height = "350px"
            ),
            
            p(
              "[À compléter : décrire l'évolution du nombre moyen ",
              "d'heures de sommeil lorsque le niveau d'anxiété augmente.]"
            )
          ),
          
          box(
            width = 6,
            title = "Stress",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              ns("stress"),
              width = "100%",
              height = "350px"
            ),
            
            p(
              "[À compléter : décrire l'évolution du niveau de stress ",
              "moyen selon le niveau d'anxiété.]"
            )
          )
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Caféine",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              ns("cafeine"),
              width = "100%",
              height = "350px"
            ),
            
            p(
              "[À compléter : décrire l'évolution de la consommation ",
              "moyenne de caféine selon le niveau d'anxiété.]"
            )
          ),
          
          box(
            width = 6,
            title = "Activité physique",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              ns("activite"),
              width = "100%",
              height = "350px"
            ),
            
            p(
              "[À compléter : décrire l'évolution de l'activité physique ",
              "moyenne selon le niveau d'anxiété.]"
            )
          )
        ),
        
        br(),
        
        h3("Comparaison de l'ensemble des caractéristiques"),
        
        p(
          "Les variables sont ici standardisées afin de pouvoir ",
          "comparer leur évolution malgré leurs unités et leurs échelles différentes."
        ),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("evolution_facettes"),
            width = "100%",
            height = "850px"
          ),
          
          p(
            "[À compléter : identifier les variables présentant les ",
            "évolutions les plus nettes. Une attention particulière ",
            "peut être portée à la transition entre les niveaux 7 et 8.]"
          )
        )
      ),
      
      
      # ======================================================
      # 3. PROFILS D'ANXIETE
      # ======================================================
      
      tabPanel(
        "Profils d'anxiété",
        
        br(),
        
        h3("Deux groupes de niveaux d'anxiété"),
        
        p(
          "L'exploration précédente suggère de distinguer deux groupes : ",
          "les niveaux d'anxiété de 1 à 7 et ceux de 8 à 10. ",
          "Cette séparation permet de comparer les caractéristiques ",
          "moyennes de ces deux profils."
        ),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("profils"),
            width = "100%",
            height = "600px"
          ),
          
          p(
            "[À compléter : décrire les caractéristiques qui distinguent ",
            "le plus les deux groupes. Indiquer quelles variables sont ",
            "plus élevées ou plus faibles dans le groupe 8–10.]"
          )
        ),
        
        br(),
        
        h3("Interprétation"),
        
        box(
          width = 12,
          status = "info",
          solidHeader = TRUE,
          
          p(
            "Cette comparaison permet de rechercher des profils associés ",
            "aux niveaux d'anxiété élevés. Elle ne permet cependant pas ",
            "de déterminer si ces caractéristiques sont la cause de ",
            "l'anxiété ou simplement associées à celle-ci."
          )
        )
      ),
      
      
      # ======================================================
      # 4. STRUCTURE MULTIVARIEE
      # ======================================================
      
      tabPanel(
        "Structure multivariée",
        
        br(),
        
        h3("Analyse en composantes principales"),
        
        p(
          "L'ACP permet de résumer simultanément plusieurs variables ",
          "quantitatives en quelques dimensions. Le niveau d'anxiété ",
          "n'est pas utilisé pour construire les axes."
        ),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          h4("Variance expliquée"),
          
          plotOutput(
            ns("eigenvalues"),
            width = "100%",
            height = "400px"
          ),
          
          p(
            "[À compléter : indiquer combien de dimensions expliquent ",
            "une part importante de la variabilité des données.]"
          )
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Variables",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              ns("acp_variables"),
              width = "100%",
              height = "500px"
            ),
            
            p(
              "[À compléter : identifier les variables contribuant ",
              "le plus aux premières dimensions de l'ACP.]"
            )
          ),
          
          box(
            width = 6,
            title = "Individus",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              ns("acp_individus"),
              width = "100%",
              height = "500px"
            ),
            
            p(
              "[À compléter : indiquer si les deux groupes d'anxiété ",
              "semblent former des groupes distincts dans l'espace ",
              "des caractéristiques quantitatives.]"
            )
          )
        ),
        
        br(),
        
        h3("Ruptures entre niveaux d'anxiété"),
        
        p(
          "On cherche ici à identifier les transitions entre niveaux ",
          "d'anxiété pour lesquelles les caractéristiques moyennes ",
          "changent le plus fortement."
        ),
        
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          plotOutput(
            ns("heatmap_ruptures"),
            width = "100%",
            height = "650px"
          ),
          
          p(
            "[À compléter : identifier les transitions présentant ",
            "les changements les plus importants et vérifier si ",
            "elles se concentrent autour d'une transition particulière, ",
            "notamment entre les niveaux 7 et 8.]"
          )
        )
      )
    )
  )
}



# ============================================================
# SERVEUR
# ============================================================

page_analyse_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # ========================================================
    # 1. PREPARATION DES DONNEES
    # ========================================================
    
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
    
    
    # ========================================================
    # 2. DISTRIBUTION DU NIVEAU D'ANXIETE
    # ========================================================
    
    output$distribution_anxiete <- renderPlot({
      
      ggplot2::ggplot(
        donnees,
        ggplot2::aes(x = `Niveau d'anxiété`)
      ) +
        
        ggplot2::geom_histogram(
          binwidth = 1,
          boundary = 0.5,
          fill = "#4E79A7",
          color = "white"
        ) +
        
        ggplot2::scale_x_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::labs(
          title = "Distribution du niveau d'anxiété",
          x = "Niveau d'anxiété",
          y = "Nombre d'individus"
        ) +
        
        ggplot2::theme_minimal(base_size = 13)
    })
    
    
    # ========================================================
    # 3. MATRICE DE CORRELATIONS
    # ========================================================
    
    output$correlations <- renderPlot({
      
      tab_cor <- donnees |>
        dplyr::select(
          dplyr::all_of(variables_quantitatives)
        )
      
      mat_cor <- cor(
        tab_cor,
        use = "complete.obs"
      )
      
      cor_long <- as.data.frame(
        as.table(mat_cor)
      )
      
      names(cor_long) <- c(
        "Variable1",
        "Variable2",
        "Correlation"
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
          color = "white"
        ) +
        
        ggplot2::geom_text(
          ggplot2::aes(
            label = round(Correlation, 2)
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
        
        ggplot2::labs(
          title = "Relations entre les variables quantitatives",
          subtitle = "Matrice des corrélations de Pearson",
          x = NULL,
          y = NULL,
          fill = "Corrélation"
        ) +
        
        ggplot2::theme_minimal(base_size = 11) +
        
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          )
        )
    })
    
    
    # ========================================================
    # 4. CALCUL DES MOYENNES PAR NIVEAU D'ANXIETE
    # ========================================================
    
    evolution <- donnees |>
      dplyr::group_by(
        `Niveau d'anxiété`
      ) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(variables_profils),
          ~ mean(.x, na.rm = TRUE)
        ),
        n = dplyr::n(),
        .groups = "drop"
      )
    
    
    # ========================================================
    # 5. SOMMEIL
    # ========================================================
    
    output$sommeil <- renderPlot({
      
      ggplot2::ggplot(
        evolution,
        ggplot2::aes(
          x = `Niveau d'anxiété`,
          y = `Heures de sommeil`
        )
      ) +
        
        ggplot2::geom_line(
          color = "#3B6FB6",
          linewidth = 1.2
        ) +
        
        ggplot2::geom_point(
          color = "#3B6FB6",
          size = 3
        ) +
        
        ggplot2::scale_x_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::labs(
          title = "Évolution du sommeil avec le niveau d'anxiété",
          x = "Niveau d'anxiété",
          y = "Sommeil moyen (heures)"
        ) +
        
        ggplot2::theme_minimal(base_size = 13)
    })
    
    
    # ========================================================
    # 6. STRESS
    # ========================================================
    
    output$stress <- renderPlot({
      
      ggplot2::ggplot(
        evolution,
        ggplot2::aes(
          x = `Niveau d'anxiété`,
          y = `Niveau de stress`
        )
      ) +
        
        ggplot2::geom_line(
          color = "#E45756",
          linewidth = 1.2
        ) +
        
        ggplot2::geom_point(
          color = "#E45756",
          size = 3
        ) +
        
        ggplot2::scale_x_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::labs(
          title = "Évolution du stress avec le niveau d'anxiété",
          x = "Niveau d'anxiété",
          y = "Stress moyen"
        ) +
        
        ggplot2::theme_minimal(base_size = 13)
    })
    
    
    # ========================================================
    # 7. CAFEINE
    # ========================================================
    
    output$cafeine <- renderPlot({
      
      ggplot2::ggplot(
        evolution,
        ggplot2::aes(
          x = `Niveau d'anxiété`,
          y = `Caféine consommée`
        )
      ) +
        
        ggplot2::geom_line(
          color = "#F28E2B",
          linewidth = 1.2
        ) +
        
        ggplot2::geom_point(
          color = "#F28E2B",
          size = 3
        ) +
        
        ggplot2::scale_x_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::labs(
          title = "Évolution de la consommation de caféine",
          x = "Niveau d'anxiété",
          y = "Caféine moyenne (mg/jour)"
        ) +
        
        ggplot2::theme_minimal(base_size = 13)
    })
    
    
    # ========================================================
    # 8. ACTIVITE PHYSIQUE
    # ========================================================
    
    output$activite <- renderPlot({
      
      ggplot2::ggplot(
        evolution,
        ggplot2::aes(
          x = `Niveau d'anxiété`,
          y = `Activité physique`
        )
      ) +
        
        ggplot2::geom_line(
          color = "#59A14F",
          linewidth = 1.2
        ) +
        
        ggplot2::geom_point(
          color = "#59A14F",
          size = 3
        ) +
        
        ggplot2::scale_x_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::labs(
          title = "Évolution de l'activité physique",
          x = "Niveau d'anxiété",
          y = "Activité physique moyenne (h/semaine)"
        ) +
        
        ggplot2::theme_minimal(base_size = 13)
    })
    
    
    # ========================================================
    # 9. DONNEES STANDARDISEES
    # ========================================================
    
    data_long <- donnees |>
      dplyr::select(
        `Niveau d'anxiété`,
        dplyr::all_of(variables_profils)
      ) |>
      tidyr::pivot_longer(
        cols = -`Niveau d'anxiété`,
        names_to = "Variable",
        values_to = "Valeur"
      ) |>
      dplyr::group_by(Variable) |>
      dplyr::mutate(
        Valeur_z = as.numeric(
          scale(Valeur)
        )
      ) |>
      dplyr::ungroup()
    
    
    moyennes_z <- data_long |>
      dplyr::group_by(
        Variable,
        `Niveau d'anxiété`
      ) |>
      dplyr::summarise(
        Moyenne_z = mean(
          Valeur_z,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    
    # ========================================================
    # 10. EVOLUTION STANDARDISEE EN FACETTES
    # ========================================================
    
    output$evolution_facettes <- renderPlot({
      
      ggplot2::ggplot(
        moyennes_z,
        ggplot2::aes(
          x = `Niveau d'anxiété`,
          y = Moyenne_z
        )
      ) +
        
        ggplot2::geom_line(
          color = "#3B6FB6",
          linewidth = 1
        ) +
        
        ggplot2::geom_point(
          color = "#3B6FB6",
          size = 2
        ) +
        
        ggplot2::geom_vline(
          xintercept = 7.5,
          linetype = "dashed",
          color = "#E45756"
        ) +
        
        ggplot2::scale_x_continuous(
          breaks = 1:10
        ) +
        
        ggplot2::facet_wrap(
          ~ Variable,
          ncol = 2
        ) +
        
        ggplot2::labs(
          title = "Évolution des caractéristiques selon le niveau d'anxiété",
          subtitle = "Variables standardisées pour permettre leur comparaison",
          x = "Niveau d'anxiété",
          y = "Moyenne standardisée"
        ) +
        
        ggplot2::theme_minimal(base_size = 11) +
        
        ggplot2::theme(
          strip.text = ggplot2::element_text(
            face = "bold"
          )
        )
    })
    
    
    # ========================================================
    # 11. CREATION DES DEUX GROUPES
    # ========================================================
    
    donnees_profils <- donnees |>
      dplyr::mutate(
        Groupe_anxiete = dplyr::if_else(
          `Niveau d'anxiété` >= 8,
          "Anxiété 8–10",
          "Anxiété 1–7"
        )
      )
    
    
    # ========================================================
    # 12. PROFILS STANDARDISES
    # ========================================================
    
    profil_groupes <- donnees_profils |>
      dplyr::select(
        Groupe_anxiete,
        dplyr::all_of(variables_profils)
      ) |>
      tidyr::pivot_longer(
        cols = -Groupe_anxiete,
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
    
    
    # ========================================================
    # 13. GRAPHIQUE DES PROFILS
    # ========================================================
    
    output$profils <- renderPlot({
      
      ggplot2::ggplot(
        profil_groupes,
        ggplot2::aes(
          x = Moyenne_z,
          y = reorder(
            Variable,
            Moyenne_z
          ),
          color = Groupe_anxiete
        )
      ) +
        
        ggplot2::geom_point(
          size = 4
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
          title = "Comparaison des profils selon le niveau d'anxiété",
          subtitle = "Moyennes standardisées des caractéristiques",
          x = "Moyenne standardisée",
          y = NULL,
          color = NULL
        ) +
        
        ggplot2::theme_minimal(base_size = 13) +
        
        ggplot2::theme(
          legend.position = "bottom"
        )
    })
    
    
    # ========================================================
    # 14. ACP
    # ========================================================
    
    tab_acp <- donnees |>
      dplyr::select(
        `Heures de sommeil`,
        `Activité physique`,
        `Caféine consommée`,
        `Consommation d'alcool`,
        `Qualité de l'alimentation`,
        `Niveau de stress`,
        `Fréquence cardiaque`,
        `Fréquence respiratoire`,
        `Niveau de transpiration`
      )
    
    
    res_acp <- FactoMineR::PCA(
      tab_acp,
      ncp = 9,
      scale.unit = TRUE,
      graph = FALSE
    )
    
    
    # ========================================================
    # 15. EBOULIS
    # ========================================================
    
    output$eigenvalues <- renderPlot({
      
      factoextra::fviz_eig(
        res_acp,
        ncp = 9,
        addlabels = TRUE
      ) +
        
        ggplot2::labs(
          title = "Part de variance expliquée par les dimensions de l'ACP"
        )
    })
    
    
    # ========================================================
    # 16. VARIABLES DE L'ACP
    # ========================================================
    
    output$acp_variables <- renderPlot({
      
      factoextra::fviz_pca_var(
        res_acp,
        col.var = "contrib",
        gradient.cols = c(
          "#3B6FB6",
          "#F2A541",
          "#E45756"
        ),
        repel = TRUE
      ) +
        
        ggplot2::labs(
          title = "Variables structurant les profils d'individus"
        )
    })
    
    
    # ========================================================
    # 17. INDIVIDUS DE L'ACP
    # ========================================================
    
    output$acp_individus <- renderPlot({
      
      factoextra::fviz_pca_ind(
        res_acp,
        
        geom = "point",
        
        habillage = factor(
          donnees_profils$Groupe_anxiete
        ),
        
        addEllipses = TRUE,
        
        ellipse.type = "confidence",
        
        alpha.ind = 0.25,
        
        palette = c(
          "#3B6FB6",
          "#E45756"
        )
      ) +
        
        ggplot2::labs(
          title = "Structure des profils d'individus",
          subtitle = paste0(
            "L'anxiété n'est pas utilisée pour construire l'ACP ; ",
            "elle sert uniquement à colorer les individus."
          )
        )
    })
    
    
    # ========================================================
    # 18. CALCUL DES RUPTURES
    # ========================================================
    
    ruptures <- moyennes_z |>
      dplyr::arrange(
        Variable,
        `Niveau d'anxiété`
      ) |>
      dplyr::group_by(Variable) |>
      dplyr::mutate(
        Niveau_precedent = lag(
          `Niveau d'anxiété`
        ),
        
        Changement =
          Moyenne_z - lag(Moyenne_z),
        
        Amplitude =
          abs(Changement),
        
        Transition = paste0(
          Niveau_precedent,
          " → ",
          `Niveau d'anxiété`
        )
      ) |>
      dplyr::filter(
        !is.na(Changement)
      ) |>
      dplyr::ungroup()
    
    
    # ========================================================
    # 19. HEATMAP DES RUPTURES
    # ========================================================
    
    ruptures$Transition <- factor(
      ruptures$Transition,
      
      levels = c(
        "1 → 2",
        "2 → 3",
        "3 → 4",
        "4 → 5",
        "5 → 6",
        "6 → 7",
        "7 → 8",
        "8 → 9",
        "9 → 10"
      )
    )
    
    
    output$heatmap_ruptures <- renderPlot({
      
      ggplot2::ggplot(
        ruptures,
        ggplot2::aes(
          x = Transition,
          y = Variable,
          fill = Amplitude
        )
      ) +
        
        ggplot2::geom_tile(
          color = "white",
          linewidth = 0.7
        ) +
        
        ggplot2::scale_fill_gradient(
          low = "#F5F5F5",
          high = "#D73027"
        ) +
        
        ggplot2::labs(
          title = "Où se produisent les principaux changements de profil ?",
          subtitle = paste0(
            "Amplitude standardisée entre deux niveaux d'anxiété consécutifs"
          ),
          x = "Transition",
          y = NULL,
          fill = "Amplitude"
        ) +
        
        ggplot2::theme_minimal(base_size = 12) +
        
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          )
        )
    })
    
  })
}