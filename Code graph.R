# ============================================================
# PROJET ANXIETE
# EXPLORATION ET VISUALISATION DES DONNEES
# ============================================================
#
# Ce script regroupe les visualisations exploratoires réalisées
# avant la sélection des visualisations finales.
#
# Objectifs :
# - comprendre la structure du jeu de données
# - explorer les relations entre les variables
# - étudier l'évolution des caractéristiques avec l'anxiété
# - rechercher différents profils d'individus
# - explorer la structure multivariée avec une ACP
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)


# ============================================================
# 2. IMPORTATION DES DONNEES
# ============================================================

# Adapter le chemin si nécessaire selon l'organisation du projet

data <- read.csv(
  "data/enhanced_anxiety_dataset.csv",
  check.names = FALSE
)

# Vérification
dim(data)
names(data)
str(data)
summary(data)


# ============================================================
# 3. DISTRIBUTION DU NIVEAU D'ANXIETE
# ============================================================

graph_distribution <- ggplot(
  data,
  aes(x = `Anxiety Level (1-10)`)
) +
  
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    fill = "#4E79A7",
    color = "white"
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Distribution du niveau d'anxiété",
    x = "Niveau d'anxiété",
    y = "Nombre d'individus"
  ) +
  
  theme_minimal(base_size = 13)

print(graph_distribution)



# ============================================================
# 4. VARIABLES QUANTITATIVES
# ============================================================

variables_quantitatives <- c(
  "Age",
  "Sleep Hours",
  "Physical Activity (hrs/week)",
  "Caffeine Intake (mg/day)",
  "Alcohol Consumption (drinks/week)",
  "Stress Level (1-10)",
  "Heart Rate (bpm)",
  "Breathing Rate (breaths/min)",
  "Sweating Level (1-5)",
  "Therapy Sessions (per month)",
  "Diet Quality (1-10)",
  "Anxiety Level (1-10)"
)



# ============================================================
# 5. MATRICE DE CORRELATIONS
# ============================================================

tab_cor <- data %>%
  select(all_of(variables_quantitatives))

mat_cor <- cor(
  tab_cor,
  use = "complete.obs"
)

# Passage en format long pour ggplot

cor_long <- as.data.frame(
  as.table(mat_cor)
)

names(cor_long) <- c(
  "Variable1",
  "Variable2",
  "Correlation"
)


graph_correlation <- ggplot(
  cor_long,
  aes(
    x = Variable1,
    y = Variable2,
    fill = Correlation
  )
) +
  
  geom_tile(
    color = "white"
  ) +
  
  geom_text(
    aes(
      label = round(Correlation, 2)
    ),
    size = 3
  ) +
  
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#E45756",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  
  labs(
    title = "Relations entre les variables quantitatives",
    subtitle = "Matrice des corrélations de Pearson",
    x = NULL,
    y = NULL,
    fill = "Corrélation"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    )
  )

print(graph_correlation)



# ============================================================
# 6. MOYENNES SELON LE NIVEAU D'ANXIETE
# ============================================================

variables_profils <- c(
  "Sleep Hours",
  "Physical Activity (hrs/week)",
  "Caffeine Intake (mg/day)",
  "Alcohol Consumption (drinks/week)",
  "Stress Level (1-10)",
  "Heart Rate (bpm)",
  "Breathing Rate (breaths/min)",
  "Sweating Level (1-5)",
  "Therapy Sessions (per month)",
  "Diet Quality (1-10)"
)


evolution <- data %>%
  
  group_by(
    `Anxiety Level (1-10)`
  ) %>%
  
  summarise(
    across(
      all_of(variables_profils),
      ~ mean(.x, na.rm = TRUE)
    ),
    
    n = n(),
    
    .groups = "drop"
  )


print(evolution)



# ============================================================
# 7. EVOLUTION DU SOMMEIL SELON L'ANXIETE
# ============================================================

graph_sommeil <- ggplot(
  evolution,
  aes(
    x = `Anxiety Level (1-10)`,
    y = `Sleep Hours`
  )
) +
  
  geom_line(
    color = "#3B6FB6",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#3B6FB6",
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution du sommeil avec le niveau d'anxiété",
    x = "Niveau d'anxiété",
    y = "Sommeil moyen (heures)"
  ) +
  
  theme_minimal(base_size = 13)

print(graph_sommeil)



# ============================================================
# 8. EVOLUTION DU STRESS SELON L'ANXIETE
# ============================================================

graph_stress <- ggplot(
  evolution,
  aes(
    x = `Anxiety Level (1-10)`,
    y = `Stress Level (1-10)`
  )
) +
  
  geom_line(
    color = "#E45756",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#E45756",
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution du stress avec le niveau d'anxiété",
    x = "Niveau d'anxiété",
    y = "Stress moyen"
  ) +
  
  theme_minimal(base_size = 13)

print(graph_stress)



# ============================================================
# 9. EVOLUTION DE LA CAFEINE
# ============================================================

graph_cafeine <- ggplot(
  evolution,
  aes(
    x = `Anxiety Level (1-10)`,
    y = `Caffeine Intake (mg/day)`
  )
) +
  
  geom_line(
    color = "#F28E2B",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#F28E2B",
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution de la consommation de caféine",
    x = "Niveau d'anxiété",
    y = "Caféine moyenne (mg/jour)"
  ) +
  
  theme_minimal(base_size = 13)

print(graph_cafeine)



# ============================================================
# 10. EVOLUTION DE L'ACTIVITE PHYSIQUE
# ============================================================

graph_activite <- ggplot(
  evolution,
  aes(
    x = `Anxiety Level (1-10)`,
    y = `Physical Activity (hrs/week)`
  )
) +
  
  geom_line(
    color = "#59A14F",
    linewidth = 1.2
  ) +
  
  geom_point(
    color = "#59A14F",
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution de l'activité physique",
    x = "Niveau d'anxiété",
    y = "Activité physique moyenne (h/semaine)"
  ) +
  
  theme_minimal(base_size = 13)

print(graph_activite)



# ============================================================
# 11. PASSAGE EN FORMAT LONG POUR COMPARER LES EVOLUTIONS
# ============================================================

data_long <- data %>%
  
  select(
    `Anxiety Level (1-10)`,
    all_of(variables_profils)
  ) %>%
  
  pivot_longer(
    cols = -`Anxiety Level (1-10)`,
    names_to = "Variable",
    values_to = "Valeur"
  )


# Standardisation variable par variable

data_long <- data_long %>%
  
  group_by(Variable) %>%
  
  mutate(
    Valeur_z = as.numeric(
      scale(Valeur)
    )
  ) %>%
  
  ungroup()



# ============================================================
# 12. MOYENNES STANDARDISEES PAR NIVEAU D'ANXIETE
# ============================================================

moyennes_z <- data_long %>%
  
  group_by(
    Variable,
    `Anxiety Level (1-10)`
  ) %>%
  
  summarise(
    Moyenne_z = mean(
      Valeur_z,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )



# ============================================================
# 13. EVOLUTION STANDARDISEE DE TOUTES LES VARIABLES
# ============================================================

graph_evolution_standardisee <- ggplot(
  moyennes_z,
  aes(
    x = `Anxiety Level (1-10)`,
    y = Moyenne_z,
    color = Variable,
    group = Variable
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  geom_vline(
    xintercept = 7.5,
    linetype = "dashed",
    color = "#E45756",
    linewidth = 0.8
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution des caractéristiques avec le niveau d'anxiété",
    subtitle = "Variables standardisées pour permettre leur comparaison",
    x = "Niveau d'anxiété",
    y = "Moyenne standardisée",
    color = "Variable"
  ) +
  
  theme_minimal(base_size = 12)

print(graph_evolution_standardisee)



# ============================================================
# 14. VERSION EN FACETTES
#
# Plus lisible que toutes les courbes superposées
# ============================================================

graph_evolution_facettes <- ggplot(
  moyennes_z,
  aes(
    x = `Anxiety Level (1-10)`,
    y = Moyenne_z
  )
) +
  
  geom_line(
    color = "#3B6FB6",
    linewidth = 1
  ) +
  
  geom_point(
    color = "#3B6FB6",
    size = 2
  ) +
  
  geom_vline(
    xintercept = 7.5,
    linetype = "dashed",
    color = "#E45756"
  ) +
  
  facet_wrap(
    ~ Variable,
    ncol = 2
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution des caractéristiques selon le niveau d'anxiété",
    subtitle = "La ligne rouge indique la transition entre les niveaux 7 et 8",
    x = "Niveau d'anxiété",
    y = "Moyenne standardisée"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(graph_evolution_facettes)



# ============================================================
# 15. CREATION DES GROUPES D'ANXIETE
# ============================================================

data <- data %>%
  
  mutate(
    Groupe_anxiete = ifelse(
      `Anxiety Level (1-10)` >= 8,
      "Anxiété 8–10",
      "Anxiété 1–7"
    )
  )



# ============================================================
# 16. PROFIL STANDARDISE DES DEUX GROUPES
# ============================================================

profil_groupes <- data %>%
  
  select(
    Groupe_anxiete,
    all_of(variables_profils)
  ) %>%
  
  pivot_longer(
    cols = -Groupe_anxiete,
    names_to = "Variable",
    values_to = "Valeur"
  ) %>%
  
  group_by(Variable) %>%
  
  mutate(
    Valeur_z = as.numeric(
      scale(Valeur)
    )
  ) %>%
  
  ungroup() %>%
  
  group_by(
    Groupe_anxiete,
    Variable
  ) %>%
  
  summarise(
    Moyenne_z = mean(
      Valeur_z,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


graph_profils <- ggplot(
  profil_groupes,
  aes(
    x = Moyenne_z,
    y = reorder(Variable, Moyenne_z),
    color = Groupe_anxiete
  )
) +
  
  geom_point(
    size = 4
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  
  scale_color_manual(
    values = c(
      "Anxiété 1–7" = "#3B6FB6",
      "Anxiété 8–10" = "#E45756"
    )
  ) +
  
  labs(
    title = "Comparaison des profils selon le niveau d'anxiété",
    subtitle = "Moyennes standardisées des caractéristiques",
    x = "Moyenne standardisée",
    y = NULL,
    color = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "bottom"
  )

print(graph_profils)



# ============================================================
# 17. ACP
#
# L'ANXIETE N'EST PAS UTILISEE POUR CONSTRUIRE L'ACP
# ============================================================

tab_acp <- data %>%
  
  select(
    `Sleep Hours`,
    `Physical Activity (hrs/week)`,
    `Caffeine Intake (mg/day)`,
    `Alcohol Consumption (drinks/week)`,
    `Diet Quality (1-10)`,
    `Stress Level (1-10)`,
    `Heart Rate (bpm)`,
    `Breathing Rate (breaths/min)`,
    `Sweating Level (1-5)`
  )


res_acp <- PCA(
  tab_acp,
  scale.unit = TRUE,
  graph = FALSE
)



# ============================================================
# 18. EBOULIS DES VALEURS PROPRES
# ============================================================

graph_eigenvalues <- fviz_eig(
  res_acp,
  addlabels = TRUE
) +
  
  labs(
    title = "Part de variance expliquée par les dimensions de l'ACP"
  )

print(graph_eigenvalues)



# ============================================================
# 19. CERCLE DES VARIABLES
# ============================================================

graph_acp_variables <- fviz_pca_var(
  res_acp,
  
  col.var = "contrib",
  
  gradient.cols = c(
    "#3B6FB6",
    "#F2A541",
    "#E45756"
  ),
  
  repel = TRUE
) +
  
  labs(
    title = "Variables structurant les profils d'individus"
  )

print(graph_acp_variables)



# ============================================================
# 20. ACP DES INDIVIDUS COLORES PAR GROUPE D'ANXIETE
# ============================================================

graph_acp_individus <- fviz_pca_ind(
  res_acp,
  
  geom = "point",
  
  habillage = factor(
    data$Groupe_anxiete
  ),
  
  addEllipses = TRUE,
  
  ellipse.type = "confidence",
  
  alpha.ind = 0.25,
  
  palette = c(
    "#3B6FB6",
    "#E45756"
  )
) +
  
  labs(
    title = "Structure des profils d'individus",
    subtitle = paste0(
      "L'anxiété n'est pas utilisée pour construire l'ACP ; ",
      "elle sert uniquement à colorer les individus."
    )
  )

print(graph_acp_individus)



# ============================================================
# 21. CALCUL DES RUPTURES ENTRE NIVEAUX CONSECUTIFS
# ============================================================

ruptures <- moyennes_z %>%
  
  arrange(
    Variable,
    `Anxiety Level (1-10)`
  ) %>%
  
  group_by(Variable) %>%
  
  mutate(
    Niveau_precedent =
      lag(`Anxiety Level (1-10)`),
    
    Changement =
      Moyenne_z -
      lag(Moyenne_z),
    
    Amplitude =
      abs(Changement),
    
    Transition =
      paste0(
        Niveau_precedent,
        " → ",
        `Anxiety Level (1-10)`
      )
  ) %>%
  
  filter(
    !is.na(Changement)
  ) %>%
  
  ungroup()



# ============================================================
# 22. PLUS GRANDE RUPTURE DE CHAQUE VARIABLE
# ============================================================

plus_grande_rupture <- ruptures %>%
  
  group_by(Variable) %>%
  
  slice_max(
    order_by = Amplitude,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup() %>%
  
  arrange(
    desc(Amplitude)
  )


print(plus_grande_rupture)



# ============================================================
# 23. NOMBRE DE VARIABLES PAR TRANSITION MAXIMALE
# ============================================================

resume_transitions <- plus_grande_rupture %>%
  
  count(
    Transition,
    sort = TRUE
  )


print(resume_transitions)



# ============================================================
# 24. HEATMAP EXPLORATOIRE DES RUPTURES
# ============================================================

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


graph_heatmap_ruptures <- ggplot(
  ruptures,
  aes(
    x = Transition,
    y = Variable,
    fill = Amplitude
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  
  scale_fill_gradient(
    low = "#F5F5F5",
    high = "#D73027"
  ) +
  
  labs(
    title = "Où se produisent les principaux changements de profil ?",
    subtitle = "Amplitude standardisée entre deux niveaux d'anxiété consécutifs",
    x = "Transition",
    y = NULL,
    fill = "Amplitude"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(graph_heatmap_ruptures)



