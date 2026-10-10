library(readr)
library(FactoMineR)
library(Factoshiny)
library(dplyr)
donnees <- read_csv("enhanced_anxiety_dataset.csv")
donnees <- donnees |>
  mutate(
    across(
      where(is.character),
      as.factor
    )
  )

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Créer les groupes d'anxiété
data_cat <- donnees %>%
  mutate(Groupe = ifelse(`Anxiety Level` (1-10) >= 8,
                         "Anxiété 8–10", "Anxiété 1–7")) %>%
  select(Groupe, Smoking, `Family History of Anxiety`,
         Dizziness, Medication, `Recent Major Life Event`) %>%
  pivot_longer(-Groupe, names_to = "Variable", values_to = "Reponse")

# 2. Calculer les pourcentages de réponses Yes
resume_cat <- data_cat %>%
  group_by(Groupe, Variable) %>%
  summarise(Pourcentage = mean(Reponse == "Yes", na.rm = TRUE) * 100,
            .groups = "drop") %>%
  mutate(Variable = recode(Variable,
                           "Smoking" = "Tabagisme",
                           "Family History of Anxiety" = "Antécédents familiaux",
                           "Dizziness" = "Vertiges",
                           "Medication" = "Médication",
                           "Recent Major Life Event" = "Événement de vie récent"
  ))

# 3. Classer les variables selon l'écart entre les groupes
ordre <- resume_cat %>%
  pivot_wider(names_from = Groupe, values_from = Pourcentage) %>%
  mutate(Ecart = abs(`Anxiété 8–10` - `Anxiété 1–7`)) %>%
  arrange(Ecart) %>%
  pull(Variable)

resume_cat$Variable <- factor(resume_cat$Variable, levels = ordre)

# 4. Graphique
graph4 <- ggplot(resume_cat,
                 aes(x = Pourcentage, y = Variable, color = Groupe)) +
  geom_line(aes(group = Variable), color = "grey80", linewidth = 2) +
  geom_point(size = 5) +
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
            hjust = -0.35, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c(
    "Anxiété 1–7" = "#3B6FB6",
    "Anxiété 8–10" = "#E45756"
  )) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  labs(
    title = "Les profils diffèrent-ils aussi sur les variables qualitatives ?",
    subtitle = "Proportion d'individus répondant « Yes » dans chacun des deux groupes",
    x = "Proportion d'individus",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

print(graph4)