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
names(donnees) <- c(
  "Âge",
  "Sexe",
  "Profession",
  "Heures de sommeil",
  "Activité physique",
  "Caféine consommée",
  "Consommation d'alcool",
  "Tabagisme",
  "Antécédents familiaux d'anxiété",
  "Niveau de stress",
  "Fréquence cardiaque",
  "Fréquence respiratoire",
  "Niveau de transpiration",
  "Vertiges",
  "Médication",
  "Séances de thérapie",
  "Événement de vie récent",
  "Qualité de l'alimentation",
  "Niveau d'anxiété"
)
# donnees_afbm <- donnees 
# anxiete <- donnees$`Niveau d'anxiété`
# res_afbm <- FAMD(
#   donnees_afbm,
#   graph = FALSE
# )
# res_factoshiny <- Factoshiny(res_afbm)
# # Factoshiny(donnees_afbm)



# prop.table(table(donnees$Profession))
library(MASS)

donnees_afd <- donnees |>
  dplyr::select(
    where(is.numeric),
    Profession
  ) |>
  na.omit()

modele_afd <- lda(
  Profession ~ .,
  data = donnees_afd
)

modele_afd
prediction <- predict(modele_afd)

coord <- as.data.frame(prediction$x)

coord$Profession <- donnees_afd$Profession
library(ggplot2)

ggplot(
  coord,
  aes(
    x = LD1,
    y = LD2,
    colour = Profession
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2.5
  ) +
  theme_classic(base_size = 13) +
  labs(
    title = "Analyse factorielle discriminante",
    subtitle = "Séparation des individus selon leur profession",
    x = "Axe discriminant 1",
    y = "Axe discriminant 2",
    colour = "Profession"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

modele_afd$scaling
coef_afd <- as.data.frame(modele_afd$scaling)

coef_afd$variable <- rownames(coef_afd)

coef_afd
library(tidyr)

coef_long <- coef_afd |>
  pivot_longer(
    cols = starts_with("LD"),
    names_to = "axe",
    values_to = "coefficient"
  )

coef_long
