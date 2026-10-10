# ============================================================

# ENTRAINEMENT ET EVALUATION DES MODELES

# Ce script est à exécuter manuellement, pas dans Shiny.

# ============================================================

# Packages ----------------------------------------------------

library(readr)
library(dplyr)
library(glmnet)
library(randomForest)
library(tidyr)

# 1. Importation et préparation des données -------------------

donnees <- read_csv("Facteurs_anxiete/data/enhanced_anxiety_dataset.csv")

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

# 2. Séparation apprentissage / test ---------------------------

set.seed(123)

n <- nrow(donnees)

train_index <- sample(
  seq_len(n),
  size = floor(0.8 * n)
)

train <- donnees[train_index, ]
test  <- donnees[-train_index, ]

# Fonction pour calculer les métriques ------------------------

calculer_metriques <- function(observe, predit) {
  
  observe <- as.numeric(observe)
  predit <- as.numeric(predit)
  
  tibble(
    RMSE = sqrt(mean((observe - predit)^2)),
    MAE = mean(abs(observe - predit)),
    R2 = 1 - sum((observe - predit)^2) /
      sum((observe - mean(observe))^2)
  )
}

# Formules ----------------------------------------------------

formule_complete <- `Niveau d'anxiété` ~
  `Âge` +
  `Heures de sommeil` +
  `Activité physique` +
  `Caféine consommée` +
  `Consommation d'alcool` +
  `Qualité de l'alimentation` +
  `Niveau de stress` +
  `Fréquence cardiaque` +
  `Fréquence respiratoire` +
  `Niveau de transpiration` +
  `Sexe` +
  `Profession` +
  `Tabagisme` +
  `Antécédents familiaux d'anxiété` +
  `Vertiges` +
  `Médication` +
  `Événement de vie récent`

formule_utilisateur <- `Niveau d'anxiété` ~
  `Âge` +
  `Heures de sommeil` +
  `Activité physique` +
  `Caféine consommée` +
  `Consommation d'alcool` +
  `Qualité de l'alimentation` +
  `Niveau de stress` +
  `Tabagisme` +
  `Événement de vie récent`

# 3. Régression linéaire complète ------------------------------

mod_complet <- lm(
  formule_complete,
  data = train
)

pred_complet <- predict(
  mod_complet,
  newdata = test
)

metriques_complet <- calculer_metriques(
  test$`Niveau d'anxiété`,
  pred_complet
)

# 4. Régression LASSO ------------------------------------------

# Construction des matrices à partir de la formule complète.

# Les facteurs conservent les niveaux définis dans les données.

x_train <- model.matrix(
  formule_complete,
  data = train
)[, -1, drop = FALSE]

x_test <- model.matrix(
  formule_complete,
  data = test
)[, -1, drop = FALSE]

y_train <- train$`Niveau d'anxiété`

set.seed(123)

cv_lasso <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1,
  family = "gaussian",
  type.measure = "mse",
  nfolds = 10
)

pred_lasso_min <- predict(
  cv_lasso,
  newx = x_test,
  s = "lambda.min"
)

pred_lasso_1se <- predict(
  cv_lasso,
  newx = x_test,
  s = "lambda.1se"
)

metriques_lasso_min <- calculer_metriques(
  test$`Niveau d'anxiété`,
  pred_lasso_min
)

metriques_lasso_1se <- calculer_metriques(
  test$`Niveau d'anxiété`,
  pred_lasso_1se
)

# 5. Modèle utilisateur ----------------------------------------

mod_user <- lm(
  formule_utilisateur,
  data = train
)

pred_user <- predict(
  mod_user,
  newdata = test
)

metriques_user <- calculer_metriques(
  test$`Niveau d'anxiété`,
  pred_user
)

# 6. Random Forest ---------------------------------------------

# Même sélection de variables que le modèle utilisateur

# afin de rendre la comparaison plus cohérente.

variables_user <- c(
  "Âge",
  "Heures de sommeil",
  "Activité physique",
  "Caféine consommée",
  "Consommation d'alcool",
  "Qualité de l'alimentation",
  "Niveau de stress",
  "Tabagisme",
  "Événement de vie récent"
)

x_train_rf <- train |>
  select(all_of(variables_user))

x_test_rf <- test |>
  select(all_of(variables_user))

y_train_rf <- train$`Niveau d'anxiété`
y_test_rf <- test$`Niveau d'anxiété`

set.seed(123)

rf_user <- randomForest(
  x = x_train_rf,
  y = y_train_rf,
  ntree = 500,
  importance = TRUE
)

pred_rf <- predict(
  rf_user,
  newdata = x_test_rf
)

metriques_rf <- calculer_metriques(
  y_test_rf,
  pred_rf
)

# 7. Tableau comparatif des performances -----------------------

resultats <- bind_rows(
  "Régression linéaire complète" = metriques_complet,
  "LASSO lambda.min" = metriques_lasso_min,
  "LASSO lambda.1se" = metriques_lasso_1se,
  "Modèle utilisateur" = metriques_user,
  "Random Forest" = metriques_rf,
  .id = "Modèle"
)

print(resultats)

# 8. Sauvegarde des modèles et résultats -----------------------

modeles <- list(
  mod_complet = mod_complet,
  cv_lasso = cv_lasso,
  mod_user = mod_user,
  rf_user = rf_user,
  resultats = resultats
)

saveRDS(
  modeles,
  "Facteurs_anxiete/models/modeles_anxiete.rds"
)

cat("Modèles et résultats sauvegardés dans Facteurs_anxiete/models/modeles_anxiete.rds\n")
