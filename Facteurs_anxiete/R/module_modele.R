
# MODULE MODELE 
library(readr)
library(ggplot2)
library(glmnet)
library(randomForest)

# 1. Donnees 
chemins <- c("Facteurs_anxiete/data/enhanced_anxiety_dataset.csv",
             "data/enhanced_anxiety_dataset.csv",
             "enhanced_anxiety_dataset.csv")
chemin <- chemins[file.exists(chemins)][1]
if (is.na(chemin)) stop("CSV introuvable : verifier le dossier de travail.")
data <- read_csv(chemin, show_col_types = FALSE)

# Variables qualitatives : niveaux fixes sur tout le jeu de donnees.
qualitatives <- c("Gender", "Occupation", "Smoking",
                  "Family History of Anxiety", "Dizziness", "Medication",
                  "Recent Major Life Event")
data[qualitatives] <- lapply(data[qualitatives], factor)

cible <- "Anxiety Level (1-10)"

# 2. Train/test : memes individus pour tous les modeles 
set.seed(123)
indices <- sample(seq_len(nrow(data)), size = floor(0.8 * nrow(data)))
train <- data[indices, , drop = FALSE]
test  <- data[-indices, , drop = FALSE]

# 3. Formules
# Modele complet : comparaison de reference.
formule_complete <- `Anxiety Level (1-10)` ~ Age + `Sleep Hours` +
  `Physical Activity (hrs/week)` + `Caffeine Intake (mg/day)` +
  `Alcohol Consumption (drinks/week)` + `Diet Quality (1-10)` +
  `Stress Level (1-10)` + `Heart Rate (bpm)` +
  `Breathing Rate (breaths/min)` + `Sweating Level (1-5)` +
  Gender + Occupation + Smoking + `Family History of Anxiety` +
  Dizziness + Medication + `Recent Major Life Event`

# Modele retenu pour l'application : 9 variables renseignables.
formule_user <- `Anxiety Level (1-10)` ~ Age + `Sleep Hours` +
  `Physical Activity (hrs/week)` + `Caffeine Intake (mg/day)` +
  `Alcohol Consumption (drinks/week)` + `Diet Quality (1-10)` +
  `Stress Level (1-10)` + Smoking + `Recent Major Life Event`

# 4. Regression lineaire complete et modele utilisateur -------
mod_complet <- lm(formule_complete, data = train)
mod_user <- lm(formule_user, data = train)
pred_complet <- as.numeric(predict(mod_complet, newdata = test))
pred_user <- as.numeric(predict(mod_user, newdata = test))

# 5. LASSO : regularisation du modele complet ------------------
# model.matrix produit les variables indicatrices des facteurs.
x_train <- model.matrix(formule_complete, data = train)[, -1, drop = FALSE]
x_test <- model.matrix(formule_complete, data = test)[, -1, drop = FALSE]
y_train <- train[[cible]]
set.seed(123)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "gaussian",
                      nfolds = 10, type.measure = "mse")
pred_lasso_min <- as.numeric(predict(cv_lasso, newx = x_test, s = "lambda.min"))
pred_lasso_1se <- as.numeric(predict(cv_lasso, newx = x_test, s = "lambda.1se"))

# 6. Random Forest : memes 9 variables que l'application
library(randomForest)
# Définition des variables
variables_user <- c(
  "Anxiety Level (1-10)",
  "Age",
  "Sleep Hours",
  "Physical Activity (hrs/week)",
  "Caffeine Intake (mg/day)",
  "Alcohol Consumption (drinks/week)",
  "Diet Quality (1-10)",
  "Stress Level (1-10)",
  "Smoking",
  "Recent Major Life Event"
)


# Données nécessaires au modèle
train_rf <- as.data.frame(train[, variables_user, drop = FALSE])
test_rf <- as.data.frame(test[, variables_user, drop = FALSE])

# Entraînement du modèle
set.seed(123)

mod_rf <- randomForest(
  x = train_rf[, -1, drop = FALSE],
  y = train_rf[["Anxiety Level (1-10)"]],
  ntree = 500,
  importance = TRUE
)

# Prédictions
pred_rf <- predict(
  mod_rf,
  newdata = test_rf[, -1, drop = FALSE]
)

# Évaluation
y_test <- test_rf[["Anxiety Level (1-10)"]]

RMSE_rf <- sqrt(mean((y_test - pred_rf)^2))
MAE_rf <- mean(abs(y_test - pred_rf))
R2_rf <- 1 - sum((y_test - pred_rf)^2) /
  sum((y_test - mean(y_test))^2)

# Résultats
cat("RMSE :", RMSE_rf, "\n")
cat("MAE :", MAE_rf, "\n")
cat("R² :", R2_rf, "\n")
# 7. Evaluation sur le meme jeu test 
# RMSE et MAE restent utiles ici POUR COMPARER les modeles ;
# ils ne sont pas affiches sur la page Shiny.
mesures <- function(observe, predit) {
  c(RMSE = sqrt(mean((observe - predit)^2)),
    MAE = mean(abs(observe - predit)),
    R2 = 1 - sum((observe - predit)^2) / sum((observe - mean(observe))^2))
}

y_test <- test[[cible]]
predictions <- list(
  "Lineaire complet" = pred_complet,
  "LASSO lambda.min" = pred_lasso_min,
  "LASSO lambda.1se" = pred_lasso_1se,
  "Lineaire utilisateur (9 variables)" = pred_user,
  "Random Forest (9 variables)" = pred_rf
)
resultats <- as.data.frame(do.call(rbind, lapply(predictions, function(p) mesures(y_test, p))))
resultats$Modele <- rownames(resultats)
rownames(resultats) <- NULL
print(resultats[, c("Modele", "R2", "RMSE", "MAE")], row.names = FALSE)

# 8. Graphique pour l'oral : comparaison du R2 -----------------
graph_comparaison <- ggplot(resultats, aes(x = reorder(Modele, R2), y = R2)) +
  geom_col(fill = "#4E79A7", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", R2)), hjust = -0.15, size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(min(0, min(resultats$R2) - 0.05),
                                max(1, max(resultats$R2) + 0.08))) +
  labs(title = "Comparaison des modeles sur le jeu de test",
       subtitle = "Meme separation train/test pour tous les modeles",
       x = NULL, y = "R² sur le jeu de test") +
  theme_minimal(base_size = 12)
print(graph_comparaison)

# 9. Graphique pour l'oral : predictions du modele retenu ------
graph_predictions <- ggplot(
  data.frame(Observe = y_test, Predit = pred_user),
  aes(x = Observe, y = Predit)
) +
  geom_point(alpha = 0.2, color = "#4E79A7") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#E15759") +
  labs(title = "Modele utilisateur : observe vs predit",
       x = "Niveau d'anxiete observe", y = "Niveau d'anxiete predit") +
  theme_minimal(base_size = 12)
print(graph_predictions)

# 10. Elements utiles a commenter a l'oral
cat("\nVariables du modele utilisateur :\n")
print(attr(terms(formule_user), "term.labels"))
cat("\nNombre de coefficients LASSO non nuls (hors intercept) :\n")
print(c(lambda_min = sum(as.vector(coef(cv_lasso, s = "lambda.min"))[-1] != 0),
        lambda_1se = sum(as.vector(coef(cv_lasso, s = "lambda.1se"))[-1] != 0)))

