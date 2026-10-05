
data <- read_csv("enhanced_anxiety_dataset.csv")


# on regarde la sturcture du modèle 
str(data)
summary(data)

# on regarde les modalitées des variables quali 
unique(data$Gender)
unique(data$Occupation)
unique(data$Dizziness)
unique(data$Medication)
unique(data$`Recent Major Life Event`)
unique(data$Smoking)
unique(data$`Family History of Anxiety`)

#1. ANALYSE DES CORRÉLATIONS AVEC LE NIVEAU D'ANXIÉTÉ

# L'objectif est d'identifier, dans un premier temps,
# les variables quantitatives qui présentent une association
# avec le niveau d'anxiété.
#
# On calcule donc la corrélation entre chaque variable quantitative
# et la variable cible : "Anxiety Level (1-10)".

install.packages("GGally")

 
 
 vars_num <- c(
   "Age",
   "Sleep Hours",
   "Physical Activity (hrs/week)",
   "Caffeine Intake (mg/day)",
   "Alcohol Consumption (drinks/week)",
   "Diet Quality (1-10)",
   "Stress Level (1-10)",
   "Heart Rate (bpm)",
   "Breathing Rate (breaths/min)",
   "Sweating Level (1-5)"
 )
 
 cor_anxiety <- sapply(
   data[, vars_num],
   function(x) cor(x, data$`Anxiety Level (1-10)`)
 )
 
 cor_df <- data.frame(
   Variable = vars_num,
   Correlation = as.numeric(cor_anxiety)
 )
 
 cor_df
 
# GRAPHIQUEMENT 
 # Représentation des corrélations avec le niveau d'anxiété.
 # Cette visualisation permet de comparer facilement la force
 # et le sens des associations entre les variables quantitatives
 # et le niveau d'anxiété.
 
 ggplot(
   cor_df,
   aes(
     x = reorder(Variable, Correlation),
     y = Correlation
   )
 ) +
   geom_col() +
   coord_flip() +
   labs(
     x = NULL,
     y = "Corrélation avec le niveau d'anxiété",
     title = "Variables quantitatives associées au niveau d'anxiété"
   ) +
   theme_minimal()
 
 #L'analyse des corrélations met en évidence des associations de différentes intensités avec le niveau d'anxiété. 
 #Le niveau de stress présente l'association positive la plus importante, tandis que le nombre d'heures de sommeil présente 
 #une association négative relativement forte. 
 #La consommation de caféine présente également une association positive, 
 #tandis que l'activité physique et la qualité de l'alimentation présentent des associations négatives plus modérées. 
 #Ces résultats constituent une première exploration des relations entre les variables, 
 #mais ne permettent pas à eux seuls de conclure à des effets causaux.
 
 # 2.CONSTRUCTION DU MODELE DU REGRESSION 
 
 # L'objectif est maintenant d'étudier simultanément les relations
 # entre les différentes variables quantitatives et le niveau
 # d'anxiété.
 #
 # Contrairement aux corrélations précédentes, la régression permet
 # d'estimer l'association entre chaque variable et l'anxiété
 # en tenant compte simultanément des autres variables du modèle.
 
 mod1 <- lm(
   `Anxiety Level (1-10)` ~
     Age +
     `Sleep Hours` +
     `Physical Activity (hrs/week)` +
     `Caffeine Intake (mg/day)` +
     `Alcohol Consumption (drinks/week)` +
     `Diet Quality (1-10)` +
     `Stress Level (1-10)` +
     `Heart Rate (bpm)` +
     `Breathing Rate (breaths/min)` +
     `Sweating Level (1-5)`,
   data = data
 )
 
 summary(mod1)
 
 # le modèle explique environ 66,5 % de la variabilité du niveau d'anxiété :
 
 #R² = 0,6654
# R² ajusté = 0,6651
 #erreur résiduelle ≈ 1,23 point d'anxiété

#C'est donc un premier modèle qui explique une part importante de la variabilité observée dans les données.
 
 #Le test global du modèle est également significatif (p < 2.2e-16), mais avec 11 000 observations, les p-values seules ne doivent surtout pas être notre critère principal de sélection
 
 
 # tt les p- valus de chacune des varaibles sont significatives mais l'echantillon est tres grand .
 
 # 3. VÉRIFICATION DE LA MULTICOLINÉARITÉ
 # ============================================================
 
 # Le VIF permet d'évaluer si certaines variables explicatives
 # sont fortement corrélées entre elles.
 #
 # Des valeurs proches de 1 indiquent une faible redondance
 # entre les variables explicatives.

 
 vif(mod1)
 
 #Tous les VIF sont proches de 1 → pas de problème apparent de multicolinéarité entre vos variables quantitatives.
 
 # voir les variables quali 
 str(data)
 sapply(data, class)
 
 
 #5. PRÉPARATION DES VARIABLES QUALITATIVES

 
 vars_cat <- c(
   "Gender",
   "Occupation",
   "Smoking",
   "Family History of Anxiety",
   "Dizziness",
   "Medication",
   "Recent Major Life Event"
 )
 
 data[vars_cat] <- lapply(data[vars_cat], factor)
 
 sapply(data[vars_cat], class)
 lapply(data[vars_cat], levels) 

 
 # graph 
 
 

 
 data_cat <- data %>%
   select(
     `Anxiety Level (1-10)`,
     Gender,
     Smoking,
     `Family History of Anxiety`,
     Dizziness,
     Medication,
     `Recent Major Life Event`
   ) %>%
   pivot_longer(
     cols = -`Anxiety Level (1-10)`,
     names_to = "Variable",
     values_to = "Modalite"
   )
 
 ggplot(
   data_cat,
   aes(
     x = Modalite,
     y = `Anxiety Level (1-10)`
   )
 ) +
   geom_boxplot() +
   facet_wrap(~ Variable, scales = "free_x") +
   theme_minimal() +
   labs(
     x = NULL,
     y = "Niveau d'anxiété",
     title = "Niveau d'anxiété selon les variables qualitatives"
   ) 

 # et la variable occupation parcequ'il ya 13 niveaux 
 
 ggplot(
   data,
   aes(
     x = reorder(
       Occupation,
       `Anxiety Level (1-10)`,
       FUN = median
     ),
     y = `Anxiety Level (1-10)`
   )
 ) +
   geom_boxplot() +
   coord_flip() +
   theme_minimal() +
   labs(
     x = "Occupation",
     y = "Niveau d'anxiété",
     title = "Niveau d'anxiété selon l'occupation"
    
   )
   
   
   
# ============================================================
# 5. ANALYSE DES VARIABLES QUALITATIVES


# On étudie simultanément l'association entre les variables
# qualitatives et le niveau d'anxiété.
#
# Une ANOVA multifactorielle permet de comparer le niveau moyen
# d'anxiété entre les différentes modalités de chaque variable,
# tout en tenant compte simultanément des autres variables
# qualitatives du modèle.

mod_anova <- aov(
  `Anxiety Level (1-10)` ~
    Gender +
    Occupation +
    Smoking +
    `Family History of Anxiety` +
    Dizziness +
    Medication +
    `Recent Major Life Event`,
  data = data
)

summary(mod_anova) # Les résultats montrent une association statistiquement significative pour l'occupation, le tabagisme, les antécédents familiaux d'anxiété, les vertiges, la prise de médicaments et la survenue récente d'un événement majeur.
#En revanche, aucune différence statistiquement significative n'est observée selon le genre (p = 0,974).


# Regression complete 

mod_complet <- lm(
  `Anxiety Level (1-10)` ~
    Age +
    `Sleep Hours` +
    `Physical Activity (hrs/week)` +
    `Caffeine Intake (mg/day)` +
    `Alcohol Consumption (drinks/week)` +
    `Diet Quality (1-10)` +
    `Stress Level (1-10)` +
    `Heart Rate (bpm)` +
    `Breathing Rate (breaths/min)` +
    `Sweating Level (1-5)` +
    Gender +
    Occupation +
    Smoking +
    `Family History of Anxiety` +
    Dizziness +
    Medication +
    `Recent Major Life Event`,
  data = data
)

summary(mod_complet)


# ============================================================
# 6. SÉPARATION TRAIN / TEST
# ============================================================

set.seed(123)

n <- nrow(data)

train_index <- sample(
  1:n,
  size = 0.8 * n
)

train <- data[train_index, ]
test <- data[-train_index, ]

dim(train)
dim(test)


# on reconstruis le modèle sur TRAIN uniquement
mod_train <- lm(
`Anxiety Level (1-10)` ~
  Age +
  `Sleep Hours` +
  `Physical Activity (hrs/week)` +
  `Caffeine Intake (mg/day)` +
  `Alcohol Consumption (drinks/week)` +
  `Diet Quality (1-10)` +
  `Stress Level (1-10)` +
  `Heart Rate (bpm)` +
  `Breathing Rate (breaths/min)` +
  `Sweating Level (1-5)` +
  Gender +
  Occupation +
  Smoking +
  `Family History of Anxiety` +
  Dizziness +
  Medication +
  `Recent Major Life Event`,
data = train
)

summary(mod_train)
# on fait la prediction sur test 

pred <- predict(mod_train, newdata = test)


# 
RMSE <- sqrt(mean(
  (test$`Anxiety Level (1-10)` - pred)^2
))

MAE <- mean(
  abs(test$`Anxiety Level (1-10)` - pred)
)

RMSE
MAE


R2_test <- 1 - sum(
  (test$`Anxiety Level (1-10)` - pred)^2
) / sum(
  (test$`Anxiety Level (1-10)` -
     mean(test$`Anxiety Level (1-10)`))^2
)

R2_test





y_train <- train$`Anxiety Level (1-10)`

x_train <- model.matrix(
  `Anxiety Level (1-10)` ~
    Age +
    `Sleep Hours` +
    `Physical Activity (hrs/week)` +
    `Caffeine Intake (mg/day)` +
    `Alcohol Consumption (drinks/week)` +
    `Diet Quality (1-10)` +
    `Stress Level (1-10)` +
    `Heart Rate (bpm)` +
    `Breathing Rate (breaths/min)` +
    `Sweating Level (1-5)` +
    Gender +
    Occupation +
    Smoking +
    `Family History of Anxiety` +
    Dizziness +
    Medication +
    `Recent Major Life Event`,
  data = train
)[, -1]

dim(x_train)



# ============================================================
# 7. RÉGRESSION LASSO
# ============================================================

set.seed(123)

cv_lasso <- cv.glmnet(
  x_train,
  y_train,
  alpha = 1,
  family = "gaussian",
  type.measure = "mse",
  nfolds = 10
)

plot(cv_lasso)


cv_lasso$lambda.min
cv_lasso$lambda.1se


# 

lasso_min <- glmnet(
  x_train,
  y_train,
  alpha = 1,
  family = "gaussian"
)

pred_lasso_min <- predict(
  lasso_min,
  newx = model.matrix(
    `Anxiety Level (1-10)` ~
      Age +
      `Sleep Hours` +
      `Physical Activity (hrs/week)` +
      `Caffeine Intake (mg/day)` +
      `Alcohol Consumption (drinks/week)` +
      `Diet Quality (1-10)` +
      `Stress Level (1-10)` +
      `Heart Rate (bpm)` +
      `Breathing Rate (breaths/min)` +
      `Sweating Level (1-5)` +
      Gender +
      Occupation +
      Smoking +
      `Family History of Anxiety` +
      Dizziness +
      Medication +
      `Recent Major Life Event`,
    data = test
  )[, -1],
  s = cv_lasso$lambda.min
)
x_test <- model.matrix(
  `Anxiety Level (1-10)` ~
    Age +
    `Sleep Hours` +
    `Physical Activity (hrs/week)` +
    `Caffeine Intake (mg/day)` +
    `Alcohol Consumption (drinks/week)` +
    `Diet Quality (1-10)` +
    `Stress Level (1-10)` +
    `Heart Rate (bpm)` +
    `Breathing Rate (breaths/min)` +
    `Sweating Level (1-5)` +
    Gender +
    Occupation +
    Smoking +
    `Family History of Anxiety` +
    Dizziness +
    Medication +
    `Recent Major Life Event`,
  data = test
)[, -1]


pred_lasso_min <- predict(
  cv_lasso,
  newx = x_test,
  s = "lambda.min"
)


RMSE_lasso_min <- sqrt(mean(
  (test$`Anxiety Level (1-10)` - pred_lasso_min)^2
))

MAE_lasso_min <- mean(
  abs(test$`Anxiety Level (1-10)` - pred_lasso_min)
)

R2_lasso_min <- 1 - sum(
  (test$`Anxiety Level (1-10)` - pred_lasso_min)^2
) /
  sum(
    (test$`Anxiety Level (1-10)` -
       mean(test$`Anxiety Level (1-10)`))^2
  )

RMSE_lasso_min
MAE_lasso_min
R2_lasso_min


#
pred_lasso_1se <- predict(
  cv_lasso,
  newx = x_test,
  s = "lambda.1se"
)

RMSE_lasso_1se <- sqrt(mean(
  (test$`Anxiety Level (1-10)` - pred_lasso_1se)^2
))

MAE_lasso_1se <- mean(
  abs(test$`Anxiety Level (1-10)` - pred_lasso_1se)
)

R2_lasso_1se <- 1 - sum(
  (test$`Anxiety Level (1-10)` - pred_lasso_1se)^2
) /
  sum(
    (test$`Anxiety Level (1-10)` -
       mean(test$`Anxiety Level (1-10)`))^2
  )

RMSE_lasso_1se
MAE_lasso_1se
R2_lasso_1se




coef_min <- coef(cv_lasso, s = "lambda.min")

coef_min


coef_1se <- coef(cv_lasso, s = "lambda.1se")

coef_1se



coef_min <- coef(cv_lasso, s = "lambda.min")

variables_min <- rownames(coef_min)[coef_min[,1] != 0]

variables_min

coef_1se <- coef(cv_lasso, s = "lambda.1se")

variables_1se <- rownames(coef_1se)[coef_1se[,1] != 0]

variables_1se

sum(coef_min[-1,1] != 0)
sum(coef_1se[-1,1] != 0)

results <- data.frame(
  Modele = c(
    "Régression linéaire",
    "LASSO lambda.min",
    "LASSO lambda.1se"
  ),
  RMSE = c(
    RMSE,
    RMSE_lasso_min,
    RMSE_lasso_1se
  ),
  MAE = c(
    MAE,
    MAE_lasso_min,
    MAE_lasso_1se
  ),
  R2 = c(
    R2_test,
    R2_lasso_min,
    R2_lasso_1se
  )
)

results


results_long <- results %>%
  pivot_longer(
    cols = c(RMSE, MAE, R2),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(results_long, aes(x = Modele, y = Value, fill = Modele)) +
  geom_col() +
  facet_wrap(~ Metric, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Modèle",
    y = "Valeur",
    title = "Comparaison des performances des modèles"
  ) +
  theme(legend.position = "none")



#
mod_comportement <- lm(
`Anxiety Level (1-10)` ~
  Age +
  `Sleep Hours` +
  `Physical Activity (hrs/week)` +
  `Caffeine Intake (mg/day)` +
  `Alcohol Consumption (drinks/week)` +
  `Diet Quality (1-10)` +
  Smoking +
  `Medication` +
  `Recent Major Life Event`,
data = train
)

summary(mod_comportement)


pred_comportement <- predict(
  mod_comportement,
  newdata = test
)


RMSE_comportement <- sqrt(mean(
  (test$`Anxiety Level (1-10)` - pred_comportement)^2
))

MAE_comportement <- mean(
  abs(test$`Anxiety Level (1-10)` - pred_comportement)
)

R2_comportement <- 1 - sum(
  (test$`Anxiety Level (1-10)` - pred_comportement)^2
) /
  sum(
    (test$`Anxiety Level (1-10)` -
       mean(test$`Anxiety Level (1-10)`))^2
  )

RMSE_comportement
MAE_comportement
R2_comportement



mod_user <- lm(
  `Anxiety Level (1-10)` ~
    Age +
    `Sleep Hours` +
    `Physical Activity (hrs/week)` +
    `Caffeine Intake (mg/day)` +
    `Alcohol Consumption (drinks/week)` +
    `Diet Quality (1-10)` +
    `Stress Level (1-10)` +
    Smoking +
    `Recent Major Life Event`,
  data = train
)


pred_user <- predict(
  mod_user,
  newdata = test
)

RMSE_user <- sqrt(mean(
  (test$`Anxiety Level (1-10)` - pred_user)^2
))

MAE_user <- mean(
  abs(test$`Anxiety Level (1-10)` - pred_user)
)

R2_user <- 1 - sum(
  (test$`Anxiety Level (1-10)` - pred_user)^2
) /
  sum(
    (test$`Anxiety Level (1-10)` -
       mean(test$`Anxiety Level (1-10)`))^2
  )

RMSE_user
MAE_user
R2_user



x_train_user <- model.matrix(
  `Anxiety Level (1-10)` ~
    Age +
    `Sleep Hours` +
    `Physical Activity (hrs/week)` +
    `Caffeine Intake (mg/day)` +
    `Alcohol Consumption (drinks/week)` +
    `Diet Quality (1-10)` +
    `Stress Level (1-10)` +
    Smoking +
    `Recent Major Life Event`,
  data = train
)[, -1]

x_test_user <- model.matrix(
  `Anxiety Level (1-10)` ~
    Age +
    `Sleep Hours` +
    `Physical Activity (hrs/week)` +
    `Caffeine Intake (mg/day)` +
    `Alcohol Consumption (drinks/week)` +
    `Diet Quality (1-10)` +
    `Stress Level (1-10)` +
    Smoking +
    `Recent Major Life Event`,
  data = test
)[, -1]

y_train <- train$`Anxiety Level (1-10)`


cv_lasso_user <- cv.glmnet(
  x_train_user,
  y_train,
  alpha = 1,
  family = "gaussian"
)

plot(cv_lasso_user)

coef(cv_lasso_user, s = "lambda.min")
coef(cv_lasso_user, s = "lambda.1se")




ggplot(
  data.frame(
    Observed = test$`Anxiety Level (1-10)`,
    Predicted = pred_user
  ),
  aes(x = Observed, y = Predicted)
) +
  geom_point(alpha = 0.3) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  theme_minimal() +
  labs(
        x = "Niveau d'anxiété observé",
         y = "Niveau d'anxiété prédit",
    title = "Valeurs observées vs valeurs prédites"
       )


par(mfrow = c(2, 2))
plot(mod_user)


# Randomforest 
train_user <- train %>%
  select(
    `Anxiety Level (1-10)`,
    Age,
    `Sleep Hours`,
    `Physical Activity (hrs/week)`,
    `Caffeine Intake (mg/day)`,
    `Alcohol Consumption (drinks/week)`,
    `Diet Quality (1-10)`,
    Smoking,
    Medication,
    `Recent Major Life Event`
  )

test_user <- test %>%
  select(
    `Anxiety Level (1-10)`,
    Age,
    `Sleep Hours`,
    `Physical Activity (hrs/week)`,
    `Caffeine Intake (mg/day)`,
    `Alcohol Consumption (drinks/week)`,
    `Diet Quality (1-10)`,
    Smoking,
    Medication,
    `Recent Major Life Event`
    
  )


str(train_user)
# Transformer en facteurs 
train_user$Smoking <- as.factor(train_user$Smoking)
test_user$Smoking <- as.factor(test_user$Smoking)

train_user$Medication <- as.factor(train_user$Medication)
test_user$Medication <- as.factor(test_user$Medication)

train_user$`Recent Major Life Event` <- as.factor(train_user$`Recent Major Life Event`)
test_user$`Recent Major Life Event` <- as.factor(test_user$`Recent Major Life Event`)


# on commence abec 500 arbres 

x_train_user <- train_user[, -which(
  names(train_user) == "Anxiety Level (1-10)"
)]

y_train_user <- train_user$`Anxiety Level (1-10)`


dim(x_train_user)
length(y_train_user)



set.seed(123)

rf_user <- randomForest(
  x = x_train_user,
  y = y_train_user,
  ntree = 500,
  importance = TRUE
)

str(x_train_user)
# construction du test : 

x_test_user <- test_user[, -which(
  names(test_user) == "Anxiety Level (1-10)"
)]

y_test_user <- test_user$`Anxiety Level (1-10)`
#

x_test_user <- test_user[, -which(
  names(test_user) == "Anxiety Level (1-10)"
)]

y_test_user <- test_user$`Anxiety Level (1-10)`

dim(x_test_user)
length(y_test_user)

x_test_user$Smoking <- factor(
  x_test_user$Smoking,
  levels = levels(x_train_user$Smoking)
)

x_test_user$Medication <- factor(
  x_test_user$Medication,
  levels = levels(x_train_user$Medication)
)

x_test_user$`Recent Major Life Event` <- factor(
  x_test_user$`Recent Major Life Event`,
  levels = levels(x_train_user$`Recent Major Life Event`)
)
str(x_train_user)
str(x_test_user)
# prediction 
pred_rf_user <- predict(
  rf_user,
  newdata = x_test_user
)


RMSE_rf_user <- sqrt(mean(
  (y_test_user - pred_rf_user)^2
))

MAE_rf_user <- mean(
  abs(y_test_user - pred_rf_user)
)

R2_rf_user <- 1 - sum(
  (y_test_user - pred_rf_user)^2
) /
  sum(
    (y_test_user - mean(y_test_user))^2
  )


# Graph des modeles

# preparation des resultats 
results <- data.frame(
  Modele = c(
    "Régression linéaire",
    "LASSO lambda.min",
    "LASSO lambda.1se",
    "Modèle utilisateur",
    "Random Forest"
  ),
  RMSE = c(
    1.170834,
    1.171362,
    1.183257,
    1.225183,
    RMSE_rf_user
  ),
  MAE = c(
    0.9234364,
    0.9237293,
    0.9280680,
    0.9663482,
    MAE_rf_user
  ),
  R2 = c(
    0.6954236,
    0.6951489,
    0.6889261,
    0.6664912,
    R2_rf_user
  )
)


# transformation des données 

results_long <- results %>%
  pivot_longer(
    cols = c(RMSE, MAE, R2),
    names_to = "Metric",
    values_to = "Value"
  )

# graph 
ggplot(
  results_long,
  aes(x = Modele, y = Value, fill = Modele)
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(Value, 3)),
    vjust = -0.3,
    size = 3.5
  ) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Régression linéaire" = "#4E79A7",
      "LASSO lambda.min" = "#59A14F",
      "LASSO lambda.1se" = "#F28E2B",
      "Modèle utilisateur" = "#E15759",
      "Random Forest" = "#B07AA1"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Comparaison des performances des modèles",
    x = NULL,
    y = "Valeur"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    legend.position = "none"
  )


# graph pour chacun des indicateur 
# Rmse 
ggplot(results, aes(x = Modele, y = RMSE, fill = Modele)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(RMSE, 3)),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Régression linéaire" = "#4E79A7",
      "LASSO lambda.min" = "#59A14F",
      "LASSO lambda.1se" = "#F28E2B",
      "Modèle utilisateur" = "#E15759",
      "Random Forest" = "#B07AA1"
    )
  ) +
  labs(
    title = "Comparaison des modèles - RMSE",
    subtitle = "Plus la valeur est faible, meilleure est la performance",
    x = NULL,
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# R2 
ggplot(results, aes(x = Modele, y = R2, fill = Modele)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(R2, 3)),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Régression linéaire" = "#4E79A7",
      "LASSO lambda.min" = "#59A14F",
      "LASSO lambda.1se" = "#F28E2B",
      "Modèle utilisateur" = "#E15759",
      "Random Forest" = "#B07AA1"
    )
  ) +
  labs(
    title = "Comparaison des modèles - R²",
    subtitle = "Plus la valeur est élevée, meilleure est la performance",
    x = NULL,
    y = "R²"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
# MAE 
ggplot(results, aes(x = Modele, y = MAE, fill = Modele)) +
geom_col(width = 0.7) +
  geom_text(
    aes(label = round(MAE, 3)),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Régression linéaire" = "#4E79A7",
      "LASSO lambda.min" = "#59A14F",
      "LASSO lambda.1se" = "#F28E2B",
      "Modèle utilisateur" = "#E15759",
      "Random Forest" = "#B07AA1"
    )
  ) +
  labs(
    title = "Comparaison des modèles - MAE",
    subtitle = "Plus la valeur est faible, meilleure est la performance",
    x = NULL,
    y = "MAE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )



# les graph 
ggplot(data, aes(x = `Anxiety Level (1-10)`)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    fill = "#4E79A7",
    color = "white"
  ) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Distribution du niveau d'anxiété",
    x = "Niveau d'anxiété",
    y = "Nombre d'individus"
  ) +
  theme_minimal()




# 2 
coef_user <- coef(mod_comportement)

coef_df <- data.frame(
  Variable = names(coef_user),
  Coefficient = as.numeric(coef_user)
)

coef_df <- coef_df[
  coef_df$Variable != "(Intercept)",
]

ggplot(
  coef_df,
  aes(
    x = reorder(Variable, Coefficient),
    y = Coefficient,
    fill = Coefficient > 0
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#E15759",
      "FALSE" = "#4E79A7"
    ),
    labels = c(
      "TRUE" = "Association positive",
      "FALSE" = "Association négative"
    )
  ) +
  labs(
    title = "Variables associées au niveau d'anxiété",
    x = NULL,
    y = "Coefficient du modèle",
    fill = NULL
  ) +
  theme_minimal()


#3 
ggplot(
  data,
  aes(
    x = `Sleep Hours`,
    y = `Anxiety Level (1-10)`
  )
) +
  geom_point(
    alpha = 0.15,
    size = 1
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Relation entre sommeil et niveau d'anxiété",
    x = "Heures de sommeil",
    y = "Niveau d'anxiété"
  ) +
  theme_minimal()

#4

ggplot(
  data,
  aes(
    x = `Stress Level (1-10)`,
    y = `Anxiety Level (1-10)`
  )
) +
  geom_jitter(
    alpha = 0.15,
    width = 0.15,
    height = 0.15
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 1:10) +
  labs(
    title = "Relation entre stress et niveau d'anxiété",
    x = "Niveau de stress",
    y = "Niveau d'anxiété"
  ) +
  theme_minimal()

#5
data <- data %>%
  mutate(
    Groupe_anxiete = case_when(
      `Anxiety Level (1-10)` <= 3 ~ "Faible anxiété",
      `Anxiety Level (1-10)` >= 8 ~ "Forte anxiété",
      TRUE ~ "Intermédiaire"
    )
  )

ggplot(
  data %>% filter(Groupe_anxiete != "Intermédiaire"),
  aes(
    x = Groupe_anxiete,
    y = `Sleep Hours`,
    fill = Groupe_anxiete
  )
) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "Faible anxiété" = "#59A14F",
      "Forte anxiété" = "#E15759"
    )
  ) +
  labs(
    title = "Sommeil selon le niveau d'anxiété",
    x = NULL,
    y = "Heures de sommeil"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#6 

data_num <- data %>%
  select(
    Age,
    `Sleep Hours`,
    `Physical Activity (hrs/week)`,
    `Caffeine Intake (mg/day)`,
    `Alcohol Consumption (drinks/week)`,
    `Diet Quality (1-10)`,
    `Stress Level (1-10)`,
    `Heart Rate (bpm)`,
    `Breathing Rate (breaths/min)`,
    `Sweating Level (1-5)`,
    `Anxiety Level (1-10)`
  )

cor_mat <- cor(data_num)

cor_df <- as.data.frame(as.table(cor_mat))

names(cor_df) <- c(
  "Variable1",
  "Variable2",
  "Correlation"
)

ggplot(
  cor_df,
  aes(
    x = Variable1,
    y = Variable2,
    fill = Correlation
  )
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#4E79A7",
    mid = "white",
    high = "#E15759",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Matrice des corrélations",
    x = NULL,
    y = NULL,
    fill = "Corrélation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# 7 ACP 

data_acp <- data %>%
  select(
    `Anxiety Level (1-10)`,
    `Age`,
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

acp <- PCA(
  data_acp,
  scale.unit = TRUE,
  graph = T
)


fviz_eig(
  acp,
  addlabels = TRUE
)


fviz_pca_ind(
  acp,
  geom = "point",
  habillage = data$`Anxiety Level (1-10)`,
  palette = "RdYlBu",
  addEllipses = FALSE,
  alpha.ind = 0.5
)


#
fviz_pca_biplot(
  acp,
  geom.ind = "point",
  col.ind = data$`Anxiety Level (1-10)`,
  gradient.cols = c("#4E79A7", "white", "#E15759"),
  col.var = "#333333",
  repel = TRUE,
  alpha.ind = 0.5
)

## test chat 

# ============================================================
# ANALYSE DE LA STRUCTURE DU DATASET D'ANXIETE
# Objectif :
# 1. Etudier l'évolution des variables avec l'anxiété
# 2. Chercher une éventuelle rupture entre les niveaux 7 et 8
# 3. Quantifier cette rupture
# 4. Visualiser les changements
# 5. Vérifier la structure avec une ACP
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================




# ============================================================
# 1. VARIABLES QUANTITATIVES A ETUDIER
# ============================================================

variables <- c(
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


# ============================================================
# 2. MOYENNES PAR NIVEAU D'ANXIETE
# ============================================================

evolution <- data %>%
  group_by(`Anxiety Level (1-10)`) %>%
  summarise(
    across(
      all_of(variables),
      list(
        moyenne = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    ),
    n = n(),
    .groups = "drop"
  )

print(evolution)


# ============================================================
# 3. PASSAGE AU FORMAT LONG
# ============================================================

data_long <- data %>%
  select(
    `Anxiety Level (1-10)`,
    all_of(variables)
  ) %>%
  pivot_longer(
    cols = -`Anxiety Level (1-10)`,
    names_to = "Variable",
    values_to = "Valeur"
  )


# ============================================================
# 4. STANDARDISATION DES VARIABLES
#
# Nécessaire car on compare :
# heures, mg, scores, bpm, etc.
# ============================================================

data_long <- data_long %>%
  group_by(Variable) %>%
  mutate(
    Valeur_z = as.numeric(scale(Valeur))
  ) %>%
  ungroup()


# ============================================================
# 5. MOYENNE STANDARDISEE POUR CHAQUE NIVEAU D'ANXIETE
# ============================================================

moyennes_z <- data_long %>%
  group_by(
    Variable,
    `Anxiety Level (1-10)`
  ) %>%
  summarise(
    Moyenne_z = mean(Valeur_z, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    Variable,
    `Anxiety Level (1-10)`
  )


# ============================================================
# 6. CALCUL DES CHANGEMENTS ENTRE NIVEAUX CONSECUTIFS
#
# 1 -> 2
# 2 -> 3
# ...
# 7 -> 8
# ...
# 9 -> 10
# ============================================================

ruptures <- moyennes_z %>%
  group_by(Variable) %>%
  mutate(
    Niveau_precedent = lag(`Anxiety Level (1-10)`),
    
    Changement = Moyenne_z - lag(Moyenne_z),
    
    # Valeur absolue du changement
    Amplitude = abs(Changement),
    
    Transition = paste0(
      Niveau_precedent,
      " → ",
      `Anxiety Level (1-10)`
    )
  ) %>%
  filter(!is.na(Changement)) %>%
  ungroup()


# ============================================================
# 7. CLASSEMENT DE TOUTES LES RUPTURES
# ============================================================

classement_ruptures <- ruptures %>%
  arrange(desc(Amplitude))

print(classement_ruptures)


# ============================================================
# 8. PLUS GRANDE RUPTURE POUR CHAQUE VARIABLE
#
# Très important :
# permet de voir si 7 -> 8 revient souvent
# ============================================================

plus_grande_rupture <- ruptures %>%
  group_by(Variable) %>%
  slice_max(
    order_by = Amplitude,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  arrange(desc(Amplitude))

print(plus_grande_rupture)


# ============================================================
# 9. COMBIEN DE VARIABLES ONT LEUR PLUS FORTE RUPTURE EN 7 -> 8 ?
# ============================================================

resume_transitions <- plus_grande_rupture %>%
  count(Transition, sort = TRUE)

print(resume_transitions)


# ============================================================
# 10. ZOOM UNIQUEMENT SUR LA TRANSITION 7 -> 8
# ============================================================

rupture_7_8 <- ruptures %>%
  filter(Transition == "7 → 8") %>%
  arrange(desc(Amplitude))

print(rupture_7_8)


# ============================================================
# 11. GRAPHIQUE 1
# EVOLUTION STANDARDISEE DES VARIABLES DE 1 A 10
#
# Permet de voir visuellement si une rupture apparaît
# ============================================================

graph_evolution <- ggplot(
  moyennes_z,
  aes(
    x = `Anxiety Level (1-10)`,
    y = Moyenne_z,
    group = Variable,
    color = Variable
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  
  # Ligne verticale entre 7 et 8
  geom_vline(
    xintercept = 7.5,
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey30"
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Comment les profils évoluent-ils avec le niveau d'anxiété ?",
    subtitle = "Variables standardisées – la ligne pointillée représente la transition 7 → 8",
    x = "Niveau d'anxiété",
    y = "Moyenne standardisée (z-score)",
    color = "Variable"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    legend.position = "right"
  )

print(graph_evolution)


# ============================================================
# 12. GRAPHIQUE 2
# VERSION FACETTEE = PLUS LISIBLE
# ============================================================

graph_evolution_facettes <- ggplot(
  moyennes_z,
  aes(
    x = `Anxiety Level (1-10)`,
    y = Moyenne_z
  )
) +
  
  geom_line(
    linewidth = 1,
    color = "#3366AA"
  ) +
  
  geom_point(
    size = 2,
    color = "#3366AA"
  ) +
  
  geom_vline(
    xintercept = 7.5,
    linetype = "dashed",
    color = "#D73027"
  ) +
  
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_x_continuous(
    breaks = 1:10
  ) +
  
  labs(
    title = "Évolution des caractéristiques selon le niveau d'anxiété",
    subtitle = "La ligne rouge représente la transition entre les niveaux 7 et 8",
    x = "Niveau d'anxiété",
    y = "Moyenne standardisée"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(graph_evolution_facettes)


# ============================================================
# 13. GRAPHIQUE 3
# HEATMAP DES RUPTURES
#
# Plus la case est foncée,
# plus le changement entre deux niveaux est important
# ============================================================

graph_heatmap <- ggplot(
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
    subtitle = "Amplitude standardisée du changement entre deux niveaux d'anxiété consécutifs",
    x = "Transition entre niveaux d'anxiété",
    y = NULL,
    fill = "Amplitude"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(graph_heatmap)


# ============================================================
# 14. GRAPHIQUE 4
# AMPLITUDE SPECIFIQUE DE LA RUPTURE 7 -> 8
# ============================================================

graph_7_8 <- rupture_7_8 %>%
  ggplot(
    aes(
      x = reorder(Variable, Amplitude),
      y = Amplitude
    )
  ) +
  
  geom_col(
    fill = "#D73027",
    width = 0.7
  ) +
  
  coord_flip() +
  
  labs(
    title = "Quelles variables changent le plus entre les niveaux 7 et 8 ?",
    subtitle = "Amplitude du changement calculée sur les variables standardisées",
    x = NULL,
    y = "Amplitude du changement"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    panel.grid.major.y = element_blank()
  )

print(graph_7_8)


# ============================================================
# 15. TESTS STATISTIQUES 7 VS 8
# ============================================================

data_7_8 <- data %>%
  filter(
    `Anxiety Level (1-10)` %in% c(7, 8)
  )


# ============================================================
# Fonction pour réaliser automatiquement :
#
# - moyenne niveau 7
# - moyenne niveau 8
# - différence
# - test de Student
# - p-value
# - Cohen's d
# ============================================================

tester_variable <- function(variable) {
  
  x7 <- data_7_8 %>%
    filter(`Anxiety Level (1-10)` == 7) %>%
    pull(all_of(variable))
  
  x8 <- data_7_8 %>%
    filter(`Anxiety Level (1-10)` == 8) %>%
    pull(all_of(variable))
  
  test <- t.test(x7, x8)
  
  # Cohen's d calculé directement
  n7 <- sum(!is.na(x7))
  n8 <- sum(!is.na(x8))
  
  sd7 <- sd(x7, na.rm = TRUE)
  sd8 <- sd(x8, na.rm = TRUE)
  
  sd_pool <- sqrt(
    ((n7 - 1) * sd7^2 +
       (n8 - 1) * sd8^2) /
      (n7 + n8 - 2)
  )
  
  d <- (
    mean(x8, na.rm = TRUE) -
      mean(x7, na.rm = TRUE)
  ) / sd_pool
  
  data.frame(
    Variable = variable,
    
    Moyenne_niveau_7 =
      mean(x7, na.rm = TRUE),
    
    Moyenne_niveau_8 =
      mean(x8, na.rm = TRUE),
    
    Difference =
      mean(x8, na.rm = TRUE) -
      mean(x7, na.rm = TRUE),
    
    p_value =
      test$p.value,
    
    Cohen_d = d,
    
    Amplitude_Cohen_d = abs(d)
  )
}


# Appliquer automatiquement à toutes les variables
resultats_tests <- bind_rows(
  lapply(
    variables,
    tester_variable
  )
) %>%
  arrange(desc(Amplitude_Cohen_d))

print(resultats_tests)


# ============================================================
# 16. INTERPRETATION AUTOMATIQUE DE COHEN'S D
#
# environ :
# < 0.2 : très faible
# 0.2-0.5 : faible
# 0.5-0.8 : modéré
# >= 0.8 : fort
# ============================================================

resultats_tests <- resultats_tests %>%
  mutate(
    Importance = case_when(
      
      Amplitude_Cohen_d < 0.2 ~
        "Très faible",
      
      Amplitude_Cohen_d < 0.5 ~
        "Faible",
      
      Amplitude_Cohen_d < 0.8 ~
        "Modérée",
      
      TRUE ~
        "Forte"
    )
  )

print(resultats_tests)


# ============================================================
# 17. GRAPHIQUE DES TAILLES D'EFFET 7 -> 8
#
# Plus |d| est grand,
# plus les niveaux 7 et 8 diffèrent sur cette variable
# ============================================================

graph_effets <- ggplot(
  resultats_tests,
  aes(
    x = reorder(Variable, Amplitude_Cohen_d),
    y = Amplitude_Cohen_d,
    fill = Importance
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  coord_flip() +
  
  geom_hline(
    yintercept = 0.8,
    linetype = "dashed",
    color = "grey30"
  ) +
  
  labs(
    title = "Quelle est l'importance de la rupture entre 7 et 8 ?",
    subtitle = "Taille d'effet de Cohen – la ligne pointillée correspond à un effet fort (|d| = 0,8)",
    x = NULL,
    y = "|Cohen's d|",
    fill = "Importance"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    panel.grid.major.y = element_blank()
  )

print(graph_effets)


# ============================================================
# 18. ACP
#
# IMPORTANT :
# Anxiety Level n'est PAS utilisée pour construire l'ACP.
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
# 19. POURCENTAGE DE VARIANCE EXPLIQUEE
# ============================================================

print(res_acp$eig)


# ============================================================
# 20. CERCLE DES VARIABLES
# ============================================================

graph_acp_variables <- fviz_pca_var(
  res_acp,
  col.var = "contrib",
  gradient.cols = c(
    "#4E79A7",
    "#F28E2B",
    "#E15759"
  ),
  repel = TRUE
) +
  
  labs(
    title = "Quelles variables structurent les profils ?"
  )

print(graph_acp_variables)


# ============================================================
# 21. CREATION DES GROUPES APRES L'ACP
#
# L'anxiété sert uniquement à colorer les individus.
# Elle n'a PAS servi à calculer les axes.
# ============================================================

groupe_anxiete <- ifelse(
  data$`Anxiety Level (1-10)` >= 8,
  "Anxiété 8–10",
  "Anxiété 1–7"
)


# ============================================================
# 22. ACP DES INDIVIDUS
# ============================================================

graph_acp_individus <- fviz_pca_ind(
  res_acp,
  
  geom = "point",
  
  habillage = factor(
    groupe_anxiete
  ),
  
  addEllipses = TRUE,
  
  ellipse.type = "confidence",
  
  alpha.ind = 0.25,
  
  palette = c(
    "#4E79A7",
    "#E15759"
  )
) +
  
  labs(
    title = "Des profils distincts émergent-ils sans utiliser l'anxiété ?",
    subtitle = "ACP construite sans la variable Anxiety Level"
  )

print(graph_acp_individus)


# ============================================================
# 23. RESUME FINAL A REGARDER
# ============================================================

cat("\n")
cat("============================================\n")
cat("RESUME DE L'ANALYSE\n")
cat("============================================\n\n")

cat("PLUS GRANDE RUPTURE POUR CHAQUE VARIABLE :\n\n")
print(plus_grande_rupture)

cat("\n")
cat("NOMBRE DE PLUS GRANDES RUPTURES PAR TRANSITION :\n\n")
print(resume_transitions)

cat("\n")
cat("RUPTURE 7 -> 8 :\n\n")
print(rupture_7_8)

cat("\n")
cat("TESTS 7 VS 8 ET TAILLES D'EFFET :\n\n")
print(resultats_tests)

cat("\n")
cat("VARIANCE EXPLIQUEE PAR L'ACP :\n\n")
print(res_acp$eig)





# ============================================================
# VISUALISATIONS FINALES - PROJET ANXIETE
# ============================================================


# ============================================================
# PALETTE
# Modifie uniquement ces couleurs si vous voulez changer
# l'identité visuelle de toute la présentation
# ============================================================

bleu <- "#3B6FB6"
rouge <- "#E45756"
orange <- "#F2A541"
gris <- "#E8E8E8"
vert <- "#59A14F"


# ============================================================
# VARIABLES QUANTITATIVES
# ============================================================

variables <- c(
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


# ============================================================
# VISUALISATION 1
# HEATMAP DES RUPTURES
#
# QUESTION :
# "A quel moment les profils changent-ils le plus ?"
# ============================================================


# Passage en format long
data_long <- data %>%
  select(
    `Anxiety Level (1-10)`,
    all_of(variables)
  ) %>%
  pivot_longer(
    cols = -`Anxiety Level (1-10)`,
    names_to = "Variable",
    values_to = "Valeur"
  )


# Standardisation
data_long <- data_long %>%
  group_by(Variable) %>%
  mutate(
    Valeur_z = as.numeric(scale(Valeur))
  ) %>%
  ungroup()


# Moyennes par niveau d'anxiété
moyennes_z <- data_long %>%
  group_by(
    Variable,
    `Anxiety Level (1-10)`
  ) %>%
  summarise(
    Moyenne_z = mean(Valeur_z, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    Variable,
    `Anxiety Level (1-10)`
  )


# Changements entre niveaux consécutifs
ruptures <- moyennes_z %>%
  group_by(Variable) %>%
  mutate(
    precedent = lag(`Anxiety Level (1-10)`),
    
    Changement =
      Moyenne_z - lag(Moyenne_z),
    
    Amplitude =
      abs(Changement),
    
    Transition = paste0(
      precedent,
      " → ",
      `Anxiety Level (1-10)`
    )
  ) %>%
  filter(!is.na(Changement)) %>%
  ungroup()


# Ordre des transitions
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


# ============================================================
# VISUALISATIONS FINALES - PROJET ANXIETE
# ============================================================



# ============================================================
# VARIABLES QUANTITATIVES
# ============================================================

variables <- c(
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


# ============================================================
# PREPARATION DES DONNEES POUR LES GRAPHIQUES 1 ET 2
# ============================================================

data_long <- data %>%
  select(
    `Anxiety Level (1-10)`,
    all_of(variables)
  ) %>%
  pivot_longer(
    cols = -`Anxiety Level (1-10)`,
    names_to = "Variable",
    values_to = "Valeur"
  )


# Standardisation des variables
# -> permet de comparer des variables qui n'ont pas les mêmes unités

data_long <- data_long %>%
  group_by(Variable) %>%
  mutate(
    Valeur_z = as.numeric(scale(Valeur))
  ) %>%
  ungroup()


# Moyenne standardisée pour chaque niveau d'anxiété

moyennes_z <- data_long %>%
  group_by(
    Variable,
    `Anxiety Level (1-10)`
  ) %>%
  summarise(
    Moyenne_z = mean(Valeur_z, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    Variable,
    `Anxiety Level (1-10)`
  )


# Calcul des changements entre niveaux consécutifs

ruptures <- moyennes_z %>%
  group_by(Variable) %>%
  mutate(
    Niveau_precedent = lag(`Anxiety Level (1-10)`),
    Changement = Moyenne_z - lag(Moyenne_z),
    Amplitude = abs(Changement),
    Transition = paste0(
      Niveau_precedent,
      " → ",
      `Anxiety Level (1-10)`
    )
  ) %>%
  filter(!is.na(Changement)) %>%
  ungroup()


# Ordre correct des transitions sur le graphique

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


# ============================================================
# GRAPHIQUE 1
# HEATMAP DES CHANGEMENTS
#
# Question :
# A quel moment les profils changent-ils le plus ?
# ============================================================

graph1 <- ggplot(
  ruptures,
  aes(
    x = Transition,
    y = Variable,
    fill = Amplitude
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.8
  ) +
  
  # Encadrement de la transition 7 -> 8
  annotate(
    "rect",
    xmin = 6.5,
    xmax = 7.5,
    ymin = 0.5,
    ymax = length(variables) + 0.5,
    fill = NA,
    color = "#B2182B",
    linewidth = 1.2
  ) +
  
  scale_fill_gradient(
    low = "#F7F7F7",
    high = "#D73027"
  ) +
  
  labs(
    title = "Une rupture nette apparaît entre les niveaux 7 et 8",
    subtitle = "Amplitude du changement standardisé entre deux niveaux d'anxiété consécutifs",
    x = "Transition entre niveaux d'anxiété",
    y = NULL,
    fill = "Amplitude"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      size = 12,
      color = "grey30"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "right"
  )


print(graph1)



# ============================================================
# GRAPHIQUE 2
# DUMBBELL PLOT : NIVEAU 7 VS NIVEAU 8
#
# Question :
# Qu'est-ce qui change concrètement entre 7 et 8 ?
# ============================================================

profil_7_8 <- moyennes_z %>%
  
  filter(
    `Anxiety Level (1-10)` %in% c(7, 8)
  ) %>%
  
  select(
    Variable,
    `Anxiety Level (1-10)`,
    Moyenne_z
  ) %>%
  
  pivot_wider(
    names_from = `Anxiety Level (1-10)`,
    values_from = Moyenne_z,
    names_prefix = "niveau_"
  ) %>%
  
  mutate(
    Difference = niveau_8 - niveau_7,
    Variable = reorder(
      Variable,
      abs(Difference)
    )
  )


graph2 <- ggplot(
  profil_7_8,
  aes(y = Variable)
) +
  
  # Trait reliant niveau 7 et niveau 8
  geom_segment(
    aes(
      x = niveau_7,
      xend = niveau_8,
      yend = Variable
    ),
    color = "#C7C7C7",
    linewidth = 2.5
  ) +
  
  # Niveau 7 = bleu
  geom_point(
    aes(x = niveau_7),
    color = "#3B6FB6",
    size = 5
  ) +
  
  # Niveau 8 = rouge
  geom_point(
    aes(x = niveau_8),
    color = "#E45756",
    size = 5
  ) +
  
  # Moyenne générale
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  labs(
    title = "Le passage de 7 à 8 correspond à un changement de profil",
    subtitle = "Comparaison des caractéristiques standardisées aux niveaux d'anxiété 7 et 8",
    x = "Moyenne standardisée (z-score)",
    y = NULL,
    caption = "Bleu : niveau 7     |     Rouge : niveau 8"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      color = "grey30"
    ),
    
    plot.caption = element_text(
      size = 11,
      face = "bold",
      hjust = 0
    )
  )


print(graph2)



# ============================================================
# GRAPHIQUE 3
# ACP DES INDIVIDUS
#
# Question :
# La séparation apparaît-elle même lorsque Anxiety Level
# n'est PAS utilisée pour construire l'ACP ?
# ============================================================


# IMPORTANT :
# Anxiety Level n'est pas incluse ici

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


# ACP avec standardisation

res_acp <- PCA(
  tab_acp,
  scale.unit = TRUE,
  graph = FALSE
)


# Variance expliquée par les deux premières dimensions

dim1 <- round(res_acp$eig[1, 2], 1)
dim2 <- round(res_acp$eig[2, 2], 1)


# Coordonnées des individus

coord_acp <- as.data.frame(
  res_acp$ind$coord
)


# Le groupe est ajouté APRES l'ACP

coord_acp$Groupe <- ifelse(
  data$`Anxiety Level (1-10)` >= 8,
  "Anxiété 8–10",
  "Anxiété 1–7"
)


# Centres des deux groupes

centres <- coord_acp %>%
  group_by(Groupe) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2),
    .groups = "drop"
  )


graph3 <- ggplot(
  coord_acp,
  aes(
    x = Dim.1,
    y = Dim.2,
    color = Groupe,
    shape = Groupe
  )
) +
  
  # Individus
  geom_point(
    alpha = 0.18,
    size = 1.5
  ) +
  
  # Ellipse à 90 %
  stat_ellipse(
    aes(fill = Groupe),
    geom = "polygon",
    alpha = 0.07,
    color = NA,
    level = 0.90
  ) +
  
  # Centre de chaque groupe
  geom_point(
    data = centres,
    aes(
      x = Dim.1,
      y = Dim.2,
      color = Groupe
    ),
    inherit.aes = FALSE,
    size = 6
  ) +
  
  # Couleurs des groupes
  scale_color_manual(
    values = c(
      "Anxiété 1–7" = "#3B6FB6",
      "Anxiété 8–10" = "#E45756"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Anxiété 1–7" = "#3B6FB6",
      "Anxiété 8–10" = "#E45756"
    )
  ) +
  
  # Axes 0
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  
  labs(
    title = "Deux profils apparaissent sans utiliser le niveau d'anxiété",
    
    subtitle = paste0(
      "ACP construite à partir des caractéristiques des individus — ",
      "Dim1 + Dim2 = ",
      round(dim1 + dim2, 1),
      " % de variance"
    ),
    
    x = paste0(
      "Dimension 1 (",
      dim1,
      " %)"
    ),
    
    y = paste0(
      "Dimension 2 (",
      dim2,
      " %)"
    ),
    
    color = NULL,
    shape = NULL,
    fill = NULL,
    
    caption = paste0(
      "Le niveau d'anxiété n'intervient pas dans le calcul de l'ACP ; ",
      "il est utilisé uniquement pour colorer les individus."
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      color = "grey30"
    ),
    
    legend.position = "bottom",
    
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      color = "grey40",
      hjust = 0
    )
  )


print(graph3)



# ============================================================
# GRAPHIQUE 4
# VARIABLES QUALITATIVES
#
# Question :
# Les profils 1-7 et 8-10 diffèrent-ils également sur
# certaines caractéristiques qualitatives ?
# ============================================================


data_cat <- data %>%
  
  mutate(
    Groupe_anxiete = ifelse(
      `Anxiety Level (1-10)` >= 8,
      "Anxiété 8–10",
      "Anxiété 1–7"
    )
  ) %>%
  
  select(
    Groupe_anxiete,
    Smoking,
    `Family History of Anxiety`,
    Dizziness,
    Medication,
    `Recent Major Life Event`
  )


# Format long

data_cat_long <- data_cat %>%
  pivot_longer(
    cols = -Groupe_anxiete,
    names_to = "Variable",
    values_to = "Reponse"
  )


# Pourcentage de "Yes"

resume_cat <- data_cat_long %>%
  group_by(
    Groupe_anxiete,
    Variable
  ) %>%
  summarise(
    Pourcentage_Yes =
      mean(Reponse == "Yes", na.rm = TRUE) * 100,
    .groups = "drop"
  )


# Noms français plus lisibles

resume_cat <- resume_cat %>%
  mutate(
    Variable = recode(
      Variable,
      
      "Smoking" =
        "Tabagisme",
      
      "Family History of Anxiety" =
        "Antécédents familiaux",
      
      "Dizziness" =
        "Vertiges",
      
      "Medication" =
        "Médication",
      
      "Recent Major Life Event" =
        "Événement de vie récent"
    )
  )


# Calcul de l'écart entre les deux groupes
# afin d'ordonner les variables

ordre_cat <- resume_cat %>%
  
  pivot_wider(
    names_from = Groupe_anxiete,
    values_from = Pourcentage_Yes
  ) %>%
  
  mutate(
    Ecart = abs(
      `Anxiété 8–10` -
        `Anxiété 1–7`
    )
  ) %>%
  
  arrange(Ecart) %>%
  
  pull(Variable)


resume_cat$Variable <- factor(
  resume_cat$Variable,
  levels = ordre_cat
)


graph4 <- ggplot(
  resume_cat,
  aes(
    x = Pourcentage_Yes,
    y = Variable,
    color = Groupe_anxiete
  )
) +
  
  # Trait entre les deux proportions
  geom_line(
    aes(group = Variable),
    color = "#D0D0D0",
    linewidth = 2
  ) +
  
  # Points
  geom_point(
    size = 5
  ) +
  
  # Valeurs en %
  geom_text(
    aes(
      label = paste0(
        round(Pourcentage_Yes, 1),
        "%"
      )
    ),
    hjust = -0.35,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  # Couleurs
  scale_color_manual(
    values = c(
      "Anxiété 1–7" = "#3B6FB6",
      "Anxiété 8–10" = "#E45756"
    )
  ) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(
      mult = c(0.02, 0.15)
    )
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
    
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    
    plot.subtitle = element_text(
      color = "grey30"
    ),
    
    legend.position = "bottom"
  )


print(graph4)



# ============================================================
# SAUVEGARDE DES 4 GRAPHIQUES
# ============================================================

ggsave(
  filename = "01_heatmap_rupture.png",
  plot = graph1,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "02_profil_7_vs_8.png",
  plot = graph2,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "03_ACP_profils.png",
  plot = graph3,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "04_variables_qualitatives.png",
  plot = graph4,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# AFFICHER LES 4 GRAPHIQUES A LA FIN
# ============================================================

print(graph1)
print(graph2)
print(graph3)
print(graph4)