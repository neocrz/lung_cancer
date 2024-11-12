library(dplyr)

dataset <- dataset %>%
  mutate(GENDER = ifelse(GENDER == "M", 0, ifelse(GENDER == "F", 1, GENDER))) %>%
  mutate(GENDER = as.numeric(GENDER))

dataset <- dataset %>%
  mutate(SMOKING = ifelse(SMOKING == 1, 0, ifelse(SMOKING == 2, 1, SMOKING))) %>%
  mutate(SMOKING = as.numeric(SMOKING))
dataset <- dataset %>%
  mutate(YELLOW_FINGERS = ifelse(YELLOW_FINGERS == 1, 0, ifelse(YELLOW_FINGERS == 2, 1, YELLOW_FINGERS))) %>%
  mutate(YELLOW_FINGERS = as.numeric(YELLOW_FINGERS))
dataset <- dataset %>%
  mutate(ANXIETY = ifelse(ANXIETY == 1, 0, ifelse(ANXIETY == 2, 1, ANXIETY))) %>%
  mutate(ANXIETY = as.numeric(ANXIETY))
dataset <- dataset %>%
  mutate(PEER_PRESSURE = ifelse(PEER_PRESSURE == 1, 0, ifelse(PEER_PRESSURE == 2, 1, PEER_PRESSURE))) %>%
  mutate(PEER_PRESSURE = as.numeric(PEER_PRESSURE))

dataset <- dataset %>%
  mutate(CHRONIC_DISEASE = ifelse(CHRONIC_DISEASE == 1, 0, ifelse(CHRONIC_DISEASE == 2, 1, CHRONIC_DISEASE))) %>%
  mutate(CHRONIC_DISEASE = as.numeric(CHRONIC_DISEASE))

dataset <- dataset %>%
  mutate(FATIGUE = ifelse(FATIGUE == 1, 0, ifelse(FATIGUE == 2, 1, FATIGUE))) %>%
  mutate(FATIGUE = as.numeric(FATIGUE))

dataset <- dataset %>%
  mutate(ALLERGY = ifelse(ALLERGY == 1, 0, ifelse(ALLERGY == 2, 1, ALLERGY))) %>%
  mutate(ALLERGY = as.numeric(ALLERGY))

dataset <- dataset %>%
  mutate(WHEEZING = ifelse(WHEEZING == 1, 0, ifelse(WHEEZING == 2, 1, WHEEZING))) %>%
  mutate(WHEEZING = as.numeric(WHEEZING))

dataset <- dataset %>%
  mutate(ALCOHOL_CONSUMING = ifelse(ALCOHOL_CONSUMING == 1, 0, ifelse(ALCOHOL_CONSUMING == 2, 1, ALCOHOL_CONSUMING))) %>%
  mutate(ALCOHOL_CONSUMING = as.numeric(ALCOHOL_CONSUMING))

dataset <- dataset %>%
  mutate(COUGHING = ifelse(COUGHING == 1, 0, ifelse(COUGHING == 2, 1, COUGHING))) %>%
  mutate(COUGHING = as.numeric(COUGHING))

dataset <- dataset %>%
  mutate(SHORTNESS_OF_BREATH = ifelse(SHORTNESS_OF_BREATH == 1, 0, ifelse(SHORTNESS_OF_BREATH == 2, 1, SHORTNESS_OF_BREATH))) %>%
  mutate(SHORTNESS_OF_BREATH = as.numeric(SHORTNESS_OF_BREATH))

dataset <- dataset %>%
  mutate(SWALLOWING_DIFFICULTY = ifelse(SWALLOWING_DIFFICULTY == 1, 0, ifelse(SWALLOWING_DIFFICULTY == 2, 1, SWALLOWING_DIFFICULTY))) %>%
  mutate(SWALLOWING_DIFFICULTY = as.numeric(SWALLOWING_DIFFICULTY))

dataset <- dataset %>%
  mutate(CHEST_PAIN = ifelse(CHEST_PAIN == 1, 0, ifelse(CHEST_PAIN == 2, 1, CHEST_PAIN))) %>%
  mutate(CHEST_PAIN = as.numeric(CHEST_PAIN))

dataset <- dataset %>%
  mutate(LUNG_CANCER = ifelse(LUNG_CANCER == "NO", 0, ifelse(LUNG_CANCER == "YES", 1, LUNG_CANCER))) %>%
  mutate(LUNG_CANCER = as.numeric(LUNG_CANCER))
  
summary(dataset)
table(dataset$LUNG_CANCER)
# Teste Qui-Quadrado para variáveis categóricas (exemplo com GENDER)
chisq.test(table(dataset$GENDER, dataset$LUNG_CANCER))

# Correlação de ponto-biserial entre LUNG_CANCER e variáveis binárias
cor.test(dataset$LUNG_CANCER, dataset$GENDER, method = "pearson")


# Regressão logística
model <- glm(LUNG_CANCER ~ GENDER + SMOKING + YELLOW_FINGERS + ANXIETY + 
               PEER_PRESSURE + CHRONIC_DISEASE + FATIGUE + ALLERGY + 
               WHEEZING + ALCOHOL_CONSUMING + COUGHING + SHORTNESS_OF_BREATH + 
               SWALLOWING_DIFFICULTY + CHEST_PAIN, 
             data = dataset, family = binomial)

# Resumo do modelo
summary(model)

# Coeficientes do modelo
exp(coef(model)) # para interpretar os coeficientes em termos de odds ratio

# Carregar o pacote
library(randomForest)

# Modelo Random Forest
rf_model <- randomForest(LUNG_CANCER ~ ., data = dataset)

# Visualizar resultados
rf_model


# Análise de Cox
library(survival)
cox_model <- coxph(Surv(time, event) ~ GENDER + SMOKING + ANXIETY + ... , data = dataset)
summary(cox_model)

# Predições do modelo
predictions <- predict(model, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Matriz de confusão
table(Predicted = predicted_class, Actual = dataset$LUNG_CANCER)

library(pROC)
roc_curve <- roc(dataset$LUNG_CANCER, predictions)
plot(roc_curve)
auc(roc_curve)
