# -----------------------------------------------------------------------------
# SCRIPT: 03_Machine_Learning_Clasificacion.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Predicción de Fuga (Churn) usando Random Forest.
# -----------------------------------------------------------------------------

# 1. LIBRERÍAS
pacman::p_load(tidyverse, caret, randomForest, pROC, corrplot)

# 2. CARGA DE DATOS (Simulación de Clientes de Telecom/Banca)
set.seed(999)
n_clientes <- 1000
datos_ml <- data.frame(
  Antiguedad_Meses = sample(1:72, n_clientes, replace = TRUE),
  Renta = rlnorm(n_clientes, 13, 0.5),
  Reclamos_Anuales = rpois(n_clientes, lambda = 2),
  Uso_Datos_GB = runif(n_clientes, 0, 50),
  Plan_Premium = as.factor(sample(c("Si", "No"), n_clientes, replace = TRUE)),
  # Variable Objetivo: Fuga (0 = No se va, 1 = Se va)
  Fuga = as.factor(sample(c("No", "Si"), n_clientes, replace = TRUE, prob = c(0.8, 0.2)))
)

# 3. PREPROCESAMIENTO
# Partición de Datos (70% Entrenamiento - 30% Prueba)
indice <- createDataPartition(datos_ml$Fuga, p = 0.7, list = FALSE)
train_set <- datos_ml[indice, ]
test_set  <- datos_ml[-indice, ]

# 4. ENTRENAMIENTO DEL MODELO (Random Forest)
# Usamos validación cruzada (Cross Validation) de 5 pliegues
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

modelo_rf <- train(Fuga ~ ., 
                   data = train_set, 
                   method = "rf",
                   metric = "ROC", # Optimizamos curva ROC, no solo accuracy
                   trControl = ctrl)

print(modelo_rf)

# 5. IMPORTANCIA DE VARIABLES (¿Qué define que un cliente se vaya?)
varImp(modelo_rf)
plot(varImp(modelo_rf), main = "Variables más influyentes en la Fuga")

# 6. EVALUACIÓN Y PREDICCIÓN
predicciones_clase <- predict(modelo_rf, test_set)
predicciones_prob  <- predict(modelo_rf, test_set, type = "prob")

# Matriz de Confusión (Métrica clave para negocio)
conf_matrix <- confusionMatrix(predicciones_clase, test_set$Fuga, positive = "Si")
print(conf_matrix)
# Mirar: Sensitivity (Capacidad de detectar al que se va) y Specificity.

# 7. CURVA ROC
roc_obj <- roc(test_set$Fuga, predicciones_prob$Si)
plot(roc_obj, col = "blue", main = paste("Curva ROC - AUC:", round(auc(roc_obj), 3)))