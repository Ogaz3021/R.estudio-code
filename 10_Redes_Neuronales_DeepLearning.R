# -----------------------------------------------------------------------------
# SCRIPT: 10_Redes_Neuronales_DeepLearning.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Entrenamiento de Red Neuronal Perceptrón Multicapa (MLP).
# -----------------------------------------------------------------------------

pacman::p_load(tidyverse, neuralnet, caret, scales)

# 1. PREPARACIÓN DE DATOS
# Usaremos un dataset de clasificación binaria (ej: Aceptación de Crédito)
# Simulamos datos no lineales (XOR problem, difícil para modelos lineales)
set.seed(123)
n <- 500
df <- data.frame(
  Edad_Norm = runif(n, 0, 1),
  Ingreso_Norm = runif(n, 0, 1)
)
# La variable respuesta depende de una interacción compleja (círculo central)
df$Aprobado <- ifelse((df$Edad_Norm-0.5)^2 + (df$Ingreso_Norm-0.5)^2 < 0.15, 1, 0)

# Visualizar el problema (Los puntos rojos están al centro)
plot(df$Edad_Norm, df$Ingreso_Norm, col = ifelse(df$Aprobado==1, "red", "black"), pch=19,
     main = "Problema No Lineal (Target concéntrico)")

# 2. ENTRENAMIENTO DE LA RED NEURONAL
# Fórmula: Target ~ Inputs
# hidden = c(3, 2) -> Dos capas ocultas: primera con 3 neuronas, segunda con 2.
# linear.output = FALSE -> Para clasificación (función de activación sigmoide)
nn_model <- neuralnet(Aprobado ~ Edad_Norm + Ingreso_Norm, 
                      data = df, 
                      hidden = c(4, 3), 
                      linear.output = FALSE,
                      stepmax = 1e6)

# 3. VISUALIZACIÓN DE LA TOPOLOGÍA (¡Esto impresiona mucho!)
# Muestra los pesos sinápticos y las neuronas
plot(nn_model, rep = "best", show.weights = FALSE, 
     main = "Topología de Red Neuronal Profunda")

# 4. PREDICCIÓN Y EVALUACIÓN
# neuralnet devuelve probabilidades
predicciones <- compute(nn_model, df[, c("Edad_Norm", "Ingreso_Norm")])
probabilidad <- predicciones$net.result

# Convertir probabilidad a clase (Corte 0.5)
clase_predicha <- ifelse(probabilidad > 0.5, 1, 0)

# Matriz de Confusión
tabla_conf <- table(Real = df$Aprobado, Predicho = clase_predicha)
print(tabla_conf)

accuracy <- sum(diag(tabla_conf)) / sum(tabla_conf)
message(paste("Precisión del Modelo (Accuracy):", percent(accuracy)))

# Si la precisión es alta (>90%), la red aprendió el patrón circular no lineal
# que una regresión logística simple no podría capturar bien.