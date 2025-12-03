# -----------------------------------------------------------------------------
# SCRIPT: 13_Poisson_Zero_Inflated.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Comparación de Poisson vs Zero-Inflated Poisson (ZIP) 
#              para datos con exceso de ceros (ej: Reclamos de Seguros).
# -----------------------------------------------------------------------------

pacman::p_load(tidyverse, pscl, boot, countreg)

# 1. SIMULACIÓN DE DATOS "DIFÍCILES"
set.seed(123)
n <- 1000
# Covariables
edad <- runif(n, 20, 60)
historial <- rbinom(n, 1, 0.3) # 1 = Tiene historial malo

# Proceso de ceros estructurales (Gente que NUNCA reclama)
prob_cero_absoluto <- 1 / (1 + exp(-(-2 + 0.05 * edad))) 
es_cero_estructural <- rbinom(n, 1, prob_cero_absoluto)

# Proceso de conteo (Gente que podría reclamar: 0, 1, 2, 3...)
lambda <- exp(-1 + 0.5 * historial)
conteos <- rpois(n, lambda)

# Resultado final: Si es estructural es 0, sino es el conteo Poisson
reclamos <- ifelse(es_cero_estructural == 1, 0, conteos)
df <- data.frame(reclamos, edad, historial)

# Visualizar el exceso de ceros
barplot(table(df$reclamos), main = "Histograma de Reclamos (Exceso de Ceros)", col = "orange")

# 2. MODELO 1: POISSON ESTÁNDAR (El error común)
mod_pois <- glm(reclamos ~ edad + historial, data = df, family = poisson)

# 3. MODELO 2: ZERO-INFLATED POISSON (La solución experta)
# Tiene dos partes: 
#   - dist = "poisson": Predice cuántos reclamos
#   - link = "logit": Predice la probabilidad de ser un "Cero Estructural"
mod_zip <- zeroinfl(reclamos ~ edad + historial | edad, data = df, dist = "poisson")

summary(mod_zip)

# 4. COMPARACIÓN DE MODELOS (Test de Vuong)
# H0: Los modelos son equivalentes. 
# Si p < 0.05, el ZIP es significativamente mejor.
vuong(mod_pois, mod_zip)

# 5. PREDICCIÓN DE PROBABILIDADES
# Imaginemos un cliente nuevo
nuevo_cliente <- data.frame(edad = 30, historial = 1)

# Predicción del número esperado de reclamos
pred_n <- predict(mod_zip, newdata = nuevo_cliente, type = "response")

# Predicción de probabilidad de que tenga CERO reclamos
pred_prob0 <- predict(mod_zip, newdata = nuevo_cliente, type = "prob")[,1]

message(paste("Reclamos esperados:", round(pred_n, 4)))
message(paste("Probabilidad de tener 0 reclamos:", percent(pred_prob0)))