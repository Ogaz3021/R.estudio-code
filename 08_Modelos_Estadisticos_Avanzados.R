# -----------------------------------------------------------------------------
# SCRIPT: 08_Modelos_Estadisticos_Avanzados.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Comparativa de LM, GLM (Poisson/Logit), GAM (No lineal) 
#              y Pruebas No Paramétricas (Wilcoxon/Kruskal).
# -----------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mgcv, broom, car, MASS)

# 1. SIMULACIÓN DE DATOS COMPLEJOS
set.seed(123)
n <- 300
x <- runif(n, 0, 10)
# Relación NO lineal (senoidal) + Ruido heterocedástico
y_raw <- 3 + 2*sin(x) + rnorm(n, 0, 0.5 * x) 
# Variable de Conteo (para GLM Poisson)
y_count <- rpois(n, lambda = exp(0.2 * x))
# Grupo (Categoría)
grupo <- sample(c("A", "B", "C"), n, replace = TRUE)
df <- data.frame(x, y_raw, y_count, grupo)

# -----------------------------------------------------------------------------
# A. MODELOS LINEALES Y DIAGNÓSTICO (LM)
# -----------------------------------------------------------------------------
modelo_lm <- lm(y_raw ~ x, data = df)

# Diagnóstico de supuestos (Breusch-Pagan para Homocedasticidad)
# Si p < 0.05, hay heterocedasticidad (varianza no constante)
ncvTest(modelo_lm) 

# -----------------------------------------------------------------------------
# B. MODELOS LINEALES GENERALIZADOS (GLM)
# -----------------------------------------------------------------------------
# Escenario: Predicción de conteos (ej: N° de fallas, N° de clientes).
# Usamos familia Poisson con enlace Log.
modelo_glm <- glm(y_count ~ x, data = df, family = poisson(link = "log"))

summary(modelo_glm)
# Exponenciamos coeficientes para interpretar como "Risk Ratio"
exp(coef(modelo_glm))

# -----------------------------------------------------------------------------
# C. MODELOS ADITIVOS GENERALIZADOS (GAM) - Modelos No Lineales
# -----------------------------------------------------------------------------
# Cuando la relación no es una recta, usamos Splines suavizados s().
# Esto cubre "Modelos No Lineales" de forma flexible.
modelo_gam <- gam(y_raw ~ s(x), data = df)

summary(modelo_gam)

# Visualización: Ajuste Lineal (Rojo) vs No Lineal GAM (Azul)
ggplot(df, aes(x, y_raw)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue", size = 1) +
  labs(title = "Comparativa: Regresión Lineal vs GAM (Splines)",
       subtitle = "El modelo GAM captura la curvatura senoidal que el LM ignora") +
  theme_minimal()

# -----------------------------------------------------------------------------
# D. MÉTODOS NO PARAMÉTRICOS
# -----------------------------------------------------------------------------
# Escenario: Comparar grupos cuando NO hay normalidad (ej: Outliers extremos).

# 1. Test de Kruskal-Wallis (ANOVA no paramétrico)
# H0: Las medianas de los grupos son iguales.
kruskal.test(y_raw ~ grupo, data = df)

# 2. Test de Wilcoxon (Mann-Whitney) para 2 grupos
subset_ab <- df %>% filter(grupo %in% c("A", "B"))
wilcox.test(y_raw ~ grupo, data = subset_ab)

message("Análisis completado: Se verificaron supuestos y se ajustaron modelos flexibles.")