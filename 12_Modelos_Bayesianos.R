# -----------------------------------------------------------------------------
# SCRIPT: 12_Modelos_Bayesianos.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Regresión Lineal Bayesiana con rstanarm.
# -----------------------------------------------------------------------------

pacman::p_load(tidyverse, rstanarm, bayesplot)

# 1. DATOS
# Usamos 'mtcars' para predecir consumo (mpg) según peso (wt)
data("mtcars")

# 2. MODELADO BAYESIANO
# stan_glm usa sintaxis idéntica a lm(), pero usa simulación MCMC (Cadenas de Markov)
# chains = 4: Cuatro simulaciones paralelas
modelo_bayes <- stan_glm(mpg ~ wt, data = mtcars, 
                         family = gaussian(), 
                         chains = 4, iter = 2000, seed = 123)

# 3. INTERPRETACIÓN DE RESULTADOS
# En Bayes no miramos p-valores, miramos Intervalos de Credibilidad
print(modelo_bayes, digits = 3)

# "Median" es el estimador central. 
# "MAD_SD" es la desviación estándar de la posterior (incertidumbre).

# 4. VISUALIZACIÓN POSTERIOR (El "cerro" de probabilidad)
# Aquí vemos la distribución de probabilidad del coeficiente del Peso (wt)
mcmc_areas(modelo_bayes, pars = "wt", prob = 0.95) +
  labs(title = "Distribución Posterior del efecto del Peso",
       subtitle = "Toda la masa está bajo 0 -> Certeza de efecto negativo")

# 5. PREDICCIÓN BAYESIANA (Posterior Predictive Check)
# Comparamos los datos reales (y) con lo que el modelo simula (y_rep)
pp_check(modelo_bayes) + 
  labs(title = "Validación: Datos Reales vs Simulaciones del Modelo")
# Si las líneas celestes cubren la línea negra, el modelo es bueno.