# -----------------------------------------------------------------------------
# SCRIPT: 11_Modelos_No_Lineales_NLS.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Ajuste de curvas de crecimiento y saturación (Michaelis-Menten).
# -----------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, nlstools)

# 1. SIMULACIÓN: Inversión en Publicidad vs Ventas (Efecto Saturación)
set.seed(123)
inversion <- seq(0, 100, length.out = 50)
# Modelo teórico: Ventas = Vmax * Inversion / (K + Inversion)
# Vmax = 200 (Techo máximo de ventas), K = 15 (Velocidad de crecimiento)
ventas <- (200 * inversion) / (15 + inversion) + rnorm(50, 0, 5) # + Ruido
df <- data.frame(inversion, ventas)

# 2. COMPARATIVA: ¿Qué pasa si usamos Regresión Lineal?
modelo_lm <- lm(ventas ~ inversion, data = df)

# 3. AJUSTE NO LINEAL (NLS)
# Definimos la fórmula matemática explícita
# start = list(...) son los valores iniciales para que el algoritmo itere
modelo_nls <- nls(ventas ~ Vmax * inversion / (K + inversion), 
                  data = df, 
                  start = list(Vmax = 150, K = 10))

summary(modelo_nls)

# 4. VISUALIZACIÓN DE AJUSTES
df$pred_lineal <- predict(modelo_lm)
df$pred_nls <- predict(modelo_nls)

ggplot(df, aes(x = inversion, y = ventas)) +
  geom_point(alpha = 0.6, size = 3) +
  # Línea Roja: Modelo Lineal (Malo: predice ventas infinitas)
  geom_line(aes(y = pred_lineal), color = "red", linetype = "dashed", size = 1) +
  # Línea Azul: Modelo No Lineal (Bueno: detecta el techo/saturación)
  geom_line(aes(y = pred_nls), color = "blue", size = 1.2) +
  labs(title = "Retornos Decrecientes en Marketing",
       subtitle = "Rojo: Regresión Lineal | Azul: Modelo No Lineal (Saturación)",
       x = "Inversión ($)", y = "Ventas Generadas") +
  theme_minimal()

# 5. DIAGNÓSTICO DE RESIDUOS (Bootstrapping para NLS)
# nlsBoot hace un remuestreo para ver qué tan estables son los parámetros Vmax y K
res_boot <- nlsBoot(modelo_nls, niter = 200)
summary(res_boot)
plot(res_boot, type = "boxplot")