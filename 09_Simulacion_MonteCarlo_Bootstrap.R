# -----------------------------------------------------------------------------
# SCRIPT: 09_Simulacion_MonteCarlo_Bootstrap.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Estimación de Intervalos de Confianza vía Bootstrap y 
#              Simulación de Riesgo Financiero (Monte Carlo).
# -----------------------------------------------------------------------------

pacman::p_load(tidyverse, boot, scales)

set.seed(999)

# -----------------------------------------------------------------------------
# A. BOOTSTRAP (Remuestreo)
# -----------------------------------------------------------------------------
# Escenario: Tenemos pocos datos y muy sesgados (ej: Siniestros de seguros).
# Queremos el Intervalo de Confianza (IC) de la MEDIANA (la media es engañosa).

datos_sesgados <- rlnorm(50, meanlog = 10, sdlog = 1.5) # Solo 50 datos

# 1. Definir función estadística para 'boot'
funcion_mediana <- function(data, indices) {
  d <- data[indices] # Permite el remuestreo con reemplazo
  return(median(d))
}

# 2. Ejecutar Bootstrap (R = 2000 réplicas)
boot_res <- boot(data = datos_sesgados, statistic = funcion_mediana, R = 2000)

# 3. Calcular Intervalo de Confianza (BCa es el método más robusto)
boot_ci <- boot.ci(boot_res, type = "bca")
print(boot_ci)

# Visualización del Bootstrap
plot(boot_res) 

# -----------------------------------------------------------------------------
# B. SIMULACIÓN DE MONTE CARLO
# -----------------------------------------------------------------------------
# Escenario: Proyección de rentabilidad de un activo financiero a 1 año.
# Precio inicial: 100. Retorno promedio diario: 0.1%. Volatilidad: 2%.

dias <- 252 # Días bursátiles
simulaciones <- 1000 # Escenarios posibles
precio_inicial <- 100
mu <- 0.001
sigma <- 0.02

# Matriz para guardar resultados
matriz_precios <- matrix(nrow = dias, ncol = simulaciones)
matriz_precios[1, ] <- precio_inicial

# Loop de simulación (Movimiento Browniano Geométrico)
for (j in 1:simulaciones) {
  for (i in 2:dias) {
    shock <- rnorm(1) # Aleatoriedad
    matriz_precios[i, j] <- matriz_precios[i-1, j] * exp((mu - 0.5 * sigma^2) + sigma * shock)
  }
}

# Transformar a formato Tidy para graficar
df_sim <- as.data.frame(matriz_precios) %>%
  mutate(Dia = 1:dias) %>%
  pivot_longer(-Dia, names_to = "Simulacion", values_to = "Precio")

# Visualización "Spaghetti Plot"
ggplot(df_sim, aes(x = Dia, y = Precio, group = Simulacion)) +
  geom_line(alpha = 0.05, color = "steelblue") + # Las 1000 líneas
  geom_hline(yintercept = precio_inicial, linetype = "dashed", color = "red") +
  labs(title = "Simulación de Monte Carlo: Proyección de Activo",
       subtitle = paste(simulaciones, "escenarios posibles a 1 año"),
       y = "Precio Estimado ($)") +
  theme_minimal()

# Cálculo de Valor en Riesgo (VaR) al 95%
precios_finales <- matriz_precios[dias, ]
VaR_95 <- quantile(precios_finales, 0.05)
message(paste("Con un 95% de confianza, el precio no caerá bajo:", round(VaR_95, 2)))