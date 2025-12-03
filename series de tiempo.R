# -----------------------------------------------------------------------------
# SCRIPT: 02_Series_Tiempo_Forecasting.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Modelamiento ARIMA, pruebas de estacionariedad y predicción.
# -----------------------------------------------------------------------------

# 1. LIBRERÍAS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, forecast, tseries, lubridate, zoo, ggfortify)

# 2. GENERACIÓN DE DATA SIMULADA (Ventas Mensuales con Tendencia y Estacionalidad)
set.seed(123)
fechas <- seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")
n <- length(fechas)

# Componentes: Tendencia lineal + Estacionalidad anual + Ruido
tendencia <- seq(100, 200, length.out = n)
estacionalidad <- 20 * sin(2 * pi * (1:n) / 12)
ruido <- rnorm(n, mean = 0, sd = 10)

ventas <- tendencia + estacionalidad + ruido

# Crear objeto TS (Time Series) - Fundamental en R
ts_ventas <- ts(ventas, start = c(2018, 1), frequency = 12)

# 3. ANÁLISIS EXPLORATORIO DE SERIES DE TIEMPO
# Descomposición (STL: Seasonal and Trend decomposition using Loess)
descomposicion <- stl(ts_ventas, s.window = "periodic")
autoplot(descomposicion) +
  labs(title = "Descomposición de Ventas", subtitle = "Tendencia, Estacionalidad y Residuo") +
  theme_minimal()

# 4. VERIFICACIÓN DE ESTACIONARIEDAD (Test Dickey-Fuller Aumentado)
# H0: La serie NO es estacionaria (tiene raíz unitaria)
adf_test <- adf.test(ts_ventas)
print(adf_test)

# Si p-value > 0.05, diferenciamos la serie
ts_diff <- diff(ts_ventas)
autoplot(ts_diff) + labs(title = "Serie Diferenciada (Estacionaria)")

# 5. MODELAMIENTO (ARIMA)
# auto.arima busca los mejores parámetros (p,d,q)(P,D,Q) basado en AIC
modelo_arima <- auto.arima(ts_ventas, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

summary(modelo_arima) # Ver coeficientes y métricas (RMSE, MAE)

# Chequeo de Residuos (White Noise)
checkresiduals(modelo_arima)
# Ljung-Box test: si p-value > 0.05, los residuos son ruido blanco (modelo válido)

# 6. FORECASTING (Proyección)
# Proyectar próximos 12 meses
proyeccion <- forecast(modelo_arima, h = 12)

# Visualización Profesional
autoplot(proyeccion) +
  labs(title = "Proyección de Ventas 2025",
       y = "Ventas (Unidades)", x = "Año") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))

# Exportar resultados
df_proyeccion <- as.data.frame(proyeccion)
write.csv(df_proyeccion, "output_proyeccion_2025.csv")