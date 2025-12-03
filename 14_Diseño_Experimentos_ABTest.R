# -----------------------------------------------------------------------------
# SCRIPT: 14_Diseño_Experimentos_ABTest.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Cálculo de Tamaño Muestral (Power Analysis) y Análisis de A/B Test.
# -----------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, pwr, rstatix, ggpubr)

# -----------------------------------------------------------------------------
# 1. FASE DE DISEÑO (Antes del experimento)
# -----------------------------------------------------------------------------
# Pregunta de Negocio: Queremos detectar un aumento del 10% al 12% en la tasa de conversión.
# ¿Cuántos usuarios necesito en cada grupo (Control vs Test)?

# h: Tamaño del efecto (Cohen's h para proporciones)
p1 <- 0.10 # Tasa actual
p2 <- 0.12 # Tasa esperada
h_effect <- ES.h(p1, p2)

# Cálculo de Poder
# sig.level = 0.05 (Error Tipo I / Falsos Positivos)
# power = 0.80 (Error Tipo II / Capacidad de detectar el efecto si existe)
analisis_poder <- pwr.2p.test(h = h_effect, sig.level = 0.05, power = 0.80)

print(analisis_poder)
n_necesario <- ceiling(analisis_poder$n)
message(paste("Necesitas", n_necesario, "usuarios por grupo para validar el experimento."))

# -----------------------------------------------------------------------------
# 2. ANÁLISIS DE RESULTADOS (Simulación de Datos)
# -----------------------------------------------------------------------------
set.seed(123)
# Grupo Control (A): Mantiene el 10%
grupo_A <- rbinom(n = n_necesario, size = 1, prob = 0.10)
# Grupo Test (B): Logró subir al 12.5% (un poco mejor de lo esperado)
grupo_B <- rbinom(n = n_necesario, size = 1, prob = 0.125)

df_ab <- data.frame(
  Grupo = rep(c("Control", "Tratamiento"), each = n_necesario),
  Conversion = c(grupo_A, grupo_B)
)

# 3. TEST DE HIPÓTESIS (Proportion Test)
tabla_contingencia <- table(df_ab$Grupo, df_ab$Conversion)
# Conversion = 1 es la columna de interés
prop_test <- prop.test(tabla_contingencia, correct = FALSE) 

print(prop_test)

# 4. VISUALIZACIÓN DE "LIFT" (Incremento)
df_resumen <- df_ab %>%
  group_by(Grupo) %>%
  summarise(
    Tasa = mean(Conversion),
    Error_Std = sqrt(Tasa * (1 - Tasa) / n()),
    Conf_Low = Tasa - 1.96 * Error_Std,
    Conf_High = Tasa + 1.96 * Error_Std
  )

ggplot(df_resumen, aes(x = Grupo, y = Tasa, fill = Grupo)) +
  geom_col(alpha = 0.6, width = 0.5) +
  geom_errorbar(aes(ymin = Conf_Low, ymax = Conf_High), width = 0.2) +
  geom_text(aes(label = scales::percent(Tasa, accuracy = 0.1)), vjust = -2) +
  labs(title = "Resultados A/B Test", 
       subtitle = paste("P-value:", round(prop_test$p.value, 4), 
                        ifelse(prop_test$p.value < 0.05, "(Significativo)", "(No Signif.)")),
       y = "Tasa de Conversión") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.15))