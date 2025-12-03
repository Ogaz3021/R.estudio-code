# -----------------------------------------------------------------------------
# 1. PREPARACIÓN DEL ENTORNO
# -----------------------------------------------------------------------------
# Instalamos/Cargamos las librerías esenciales para un flujo de trabajo moderno
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Manipulación y gráficos (ggplot2, dplyr, readr)
  readxl,     # Leer Excel
  haven,      # Leer SPSS (.sav) y SAS
  nortest,    # Tests de normalidad extra (Anderson-Darling, etc.)
  skimr,      # Resumen exploratorio potente
  ggthemes,   # Temas estéticos para gráficos
  patchwork,  # Para unir varios gráficos en una sola imagen
  scales      # Formato de ejes (porcentajes, pesos)
)

# -----------------------------------------------------------------------------
# 2. CARGA DE DATOS (Simulación y Ejemplos de Lectura)
# -----------------------------------------------------------------------------

# --- A) Si tuvieras archivos externos (Ejemplos comentados) ---
# df <- read_csv("ruta/datos.csv")              # Archivos CSV
# df <- read_excel("ruta/datos.xlsx", sheet=1)  # Excel
# df <- read_sav("ruta/datos.sav")              # SPSS (mantiene etiquetas)

# --- B) Generación de Datos Simulados para este ejemplo ---
# Crearemos un dataframe que mezcle: Cuantitativos, Cualitativos y Escalas Likert
set.seed(123) # Para reproducibilidad

n <- 200 # Tamaño muestral

datos <- data.frame(
  ID = 1:n,
  # Variable Cuantitativa Continua (con un poco de asimetría)
  Ingreso = rlnorm(n, meanlog = 13, sdlog = 0.4),
  # Variable Cuantitativa Discreta
  Edad = round(rnorm(n, mean = 35, sd = 8)),
  # Variable Cualitativa Nominal
  Departamento = sample(c("Ventas", "RRHH", "TI", "Operaciones"), n, replace = TRUE),
  # Variables Tipo Likert (Ordinales) - 1: Muy en desacuerdo a 5: Muy de acuerdo
  P1_Ambiente = sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1)),
  P2_Liderazgo = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.4, 0.25)),
  P3_Sueldo    = sample(1:5, n, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1))
)

# Convertimos las Likert a Factores Ordenados (Clave para gráficos y análisis)
niveles_likert <- c("Muy en desacuerdo", "En desacuerdo", "Neutral", "De acuerdo", "Muy de acuerdo")

datos <- datos %>%
  mutate(
    across(starts_with("P"), ~ factor(., levels = 1:5, labels = niveles_likert, ordered = TRUE)),
    Departamento = as.factor(Departamento)
  )

# Introducimos algunos NA artificiales para enseñar limpieza
datos$Ingreso[sample(1:n, 5)] <- NA

# -----------------------------------------------------------------------------
# 3. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# -----------------------------------------------------------------------------

message("--- Vistazo General ---")
glimpse(datos)

message("--- Resumen Estadístico Robusto (skimr) ---")
# skim() es mejor que summary() porque separa por tipo de variable y da histogramas mini
skim(datos)

# Limpieza básica: Eliminar NA o imputar (aquí eliminamos por simplicidad)
datos_clean <- datos %>% drop_na()

# -----------------------------------------------------------------------------
# 4. ANÁLISIS DE VARIABLES CUANTITATIVAS (Ingreso)
# -----------------------------------------------------------------------------

# A. Estadísticos Descriptivos
resumen_ingreso <- datos_clean %>%
  summarise(
    Media = mean(Ingreso),
    Mediana = median(Ingreso),
    Desv_Std = sd(Ingreso),
    CV = sd(Ingreso) / mean(Ingreso) * 100, # Coeficiente de Variación
    Min = min(Ingreso),
    Max = max(Ingreso)
  )
print(resumen_ingreso)

# B. Visualización: Histograma + Boxplot + Densidad
p1 <- ggplot(datos_clean, aes(x = Ingreso)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Distribución de Ingresos", subtitle = "Histograma con curva de densidad") +
  theme_minimal()

p2 <- ggplot(datos_clean, aes(x = Ingreso, y = "")) + # Boxplot horizontal
  geom_boxplot(fill = "#404080", alpha = 0.6) +
  labs(title = "Detección de Outliers", x = "Ingreso", y = "") +
  theme_minimal()

# Usamos patchwork para mostrar ambos
p1 / p2

# -----------------------------------------------------------------------------
# 5. ANÁLISIS DE NORMALIDAD
# -----------------------------------------------------------------------------
# Importante: Shapiro-Wilk es para n < 50 (aprox), Kolmogorov-Smirnov para n grandes.
# Como n=200, miramos ambos (o Anderson-Darling que es muy potente).

# Test de Shapiro-Wilk
shapiro_test <- shapiro.test(datos_clean$Ingreso)

# Test de Anderson-Darling (del paquete nortest)
ad_test <- ad.test(datos_clean$Ingreso)

# QQ-Plot (Evaluación visual de normalidad)
qq_plot <- ggplot(datos_clean, aes(sample = Ingreso)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "Q-Q Plot de Ingresos", subtitle = paste("Shapiro p-value:", signif(shapiro_test$p.value, 4))) +
  theme_light()

print(qq_plot)
# Interpretación: Si los puntos se alejan de la línea roja, no es normal.
# Si p-value < 0.05, rechazamos H0 (No es normal).

# -----------------------------------------------------------------------------
# 6. ANÁLISIS DE VARIABLES CUALITATIVAS (Departamento)
# -----------------------------------------------------------------------------

tabla_freq <- datos_clean %>%
  count(Departamento) %>%
  mutate(Porcentaje = n / sum(n))

ggplot(tabla_freq, aes(x = reorder(Departamento, -n), y = n, fill = Departamento)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1)), vjust = -0.5) +
  labs(title = "Distribución por Departamento", x = "Departamento", y = "Cantidad") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 7. ANÁLISIS Y VISUALIZACIÓN DE ESCALAS LIKERT
# -----------------------------------------------------------------------------
# Aquí el truco es pivotar los datos para graficar todas las preguntas juntas.
# Haremos un gráfico de barras apiladas al 100% (Diverging Bar Chart simulado).

datos_likert_long <- datos_clean %>%
  select(ID, starts_with("P")) %>%
  pivot_longer(cols = starts_with("P"), names_to = "Pregunta", values_to = "Respuesta") %>%
  group_by(Pregunta, Respuesta) %>%
  summarise(Conteo = n(), .groups = 'drop') %>%
  group_by(Pregunta) %>%
  mutate(Porcentaje = Conteo / sum(Conteo))

# Gráfico de Barras Apiladas (Estilo Likert)
ggplot(datos_likert_long, aes(x = Pregunta, y = Porcentaje, fill = Respuesta)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  coord_flip() + # Volteamos para leer mejor las preguntas
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu") + # Rojo (Mal) a Azul (Bien)
  labs(
    title = "Resultados de Encuesta de Clima",
    subtitle = "Visualización de Escalas Likert",
    x = "", y = "Proporción"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# 8. ANÁLISIS BIVARIADO (Relación Cuantitativa vs Cualitativa)
# -----------------------------------------------------------------------------
# Ejemplo: ¿Hay diferencia de Ingresos según Departamento?

# Visualización
ggplot(datos_clean, aes(x = Departamento, y = Ingreso, fill = Departamento)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3) + # Muestra los puntos reales
  labs(title = "Ingreso según Departamento", subtitle = "Análisis ANOVA visual") +
  theme_bw() +
  theme(legend.position = "none")

# Test Estadístico (ANOVA de una vía)
# Primero verificamos homogeneidad de varianzas (Levene o Bartlett)
bartlett.test(Ingreso ~ Departamento, data = datos_clean)

# Ejecutamos ANOVA
modelo_anova <- aov(Ingreso ~ Departamento, data = datos_clean)
summary(modelo_anova)

# Si el ANOVA es significativo (p < 0.05), haríamos post-hoc Tukey
# TukeyHSD(modelo_anova)