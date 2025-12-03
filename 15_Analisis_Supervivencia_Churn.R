# -----------------------------------------------------------------------------
# SCRIPT: 15_Analisis_Supervivencia_Churn.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Curvas de Kaplan-Meier y Modelo de Riesgo Proporcional de Cox.
# -----------------------------------------------------------------------------

pacman::p_load(tidyverse, survival, survminer)

# 1. DATOS (Dataset Lung es clásico, pero lo renombraremos a contexto Negocio)
# time: Días hasta que el cliente se va
# status: 1 = Se fue (Churn), 0 = Sigue activo (Censurado)
data("lung")
df <- lung %>% 
  mutate(
    Dias = time,
    Evento_Fuga = status - 1, # Ajustar a 0/1
    Sexo = ifelse(sex == 1, "Hombre", "Mujer")
  ) %>%
  select(Dias, Evento_Fuga, Sexo, age)

# 2. OBJETO DE SUPERVIVENCIA (La clave en R)
# Crea un objeto especial que maneja la censura (el "+")
surv_obj <- Surv(time = df$Dias, event = df$Evento_Fuga)

# 3. CURVA DE KAPLAN-MEIER (Descriptivo)
# Probabilidad de seguir siendo cliente a lo largo del tiempo
km_fit <- survfit(surv_obj ~ Sexo, data = df)

# Visualización Profesional (Risk Table)
ggsurvplot(km_fit, 
           data = df, 
           pval = TRUE, # Muestra p-value log-rank test
           conf.int = TRUE,
           risk.table = TRUE, # Muestra cuántos clientes quedan en riesgo
           palette = c("#E7B800", "#2E9FDF"),
           title = "Curva de Retención de Clientes por Sexo",
           xlab = "Días de Antigüedad",
           ylab = "Probabilidad de Retención")

# 4. MODELO DE COX (Regresión Multivariada)
# Para ver qué variables aumentan el riesgo de fuga
# Hazard Ratio (HR): 
# HR > 1: Aumenta riesgo de fuga (Malo)
# HR < 1: Factor de protección (Bueno)
cox_model <- coxph(surv_obj ~ Sexo + age, data = df)

summary(cox_model)

# Visualización de Hazard Ratios (Forest Plot)
ggforest(cox_model, data = df, main = "Hazard Ratios (Riesgo de Fuga)")

message("Interpretación: Si el HR de Edad es 1.02, por cada año extra, el riesgo de irse aumenta un 2%.")