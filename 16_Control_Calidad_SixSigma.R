# -----------------------------------------------------------------------------
# SCRIPT: 16_Control_Calidad_SixSigma.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Gráficos de Control (Xbar-R) y Análisis de Capacidad (Cpk).
# -----------------------------------------------------------------------------

pacman::p_load(tidyverse, qcc, SixSigma)

# 1. SIMULACIÓN: Proceso de llenado de botellas (Objetivo: 500ml)
set.seed(123)
# Generamos 30 muestras de 5 observaciones cada una
datos_calidad <- qcc.groups(rnorm(150, 500, 2), rep(1:30, each = 5))

# 2. GRÁFICO DE CONTROL X-BARRA (Media del proceso)
# Detecta si el proceso se descentró
obj_qcc <- qcc(datos_calidad, type = "xbar", 
               plot = FALSE) # Lo graficamos manual abajo para ver detalles

plot(obj_qcc, title = "Gráfico de Control X-Barra (Estabilidad Media)")

# Verificamos si hay puntos fuera de control (Reglas de Nelson automáticas)
violaciones <- obj_qcc$violations
if(length(violaciones$beyond.limits) > 0) {
  message("¡ALERTA!: El proceso está fuera de control estadístico.")
} else {
  message("El proceso es estable.")
}

# 3. ANÁLISIS DE CAPACIDAD (Capability Analysis)
# ¿El proceso cumple las especificaciones del cliente?
# LSL = 494ml, USL = 506ml (Límites de Especificación)
lsl <- 494
usl <- 506

capacidad <- process.capability(obj_qcc, spec.limits = c(lsl, usl))

plot(capacidad)

# Interpretación de Índices
cpk <- capacidad$indices["Cpk", "Value"]
message(paste("Índice Cpk:", round(cpk, 2)))
# Cpk < 1.0 : Proceso incapaz (Produce defectos)
# Cpk > 1.33: Proceso capaz (Six Sigma aceptable)