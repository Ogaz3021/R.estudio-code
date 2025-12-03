# 📊 Ingeniería Estadística y Ciencia de Datos en R

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Tidyverse](https://img.shields.io/badge/Tidyverse-Core-blue?style=for-the-badge)
![Shiny](https://img.shields.io/badge/Shiny-Dashboard-blueviolet?style=for-the-badge)
![Inferential Statistics](https://img.shields.io/badge/Statistics-Advanced-orange?style=for-the-badge)

> **"Transformando datos en decisiones estratégicas mediante Inferencia Robusta y Modelamiento Avanzado."**

## 👨‍💻 Sobre este Repositorio
Este portafolio consolida mi experiencia como **Ingeniero Estadístico**, utilizando el ecosistema de **R** para resolver problemas complejos de análisis de datos.

A diferencia de los repositorios estándar de Data Science, esta colección profundiza en la **teoría estadística aplicada**: desde el diseño de experimentos y control de calidad industrial, hasta la simulación estocástica y modelos bayesianos, demostrando un dominio técnico que va más allá del simple ajuste de modelos predictivos.

---

## 📂 Módulos del Proyecto

El repositorio contiene scripts especializados organizados por áreas de dominio:

### 🔹 I. Fundamentos e Ingeniería de Datos
Scripts enfocados en la calidad del dato, análisis exploratorio y conexión a bases de datos.

| Archivo en Repo | Descripción Técnica | Librerías Clave |
| :--- | :--- | :--- |
| `Codigo de carga y analisis descriptivo...` | **EDA Avanzado**: Limpieza, tests de normalidad y visualización descriptiva. | `tidyverse`, `skimr` |
| `conexión SQL.R` | **Ingeniería de Datos**: Conexión a BD, consultas SQL y transformación ETL. | `DBI`, `dbplyr` |

### 🔹 II. Modelamiento Predictivo y Machine Learning
Algoritmos supervisados para clasificación y series temporales.

| Archivo en Repo | Descripción Técnica | Librerías Clave |
| :--- | :--- | :--- |
| `series de tiempo.R` | **Forecasting**: Modelos ARIMA/SARIMA y descomposición estacional. | `forecast`, `tseries` |
| `machine learning.R` | **Clasificación**: Random Forest para predicción de fuga o riesgo. | `caret`, `randomForest` |
| `10_Redes_Neuronales_DeepLearning.R` | **Deep Learning**: Perceptrón Multicapa (MLP) para patrones no lineales. | `neuralnet` |

### 🔹 III. Inferencia Estadística Avanzada
Modelos para situaciones donde los supuestos clásicos no se cumplen.

| Archivo en Repo | Descripción Técnica | Librerías Clave |
| :--- | :--- | :--- |
| `08_Modelos_Estadisticos_Avanzados.R` | **GLM & GAM**: Regresión de Poisson, Logística y suavizado no lineal. | `mgcv`, `MASS` |
| `11_Modelos_No_Lineales_NLS.R` | **Curvas de Saturación**: Ajuste de modelos no lineales (Michaelis-Menten). | `nlstools` |
| `12_Modelos_Bayesianos.R` | **Bayesiano**: Inferencia probabilística mediante simulación MCMC. | `rstanarm` |
| `13_Poisson_Zero_Inflated.R` | **Modelos ZIP**: Manejo de exceso de ceros en datos de conteo. | `pscl`, `countreg` |

### 🔹 IV. Aplicaciones Industriales y de Negocios
Herramientas para la toma de decisiones en entornos reales (Banca, Retail, Industria).

| Archivo en Repo | Descripción Técnica | Librerías Clave |
| :--- | :--- | :--- |
| `dashboard en R.R` | **Shiny App**: Dashboard interactivo para visualización de KPIs. | `shiny`, `shinydashboard` |
| `09_Simulacion_MonteCarlo_Bootstrap.R` | **Riesgo Financiero**: Simulación Monte Carlo y Bootstrap para intervalos robustos. | `boot` |
| `14_Diseño_Experimentos_ABTest.R` | **A/B Testing**: Cálculo de tamaño muestral (Power Analysis) y validación. | `pwr` |
| `15_Analisis_Supervivencia_Churn.R` | **Survival Analysis**: Curvas Kaplan-Meier y regresión de Cox para Churn. | `survival`, `survminer` |
| `16_Control_Calidad_SixSigma.R` | **Calidad Industrial**: Gráficos de Control (SPC) y capacidad (Cpk). | `qcc` |

### 🔹 V. Aprendizaje No Supervisado
| Archivo en Repo | Descripción Técnica | Librerías Clave |
| :--- | :--- | :--- |
| `07_Analisis_Multivariante_...R` | **Multivariante**: PCA, Clustering (K-Means) y Dendrogramas. | `factoextra`, `cluster` |

---

## 🛠️ Instalación y Requisitos

Para asegurar la reproducibilidad de estos scripts, recomiendo utilizar el gestor de paquetes `pacman`, el cual está integrado en el código para cargar e instalar librerías automáticamente.

```r
# Ejecuta esto en tu consola de R para instalar las dependencias principales
install.packages("pacman")
pacman::p_load(tidyverse, caret, shiny, forecast, survival, rstanarm, qcc)
