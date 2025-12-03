# -----------------------------------------------------------------------------
# SCRIPT: 07_Analisis_Multivariante_PCA_Cluster.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Reducción de dimensiones (PCA), Clustering (K-Means) y Dendrogramas.
# -----------------------------------------------------------------------------

# 1. LIBRERÍAS ESPECIALIZADAS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, cluster, factoextra, dendextend, corrplot)

# 2. CARGA Y PREPARACIÓN DE DATOS
# Usaremos 'USArrests' (dataset clásico de criminalidad) para el ejemplo
data("USArrests")
df <- USArrests

# ¡PASO CRÍTICO!: Estandarizar los datos (Escalar)
# Si no escalas, la variable con números más grandes domina el análisis.
df_scaled <- scale(df)

# 3. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# Objetivo: Reducir variables manteniendo la información (varianza)
pca_res <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# A. Visualización de Varianza Explicada (Scree Plot)
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 70), 
         main = "Scree Plot: % Varianza por Componente")

# B. Biplot (Mapa de Variables + Individuos)
# Esto muestra qué variables están correlacionadas y cómo se agrupan los estados
fviz_pca_biplot(pca_res, 
                repel = TRUE, # Evita que los textos se superpongan
                col.var = "#2E9FDF", # Color variables
                col.ind = "#696969",  # Color individuos
                title = "PCA Biplot - Mapa de Criminalidad"
)

# 4. CLUSTERING JERÁRQUICO (DENDROGRAMA)
# Objetivo: Ver visualmente cómo se agrupan los datos paso a paso
dist_mat <- dist(df_scaled, method = "euclidean")
hclust_avg <- hclust(dist_mat, method = "average")

# Graficar Dendrograma con colores
dend <- as.dendrogram(hclust_avg)
dend %>% 
  set("branches_k_color", k = 4) %>% # Cortamos en 4 grupos
  set("labels_cex", 0.6) %>% 
  plot(main = "Dendrograma de Clustering Jerárquico")
rect.hclust(hclust_avg, k = 4, border = 2:5) # Cajas alrededor de los grupos

# 5. K-MEANS CLUSTERING (Particional)
# A. Determinar número óptimo de clusters (Método del Codo / Elbow)
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Método del Codo (Elbow Method)")

# B. Ejecutar K-Means con k=4
set.seed(123)
kmeans_res <- kmeans(df_scaled, centers = 4, nstart = 25)

# C. Visualización de Clusters
fviz_cluster(kmeans_res, data = df_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Dibuja polígonos alrededor
             ggtheme = theme_minimal(),
             main = "Resultados K-Means (k=4)"
)

# 6. INTERPRETACIÓN DE RESULTADOS
# Agregamos el cluster a la base original para describir los perfiles
df_final <- df %>%
  mutate(Cluster = as.factor(kmeans_res$cluster))

# Perfilamiento: Promedio de cada variable por Cluster
perfil <- df_final %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean))

print(perfil)
# Aquí explicas: "El Cluster 1 tiene alto asalto pero bajo asesinato...", etc.