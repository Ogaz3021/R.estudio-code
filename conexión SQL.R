# -----------------------------------------------------------------------------
# SCRIPT: 04_Conexion_SQL_ETL.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Conexión a bases de datos, consultas SQL nativas y dbplyr.
# -----------------------------------------------------------------------------

# 1. LIBRERÍAS
pacman::p_load(tidyverse, DBI, RSQLite, dbplyr)

# 2. CREACIÓN DE ENTORNO SQL SIMULADO (Para portafolio)
# En una empresa real, usarías: dbConnect(RPostgres::Postgres(), ...)
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Cargamos datos de ejemplo de R a la base de datos SQL virtual
copy_to(con, mtcars, "autos_db", temporary = FALSE)
copy_to(con, iris, "flores_db", temporary = FALSE)

# 3. CONSULTAS SQL DIRECTAS (Modo Clásico)
# Escribir query en SQL puro
query_sql <- "SELECT cyl, AVG(mpg) as promedio_mpg, COUNT(*) as cantidad 
              FROM autos_db 
              GROUP BY cyl 
              HAVING cantidad > 5"

resultado_sql <- dbGetQuery(con, query_sql)
print(resultado_sql)

# 4. CONSULTAS CON DBPLYR (Modo R - Lazy Evaluation)
# Esto es magia: Escribes en R, pero se ejecuta en el servidor SQL
tbl_autos <- tbl(con, "autos_db")

consulta_dplyr <- tbl_autos %>%
  filter(hp > 100) %>%
  select(car_name = row_names, mpg, hp, cyl) %>%
  group_by(cyl) %>%
  summarise(
    media_hp = mean(hp, na.rm = TRUE),
    total = n()
  ) 

# Ver el código SQL que R generó por detrás
show_query(consulta_dplyr)

# "Bajar" los datos a R (Solo cuando el resumen está listo)
datos_finales <- collect(consulta_dplyr)
print(datos_finales)

# 5. CERRAR CONEXIÓN
dbDisconnect(con)