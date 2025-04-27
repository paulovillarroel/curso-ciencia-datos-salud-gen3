# Instala los paquetes si no los tienes aún
install.packages(c("tidyverse", "RSQLite", "DBI"))

# Carga las librerías necesarias
library(tidyverse)
library(DBI)
library(RSQLite)

# Paso 1: Leer el archivo
establecimientos <- read_csv2("raw-data/establecimientos_20250422.csv")

# Paso 2: Crear o conectar a una base de datos SQLite
con <- dbConnect(RSQLite::SQLite(), "establecimientos.db")

# Paso 3: Escribir los datos a una tabla
dbWriteTable(con, "establecimientos_db", establecimientos)

# (Opcional) Consultar la tabla
dbListTables(con)
dbReadTable(con, "establecimientos_db")

# Consultas SQL
todos_establecimientos <- dbGetQuery(con, "SELECT * FROM establecimientos_db;")

establecimientos_punta_arenas <- dbGetQuery(
  con,
  "SELECT * FROM establecimientos_db WHERE ComunaGlosa = 'Punta Arenas';"
)

# Cierra la conexión cuando termines
dbDisconnect(con, shutdown = TRUE)
