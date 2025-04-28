# Instala los paquetes si no los tienes aún
install.packages(c("rio", "RSQLite", "DBI"))

# Carga las librerías necesarias
library(DBI)
library(RSQLite)
library(rio)

# Paso 1: Leer el archivo
establecimientos <- import("raw-data/establecimientos_20250422.csv")

# Paso 2: Crear o conectar a una base de datos SQLite
con <- dbConnect(RSQLite::SQLite(), "establecimientos.db")

# Paso 3: Escribir los datos a una tabla
dbWriteTable(con, "establecimientos_db", establecimientos, overwrite = TRUE)

# (Opcional) Consultar la tabla
dbListTables(con)
dbReadTable(con, "establecimientos_db")

# Cierra la conexión cuando termines
dbDisconnect(con, shutdown = TRUE)
