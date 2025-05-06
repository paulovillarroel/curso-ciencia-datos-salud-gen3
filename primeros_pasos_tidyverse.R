# Instalar las librerías si no las tienes aún
install.packages(c("tidyverse", "rio"))

# Cargar las librerías necesarias
library(tidyverse)
library(rio)

# Paso 1: Leer el archivo
establecimientos <- import("raw-data/establecimientos_20250422.csv")

establecimientos <- establecimientos |>
  janitor::clean_names()

# Analizar estructura de los datos
dim(establecimientos)
str(establecimientos)
glimpse(establecimientos)
summary(establecimientos)
colnames(establecimientos)

# Total de registros
establecimientos |>
  summarise(total_establecimientos = n())

# Agrupar por región
estab_region <- establecimientos |>
  group_by(region_glosa) |>
  summarise(total_establecimientos = n()) |>
  arrange(desc(total_establecimientos))

export(estab_region, "clean-data/establecimientos_region.xlsx")

# Agrupar por tipo de establecimiento
estab_tipos <- establecimientos |>
  group_by(tipo_establecimiento_glosa) |>
  summarise(total_establecimientos = n()) |>
  arrange(desc(total_establecimientos))

export(
  list(estab_region, estab_tipos),
  "clean-data/establecimientos_todos.xlsx"
)

# Establecimiento público en comunas
estab_publicos_comunas <- establecimientos |>
  filter(
    tipo_pertenencia_estab_glosa ==
      "Perteneciente al Sistema Nacional de Servicios de Salud"
  ) |>
  group_by(region_glosa, comuna_glosa) |>
  summarise(total_establecimientos = n()) |>
  arrange(desc(total_establecimientos))

# Filtrar por región
estab_coquimbo <- establecimientos |>
  filter(region_glosa == "Región De Coquimbo")

# Calcular media de antigüedad por región
establecimientos |>
  mutate(
    fecha_inicio_funcionamiento_estab = dmy(fecha_inicio_funcionamiento_estab),
    antiguedad_estab = today() - fecha_inicio_funcionamiento_estab
  ) |>
  group_by(region_glosa) |>
  summarise(media_antiguedad = mean(antiguedad_estab, na.rm = TRUE))

# Calcular media de antigüedad en años por región
establecimientos |>
  mutate(
    fecha_inicio_funcionamiento_estab = dmy(fecha_inicio_funcionamiento_estab),
    # Calcula la antigüedad directamente en años
    antiguedad_estab_anos = interval(
      fecha_inicio_funcionamiento_estab,
      today()
    ) /
      years(1)
  ) |>
  group_by(region_glosa) |>
  summarise(
    media_antiguedad_anos = mean(antiguedad_estab_anos, na.rm = TRUE)
  )
