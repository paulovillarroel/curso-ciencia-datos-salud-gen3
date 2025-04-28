# Instalar las librerías si no las tienes aún
install.packages(c("tidyverse", "rio"))

# Cargar las librerías necesarias
library(tidyverse)
library(rio)

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
  group_by(RegionGlosa) |>
  summarise(total_establecimientos = n()) |>
  arrange(desc(total_establecimientos))

export(estab_region, "clean-data/establecimientos_region.xlsx")

# Agrupar por tipo de establecimiento
estab_tipos <- establecimientos |>
  group_by(TipoEstablecimientoGlosa) |>
  summarise(total_establecimientos = n()) |>
  arrange(desc(total_establecimientos))

export(
  list(estab_region, estab_tipos),
  "clean-data/establecimientos_todos.xlsx"
)

# Filtrar por región
estab_coquimbo <- establecimientos |>
  filter(RegionGlosa == "Región De Coquimbo")

# Calcular media de antigüedad por región
establecimientos |>
  mutate(
    FechaInicioFuncionamientoEstab = dmy(FechaInicioFuncionamientoEstab),
    antiguedad_estab = today() - FechaInicioFuncionamientoEstab
  ) |>
  group_by(RegionGlosa) |>
  summarise(media_antiguedad = mean(antiguedad_estab, na.rm = TRUE))


establecimientos |>
  mutate(
    FechaInicioFuncionamientoEstab = dmy(FechaInicioFuncionamientoEstab),
    # Calcula la antigüedad directamente en años
    antiguedad_estab_anos = interval(FechaInicioFuncionamientoEstab, today()) /
      years(1)
  ) |>
  group_by(RegionGlosa) |>
  summarise(
    media_antiguedad_anos = mean(antiguedad_estab_anos, na.rm = TRUE)
  )
