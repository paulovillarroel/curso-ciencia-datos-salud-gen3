library(tidyverse)

rem20 <- read_csv2(
  "https://datos.gob.cl/dataset/a756323f-85ba-4759-87dc-f5d5e63868cc/resource/657cc933-eac8-4bfc-b004-c4d6dcd988a8/download/indicadores_rem20_20250425.csv"
)

rem20 <- rem20 |>
  janitor::clean_names()

arica <- rem20 |>
  filter(codigo_establecimiento == 101100) # ojo con el nombre

unique(arica$establecimiento)

unique(arica$area_funcional)

arica |>
  group_by(area_funcional) |>
  summarise(total_egresos = sum(numero_egresos)) |>
  arrange(desc(total_egresos))

arica_obstetricia <- arica |>
  filter(area_funcional == "Área Obstetricia") |>
  mutate(fecha = paste0(periodo, "-", mes), fecha = zoo::as.yearmon(fecha))

egresos_anio <- arica_obstetricia |>
  group_by(anio = year(fecha)) |>
  summarise(total_egresos = sum(numero_egresos))

egresos_mes <- arica_obstetricia |>
  group_by(mes = month(fecha)) |>
  summarise(total_egresos = sum(numero_egresos))

egresos_anio |>
  mutate(diferencia = total_egresos - lag(total_egresos))

egresos_anio |>
  mutate(
    diferencia = total_egresos - lag(total_egresos),
    mayor_media = total_egresos > 3500
  )

egresos_anio |>
  mutate(
    diferencia = total_egresos - lag(total_egresos),
    mayor_media = ifelse(is.na(diferencia), NA, diferencia < 0)
  )

egresos_anio |>
  mutate(
    diferencia = total_egresos - lag(total_egresos),
    tendencia = case_when(
      diferencia > 0 ~ "Aumento",
      diferencia < 0 ~ "Disminucion",
      TRUE ~ "Sin cambio"
    )
  )


plot(arica_obstetricia$dias_estada)

arica_obstetricia |>
  mutate(promedio_dias_estada = as.numeric(promedio_dias_estada)) |>
  ggplot(aes(fecha, promedio_dias_estada)) +
  geom_line()

arica_obstetricia |>
  ggplot(aes(fecha, numero_egresos)) +
  geom_line() +
  geom_smooth() +
  geom_point()
