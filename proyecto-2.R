library(tidyverse)
library(arrow)
library(janitor)
library(scales)
library(ggtext)

# --- 1. Carga y limpieza inicial de datos ---

url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

# Cargar datos y limpiar nombres de columnas
raw_data <- arrow::read_parquet(url) |>
  clean_names()

# arrow::write_parquet(raw_data, "raw-data/at_urg_respiratorio_semanal_05062025.parquet")

# Revisar el tamaño del objeto
print(format(object.size(raw_data), units = "auto"))

# Agrupar por region_glosa y calcular métricas estadísticas
metrics <- raw_data |>
  filter(causa == "TOTAL CAUSAS SISTEMA RESPIRATORIO") |>
  group_by(servicio_salud_glosa, anio) |>
  summarise(
    across(
      .cols = c(
        num_total,
        num_menor1anio,
        num1a4anios,
        num5a14anios,
        num15a64anios,
        num65o_mas
      ),
      .fns = list(
        media = mean,
        mediana = median,
        desviacion_estandar = sd,
        minimo = min,
        maximo = max,
        q25 = ~ quantile(.x, probs = 0.25),
        q75 = ~ quantile(.x, probs = 0.75)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# --- 2. Preparación de datos para gráficos ---

# Datos de Influenza (Urgencia Hospitalaria) opción 1
influenza_hosp_data <- raw_data |>
  filter(
    tipo_urgencia == "Urgencia Hospitalaria (UEH)",
    causa == "Influenza (J09-J11)",
    !(anio == 2025 &
      semana_estadistica == max(semana_estadistica[anio == 2025]))
  )


# Función para filtrar la última semana de 2025 opción 2 (con función ad hoc)
filter_last_2025_week <- function(df) {
  df |>
    filter(
      !(anio == 2025 &
        semana_estadistica == max(semana_estadistica[anio == 2025]))
    )
}

# Datos de Influenza (Urgencia Hospitalaria)
influenza_hosp_data <- raw_data |>
  filter(
    tipo_urgencia == "Urgencia Hospitalaria (UEH)",
    causa == "Influenza (J09-J11)"
  ) |>
  filter_last_2025_week()


# Datos agregados por año y semana (para gráficos 1 y 3)
flu_hospitals_agg <- influenza_hosp_data |>
  group_by(anio, semana_estadistica) |>
  summarise(casos = sum(num_total), .groups = "drop") # ungroup() para evitar problemas posteriores

# Datos agregados por región, año y semana (para gráfico 2)
flu_hospitals_region_agg <- influenza_hosp_data |>
  group_by(region_glosa, anio, semana_estadistica) |>
  summarise(casos = sum(num_total), .groups = "drop")

# Datos de Hospitalizaciones Respiratorias en menores de 1 año
vrs_hospitals_agg <- raw_data |>
  filter(causa == "HOSPITALIZACIONES POR CAUSAS SISTEMA RESPIRATORIO") |>
  filter_last_2025_week() |>
  group_by(anio, semana_estadistica) |>
  summarise(casos = sum(num_menor1anio), .groups = "drop")

# --- 3. Gráfico simple de atencions de urgencia ---

# Filtrar los datos para incluir solo el año 2025
flu_hospitals_2025 <- flu_hospitals_agg |>
  filter(anio == 2025)

# Crear el gráfico simple para el año 2025
flu_hospitals_2025 |>
  ggplot(
    aes(semana_estadistica, casos)
  ) +
  geom_line(linewidth = 1, color = "#ff006e") +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals_2025$semana_estadistica),
      max(flu_hospitals_2025$semana_estadistica),
      by = 3
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza (Año 2025)",
    x = "Semana epidemiológica",
    y = "Casos",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 8, face = "bold")
  )

# --- 4. Comparar 2024 y 2025 ---

# Filtrar los datos para incluir solo los años 2024 y 2025
flu_hospitals_2024_2025 <- flu_hospitals_agg |>
  filter(anio %in% c(2024, 2025))

# Definir los colores para 2024 y 2025
comparison_colors <- c("2024" = "#8338ec", "2025" = "#0096c7")

# Crear el gráfico comparando 2024 y 2025
flu_hospitals_2024_2025 |>
  ggplot(
    aes(x = semana_estadistica, y = casos, color = factor(anio)) # Mapear año al color
  ) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals_2024_2025$semana_estadistica),
      max(flu_hospitals_2024_2025$semana_estadistica),
      by = 3
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = comparison_colors) + # Aplicar colores específicos
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza",
    subtitle = "Comparación de los años <span style='color:#8338ec;'>2024</span> y <span style='color:#0096c7;'>2025</span>", # Subtítulo con colores
    x = "Semana epidemiológica",
    y = "Casos",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Ocultar la leyenda ya que los años están en el subtítulo
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 15, face = "bold"), # Usar element_markdown para el subtítulo
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8, face = "bold")
  )

# --- 5. Comparar 2024 y 2025 por regiones ---

# Filtrar los datos agregados por región para incluir solo los años 2024 y 2025
flu_hospitals_region_2024_2025 <- flu_hospitals_region_agg |>
  filter(anio %in% c(2024, 2025))

# Definir los colores para 2024 y 2025
comparison_colors <- c("2024" = "#8338ec", "2025" = "#0096c7")

# Crear el gráfico comparando 2024 y 2025, separado por región
ggplot(
  data = flu_hospitals_region_2024_2025,
  aes(x = semana_estadistica, y = casos, color = factor(anio)) # Mapear año al color
) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals_region_2024_2025$semana_estadistica),
      max(flu_hospitals_region_2024_2025$semana_estadistica),
      by = 10
    )
  ) + # Ajustar breaks para facetas
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = comparison_colors) + # Aplicar colores específicos
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza, por regiones de Chile",
    subtitle = "Comparación de los años <span style='color:#8338ec;'>2024</span> y <span style='color:#0096c7;'>2025</span>", # Subtítulo con colores
    x = "Semana epidemiológica",
    y = "Casos",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() + # Tema simple
  theme(
    legend.position = "none", # Ocultar la leyenda
    plot.title = element_text(size = 20, face = "bold"), # Ajustar tamaño del título
    plot.subtitle = ggtext::element_markdown(size = 15, face = "bold"), # Usar element_markdown para el subtítulo
    axis.title = element_text(size = 9, face = "bold"), # Ajustar tamaño de ejes
    axis.text = element_text(size = 7, face = "bold"), # Ajustar tamaño de texto de ejes
    strip.text = element_text(size = 8, face = "bold") # Ajustar tamaño del texto de las facetas (nombres de región)
  ) +
  facet_wrap(~region_glosa, scales = "free_y") # Separar por región, permitiendo ejes Y libres


# --- 6. Comparar todos los años ---

# Obtener años únicos presentes en los datos
unique_years <- unique(flu_hospitals_agg$anio)

# Inicializar todos los colores a gris por defecto
colors_plot_simple <- rep("#ced4da", times = length(unique_years))

# Asignar colores específicos a los años que queremos resaltar (2024 y 2025)
# Buscamos la posición de 2024 y 2025 en la lista de años únicos
pos_2024 <- which(unique_years == 2024)
pos_2025 <- which(unique_years == 2025)

# Asignamos los colores a esas posiciones
colors_plot_simple[pos_2024] <- "#8338ec" # Color para 2024

colors_plot_simple[pos_2025] <- "#0096c7" # Color para 2025

# Nombrar los colores con los años para que ggplot los use correctamente
names(colors_plot_simple) <- unique_years

highlight_points_plot <- bind_rows(
  flu_hospitals_agg |>
    filter(anio == 2025) |>
    slice(which.max(semana_estadistica)), # Último punto de 2025
  flu_hospitals_agg |>
    filter(anio == 2024) |>
    slice(which.max(casos)) # Punto de mayor caso en 2024
)

# Crear el gráfico base y añadir las capas de resaltado directamente
plot_simple <- ggplot(
  data = flu_hospitals_agg,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line(linewidth = 0.8) +
  # Añadir los puntos de resaltado
  geom_point(
    data = highlight_points_plot, # Usar los puntos calculados
    aes(x = semana_estadistica, y = casos, color = factor(anio)),
    size = 3
  ) +
  # Añadir las etiquetas de texto para los puntos resaltados
  geom_text(
    data = highlight_points_plot, # Usar los puntos calculados
    aes(
      x = semana_estadistica,
      y = casos,
      label = casos,
      color = factor(anio)
    ),
    vjust = 0, # Ajuste vertical de la etiqueta
    hjust = -0.3, # Ajuste horizontal de la etiqueta
    size = 4
  ) +
  # Aplicar la escala de color que definimos manualmente
  scale_color_manual(values = colors_plot_simple) +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals_agg$semana_estadistica),
      max(flu_hospitals_agg$semana_estadistica),
      by = 5
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza",
    subtitle = "Se han resaltado los años <span style='color:#8338ec;'>2024</span> y <span style='color:#0096c7;'>2025</span>",
    x = "Semana epidemiológica",
    y = "Casos",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Ocultar la leyenda
    plot.title = element_text(size = 20, face = "bold"), # Ajustar tamaño del título
    plot.subtitle = ggtext::element_markdown(size = 15, face = "bold"), # Usar element_markdown para el subtítulo
    axis.title = element_text(size = 9, face = "bold"), # Ajustar tamaño de ejes
    axis.text = element_text(size = 7, face = "bold") # Ajustar tamaño de texto de ejes
  )

plot_simple
