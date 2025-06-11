# Encontrando los dígitos
palabras <- c(
  "DW-40",
  "Hazla con Datos",
  "Vuelo 467",
  "Perro",
  "Nubes",
  "Depto 287"
)

grep("\\d", palabras)
grep("[0-9]", palabras, value = TRUE)
grep("\\D", palabras, value = TRUE)

# Buscar palabras que comiencen con una letra seguida de números
pacientes <- c("ID: A123", "ID: B456", "Paciente C789", "Sin ID")

grep("[A-Z]\\d{3}", pacientes, value = TRUE)

# Buscar fechas con formato dd/mm/aaaa
fechas <- c(
  "Consulta: 12/08/2023",
  "Seguimiento 5/9/22",
  "Control 2022-05-30",
  "Cx 10/27/2023"
)

grep("\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b", fechas, value = TRUE) # Incompleta
grep(
  "\\b(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/\\d{4}\\b",
  fechas,
  value = TRUE
)

# Buscar presión arterial en formato numérico/numérico
signos_vitales <- c("Presión: 120/80 mmHg", "PA: 140/90", "Frecuencia 80bpm")

grep("\\b\\d{2,3}/\\d{2,3}\\b", signos_vitales, value = TRUE)

# Buscar medicamentos que terminan en "ina"
medicamentos <- c(
  "Paracetamol",
  "Amoxicilina",
  "Ibuprofeno",
  "Metformina",
  "Insulina",
  "imipenem",
  "Clindamicina"
)

grep("ina$|imi", medicamentos, value = TRUE)


# Usando tidyverse para extraer información específica
library(tidyverse)

medicamentos |>
  str_extract("ina$")

df <- medicamentos |>
  as.data.frame() |>
  mutate(termina_en_ina = str_detect(medicamentos, "ina$"))

medicamentos |>
  as.data.frame() |>
  filter(str_detect(medicamentos, "ina$"))

# Crear un DataFrame de ejemplo
df <- data.frame(
  nombre = c(
    "Juan Pérez",
    "Ana Gómez",
    "Carlos López",
    "María Rodríguez",
    "Pedro González"
  ),
  email = c(
    "juan.perez@hotmail.com",
    "ana.gomez@gmail.com",
    "carlos.lopez@outlook.com",
    "maria.rodriguez@gmail.com",
    "pedro.gonzalez@yahoo.com"
  )
)

df |>
  filter(str_detect(email, "@gmail\\.com"))

df |>
  mutate(gmail = str_detect(email, "@gmail\\.com"))
