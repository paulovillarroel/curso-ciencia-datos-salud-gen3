library(ellmer)
library(tidyverse)

# usethis::edit_r_environ("project")

models_google_gemini(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = Sys.getenv("GOOGLE_API_KEY")
)

chat <- chat_google_gemini(
  model = "gemini-2.5-flash-preview-05-20",
  system_prompt = "You are a helpful assistant. Answer the user's questions concisely and accurately.",
  api_key = Sys.getenv("GOOGLE_API_KEY")
)

chat$chat("Hola!! ¿Cómo estás?")


# Basics

chat$chat_structured(
  "My name is Susan and I'm 13 years old",
  type = type_object(
    age = type_number(),
    name = type_string()
  )
)

# Article summarisation

text <- readLines("raw-data/historia_clinica.txt")

type_summary <- type_object(
  "Resumen del caso clínico.",
  physician = type_string("Nombre del médico tratante."),
  topics = type_array(
    "Etiquetas clínicas específicas relacionadas al caso. Solo 3 etiquetas.",
    type_string()
  ),
  summary = type_string("Resumen breve del caso clínico en uno o dos párrafos.")
)

data <- chat$chat_structured(text, type = type_summary)

cat(data$physician)

str(data)

data_df <- tibble(
  physician = data$physician,
  topics = paste(data$topics, collapse = ", "),
  summary = data$summary
)


# System prompt for a specific document

system_prompt <- paste(
  "Tu eres un asistente que respondes preguntas en base a esta política:",
  "<policy>",
  readLines("politica_calidad.md"),
  "</policiy>",
  "* Usa un lenguaje sencillo y claro",
  collapse = "\n"
)

client <- chat_google_gemini(
  model = "gemini-2.5-flash-preview-05-20",
  system_prompt = system_prompt,
  api_key = Sys.getenv("GOOGLE_API_KEY")
)

client$chat("¿Qué debo hacer si tengo una queja con un médico?")
client$chat("¿Cómo puedo acceder a mi historial médico?")


# Named entity recognition NER

type_named_entity <- type_object(
  name = type_string("The extracted entity name."),
  type = type_enum(
    "The entity type",
    c("person", "location", "diagnosis", "symtomps", "treatment")
  ),
  context = type_string("The context in which the entity appears in the text.")
)

type_named_entities <- type_array(items = type_named_entity)

ner <- chat$chat_structured(text, type = type_named_entities)


# Text classification

type_classification <- type_array(
  "Array de resultados de clasificación clínica. Los puntajes deben sumar 1.",
  type_object(
    name = type_enum(
      "Nombre de la especialidad médica",
      values = c(
        "Medicina Interna",
        "Geriatría",
        "Pediatría",
        "Cirugía General",
        "Gastroenterología",
        "Urgencias",
        "Cardiología",
        "Neurología",
        "Oncología",
        "Psiquiatría",
        "Ginecología",
        "Otro"
      )
    ),
    score = type_number(
      "Puntaje de clasificación para la especialidad, entre 0.0 y 1.0."
    )
  )
)

data <- chat$chat_structured(text, type = type_classification)


# Others examples

text <- c("mastectomia radical tumorectomia con vaciamiento ganglionar total")

chat$chat_structured(
  text,
  type = type_object(
    cancer = type_enum(
      "Clasify if related to cancer",
      values = c("Related", "Not related")
    )
  )
)

# Parallel processing with structured data

prompts <- list(
  "mastectomia radical tumorectomia con vaciamiento ganglionar total",
  "Hipertensión arterial descompensada",
  "Cáncer de pulmón en etapa avanzada",
  "Miastenia gravis de larga data"
)

results <- parallel_chat_structured(
  chat,
  prompts,
  type = type_object(
    cancer = type_enum(
      "Clasify if related to cancer",
      values = c("Related", "Not related")
    )
  )
)

results_df <- data.frame(prompt = unlist(prompts), result = results)
