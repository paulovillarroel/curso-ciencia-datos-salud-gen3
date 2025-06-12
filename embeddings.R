library(ollamar)
library(tidyverse)

test_connection()

list_models()

resp <- generate("gemma3:4b", "Cuéntame un chiste sobre perros")
resp_process(resp, "text")
resp_process(resp, "df")

generate("gemma3:4b", "Cuéntame un chiste sobre perros", output = "text", )

# embeddings
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

embed1 <- embed("nomic-embed-text:latest", "el perro negro")
embed2 <- embed("nomic-embed-text:latest", "perro negro")

cosine_similarity(embed1, embed2)

embed3 <- embed("nomic-embed-text:latest", "el perro negro")
embed4 <- embed("nomic-embed-text:latest", "la casa bonita")

cosine_similarity(embed3, embed4)

embed5 <- embed("nomic-embed-text:latest", "el perro negro")
embed6 <- embed("nomic-embed-text:latest", "el canino oscuro")

cosine_similarity(embed5, embed6)

embed7 <- embed("nomic-embed-text:latest", "carcinoma hepático")
embed8 <- embed("nomic-embed-text:latest", "tumor maligno en hígado")

cosine_similarity(embed7, embed8)

embed9 <- embed(
  "nomic-embed-text:latest",
  "Pedro se encontró con su amigo Juan en el parque"
)
embed10 <- embed(
  "nomic-embed-text:latest",
  "Juan y Pedro se encontraron en el parque"
)

cosine_similarity(embed9, embed10)

emb_base <- embed("nomic-embed-text:latest", "control de carcinoma hepático")
emb_errada <- embed("nomic-embed-text:latest", "cntrol de karsinoma epatiko")

cosine_similarity(emb_base, emb_errada)



# Clasificación con embeddings
diagnosticos <- tibble(
  diagnos = c(
    # CÁNCER
    "carcinoma hepático",
    "tumor pulmonar",
    "adenocarcinoma gástrico",
    "neoplasia maligna de tiroides",
    "linfoma no hodgkin",
    "melanoma maligno en piel",
    "tumor cerebral maligno",
    "cáncer de próstata",
    "carcinoma de mama",
    "metástasis ósea",
    "tumor maligno de colon",
    "glioblastoma multiforme",
    "leucemia mieloide aguda",
    "sarcoma de tejidos blandos",
    "carcinoma escamocelular",
    "tumor de ovario",
    "carcinoma de células renales",
    "cáncer de vejiga",
    "adenoma pituitario maligno",
    "neoplasia maligna pancreática",
    "linfoma de Hodgkin",
    "tumor neuroendocrino",
    "cáncer de esófago",
    "melanoma ocular",

    # NO CÁNCER
    "hernia inguinal",
    "cefalea tensional",
    "fractura de fémur",
    "dolor abdominal",
    "lumbalgia mecánica",
    "apendicitis aguda",
    "asma bronquial",
    "gastroenteritis viral",
    "hipertensión arterial",
    "diabetes mellitus tipo 2",
    "enfermedad pulmonar obstructiva crónica",
    "artrosis de rodilla",
    "bronquitis crónica",
    "epicondilitis lateral",
    "dermatitis atópica",
    "sinusitis maxilar",
    "hernia de disco lumbar",
    "inflamación de tendones",
    "insuficiencia renal crónica",
    "colitis ulcerosa",
    "migraña clásica",
    "psoriasis",
    "artritis reumatoide",
    "fibromialgia"
  ),
  label = c(
    rep("cancer", 24),
    rep("no_cancer", 24)
  )
)


seeds_cancer <- c(
  "neoplasia",
  "oncológico",
  "metástasis",
  "cáncer",
  "tumor maligno",
  "adenocarcinoma",
  "linfoma",
  "leucemia",
  "melanoma",
  "sarcoma",
  "carcinoma",
  "quimioterapia",
  "radioterapia",
  "resección tumoral",
  "tumor metastásico",
  "biopsia oncológica",
  "infiltración maligna",
  "enfermedad oncológica",
  "progresión tumoral",
  "neoplasia maligna"
)

seeds_no_cancer <- c(
  "hernia inguinal",
  "cefalea tensional",
  "fractura de cadera",
  "asma bronquial",
  "diabetes mellitus",
  "hipertensión arterial",
  "lumbalgia mecánica",
  "resfrío común",
  "gastroenteritis aguda",
  "dolor lumbar",
  "escoliosis",
  "infección urinaria",
  "artritis reumatoide",
  "colelitiasis",
  "apendicitis aguda",
  "neumonía adquirida en la comunidad",
  "pancreatitis",
  "hemorragia digestiva alta",
  "fiebre de origen infeccioso",
  "epicondilitis lateral"
)


embeddings_cancer <- map(seeds_cancer, ~ embed("nomic-embed-text:latest", .x))

embeddings_no_cancer <- map(
  seeds_no_cancer,
  ~ embed("nomic-embed-text:latest", .x)
)

diagnosticos_emb <- diagnosticos |>
  mutate(embedding = map(diagnos, ~ embed("nomic-embed-text:latest", .x)))

# Calcular similitud promedio
similitud_promedio <- function(embedding_obj, embeddings_grupo) {
  vec <- as.numeric(embedding_obj)
  sims <- map_dbl(embeddings_grupo, function(e) {
    v <- as.numeric(e)
    sum(vec * v) / (sqrt(sum(vec^2)) * sqrt(sum(v^2)))
  })
  mean(sims)
}

# Clasificación con umbral

diagnosticos_clasificados <- diagnosticos_emb |>
  rowwise() |>
  mutate(
    sim_cancer = similitud_promedio(embedding, embeddings_cancer),
    sim_no_cancer = similitud_promedio(embedding, embeddings_no_cancer)
  ) |>
  ungroup()

# Visualización de similitudes

diagnosticos_clasificados |>
  select(diagnos, label, sim_cancer, sim_no_cancer) |>
  pivot_longer(
    cols = starts_with("sim_"),
    names_to = "grupo",
    values_to = "similitud"
  ) |>
  ggplot(aes(x = similitud, fill = label)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~grupo)

# Modelo de clasificación binaria

df_modelo <- diagnosticos_clasificados |>
  mutate(label_bin = ifelse(label == "cancer", 1, 0))

modelo <- glm(
  label_bin ~ sim_cancer + sim_no_cancer,
  data = df_modelo,
  family = binomial()
)

summary(modelo)

df_modelo |>
  ggplot(aes(x = sim_no_cancer, y = sim_cancer, color = label)) +
  geom_point(size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal()

diag_pred <- diagnosticos_clasificados |>
  mutate(
    prob = predict(modelo, type = "response"),
    pred = case_when(
      prob > 0.6 ~ "cancer",
      prob < 0.4 ~ "no_cancer",
      TRUE ~ "indeterminado"
    )
  )

# Matriz de confusión
library(caret)

confusionMatrix(factor(diag_pred$label), factor(diag_pred$pred))


# Nuevos diagnósticos para clasificar (usando el mismo modelo entrenado)
nuevos_diagnosticos <- tibble(
  diagnos = c(
    "inflamación de garganta",
    "tumor de páncreas",
    "fractura de tobillo",
    "carcinoma colorrectal",
    "tendinitis",
    "úlcera gástrica",
    "neumonía viral",
    "neumonía bacteriana",
    "artritis psoriásica",
    "cáncer de pulmón",
    "lesión focal hepática",
    "nevo melanocítico",
    "lesión benigna de piel",
    "tumor maligno de tiroides",
    "perforación intestinal"
  )
)

model_name <- "nomic-embed-text:latest"

# 1. Generar embeddings para los nuevos diagnósticos
nuevos_diagnosticos_emb <- nuevos_diagnosticos |>
  mutate(embedding = map(diagnos, ~ embed(model_name, .x)))

# 2. Calcular similitudes promedio con las semillas
nuevos_diagnosticos_clasificados <- nuevos_diagnosticos_emb |>
  rowwise() |>
  mutate(
    sim_cancer = similitud_promedio(embedding, embeddings_cancer),
    sim_no_cancer = similitud_promedio(embedding, embeddings_no_cancer)
  ) |>
  ungroup()

# 3. Predecir las probabilidades usando el modelo entrenado
predicciones_prob <- predict(
  modelo,
  newdata = nuevos_diagnosticos_clasificados,
  type = "response"
)

nuevos_diagnosticos_con_prediccion <- nuevos_diagnosticos_clasificados |>
  mutate(prob = predicciones_prob)

# 4. Clasificar los nuevos diagnósticos
diagnosticos_final_clasificados <- nuevos_diagnosticos_con_prediccion |>
  mutate(
    pred = case_when(
      prob > 0.6 ~ "cancer",
      prob < 0.4 ~ "no_cancer",
      TRUE ~ "indeterminado"
    )
  )

# Visualización de resultados
diagnosticos_final_clasificados |>
  select(diagnos, sim_cancer, sim_no_cancer, prob, pred) |>
  ggplot(aes(x = sim_no_cancer, y = sim_cancer, color = pred)) +
  geom_point(size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Clasificación de nuevos diagnósticos",
    x = "Similitud con No Cáncer",
    y = "Similitud con Cáncer"
  )
