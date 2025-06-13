library(ollamar)
library(tidyverse)
library(caret)
library(pROC)

# Función mejorada para similitud coseno con validación
cosine_similarity <- function(a, b) {
  vec_a <- as.numeric(a)
  vec_b <- as.numeric(b)

  if (length(vec_a) != length(vec_b)) {
    stop("Los vectores deben tener la misma longitud")
  }

  norm_a <- sqrt(sum(vec_a^2))
  norm_b <- sqrt(sum(vec_b^2))

  # Evitar división por cero
  if (norm_a == 0 || norm_b == 0) {
    return(0)
  }

  return(sum(vec_a * vec_b) / (norm_a * norm_b))
}

# Función para generar embeddings con manejo de errores
safe_embed <- function(model, text, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch(
      {
        return(embed(model, text))
      },
      error = function(e) {
        if (i == max_retries) {
          stop(paste(
            "Error al generar embedding después de",
            max_retries,
            "intentos:",
            e$message
          ))
        }
        Sys.sleep(1) # Esperar antes de reintentar
      }
    )
  }
}

# Dataset balanceado con más ejemplos
diagnosticos <- tibble(
  diagnos = c(
    # CÁNCER (30 ejemplos)
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
    "carcinoma basocelular",
    "tumor de testículo",
    "cáncer de cuello uterino",
    "mieloma múltiple",
    "tumor de hueso",
    "carcinoma de lengua",

    # NO CÁNCER (30 ejemplos)
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
    "fibromialgia",
    "neumonía bacteriana",
    "infección urinaria",
    "gastritis crónica",
    "escoliosis",
    "tendinitis rotuliana",
    "síndrome del túnel carpiano"
  ),
  label = c(rep("cancer", 30), rep("no_cancer", 30))
)

# Semillas más específicas y balanceadas
seeds_cancer <- c(
  "mieloma múltiple",
  "carcinoma",
  "linfoma",
  "sarcoma",
  "metástasis hepática",
  "cáncer de mama",
  "cáncer de próstata",
  "cáncer pulmonar",
  "cáncer gástrico",
  "cáncer colorrectal",
  "quimioterapia",
  "radioterapia",
  "tumor maligno",
  "lesión maligna",
  "neoplasia maligna",
  "adenocarcinoma",
  "carcinoma escamoso",
  "carcinomatosis peritoneal",
  "tumor cerebral maligno",
  "biopsia positiva para cáncer",
  "biopsia con células atípicas",
  "masa sospechosa",
  "seguimiento oncológico",
  "histología maligna",
  "control oncológico",
  "recidiva tumoral",
  "recaída oncológica",
  "tumor infiltrante",
  "ganglios comprometidos",
  "lesión sospechosa de cáncer",
  "enfermedad metastásica",
  "tratamiento paliativo oncológico",
  "cáncer avanzado",
  "neoplasia maligna metastásica",
  "tumor agresivo",
  "marcadores tumorales elevados",
  "detección precoz de cáncer",
  "tumor no operable",
  "indicador de malignidad"
)


seeds_no_cancer <- c(
  "hernia inguinal",
  "lipoma subcutáneo",
  "tumor benigno",
  "quiste ovárico",
  "mioma uterino",
  "adenoma hepático",
  "neoplasia benigna",
  "nódulo tiroideo benigno",
  "quiste sin malignidad",
  "masa benigna",
  "hiperplasia benigna de próstata",
  "lesión sin características malignas",
  "biopsia negativa para cáncer",
  "lesión benigna",
  "estudio imagenológico normal",
  "nódulo sin cambios sospechosos",
  "masa estable sin crecimiento",
  "hallazgos benignos en ecografía",
  "sin evidencia de malignidad",
  "hiperplasia endometrial sin atipias",
  "adenoma tubular",
  "seguimiento sin progresión",
  "tumor benigno encapsulado",
  "quiste sebáceo",
  "fibroadenoma",
  "angioma hepático",
  "quiste renal simple",
  "papiloma benigno",
  "ganglio reactivo",
  "hallazgo incidental benigno",
  "masa sin requerimiento oncológico",
  "biopsia compatible con lesión benigna",
  "control sin indicios de malignidad"
)


# Configuración del modelo de embedding
MODEL_NAME <- "nomic-embed-text:latest"

# Generar embeddings con manejo de errores
embeddings_cancer <- map(seeds_cancer, ~ safe_embed(MODEL_NAME, .x))

embeddings_no_cancer <- map(seeds_no_cancer, ~ safe_embed(MODEL_NAME, .x))

diagnosticos_emb <- diagnosticos |>
  mutate(embedding = map(diagnos, ~ safe_embed(MODEL_NAME, .x)))

# Función mejorada para calcular similitud promedio
similitud_mediana <- function(embedding_obj, embeddings_grupo) {
  vec <- as.numeric(embedding_obj)

  sims <- map_dbl(embeddings_grupo, function(e) {
    cosine_similarity(vec, as.numeric(e))
  })

  # Usar mediana en lugar de media para mayor robustez
  median(sims, na.rm = TRUE)
}

# Calcular similitudes
diagnosticos_clasificados <- diagnosticos_emb |>
  rowwise() |>
  mutate(
    sim_cancer = similitud_mediana(embedding, embeddings_cancer),
    sim_no_cancer = similitud_mediana(embedding, embeddings_no_cancer),
    # Diferencia de similitudes como feature adicional
    diff_sim = sim_cancer - sim_no_cancer
  ) |>
  ungroup()

# Análisis exploratorio mejorado
# Gráfico de densidades
p1 <- diagnosticos_clasificados |>
  select(diagnos, label, sim_cancer, sim_no_cancer) |>
  pivot_longer(
    cols = starts_with("sim_"),
    names_to = "grupo",
    values_to = "similitud"
  ) |>
  mutate(grupo = str_replace(grupo, "sim_", "Similitud ")) |>
  ggplot(aes(x = similitud, fill = label)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~grupo) +
  theme_minimal() +
  labs(
    title = "Distribución de similitudes por grupo",
    x = "Similitud",
    y = "Densidad",
    fill = "Etiqueta real"
  ) +
  scale_fill_manual(values = c("cancer" = "#e74c3c", "no_cancer" = "#3498db"))

print(p1)

# Scatter plot
p2 <- diagnosticos_clasificados |>
  ggplot(aes(x = sim_no_cancer, y = sim_cancer, color = label)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  labs(
    title = "Espacio de similitudes",
    x = "Similitud con No Cáncer",
    y = "Similitud con Cáncer",
    color = "Etiqueta real"
  ) +
  scale_color_manual(values = c("cancer" = "#e74c3c", "no_cancer" = "#3498db"))

print(p2)

# Modelo de clasificación con más features
df_modelo <- diagnosticos_clasificados |>
  mutate(label_bin = ifelse(label == "cancer", 1, 0))

# Modelo logístico con regularización
modelo <- glm(
  label_bin ~ sim_cancer + sim_no_cancer + diff_sim,
  data = df_modelo,
  family = binomial()
)

print(summary(modelo))

# Evaluación del modelo
roc_obj <- roc(df_modelo$label_bin, predict(modelo, type = "response"))
auc(roc_obj)


# Validación cruzada
set.seed(123)
folds <- createFolds(df_modelo$label_bin, k = 5)

cv_results <- map_dfr(folds, function(fold_idx) {
  train_data <- df_modelo[-fold_idx, ]
  test_data <- df_modelo[fold_idx, ]

  temp_model <- glm(
    label_bin ~ sim_cancer + sim_no_cancer + diff_sim,
    data = train_data,
    family = binomial()
  )

  predictions <- predict(temp_model, test_data, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)

  tibble(
    accuracy = mean(predicted_classes == test_data$label_bin),
    fold = length(predictions)
  )
})

# Resultados de validación cruzada
cat("Accuracy promedio:", round(mean(cv_results$accuracy), 4))
cat("Desviación estándar:", round(sd(cv_results$accuracy), 4))

# Predicciones con umbrales optimizados
diag_pred <- diagnosticos_clasificados |>
  mutate(
    prob = predict(modelo, type = "response"),
    pred_conservative = case_when(
      prob > 0.7 ~ "cancer",
      prob < 0.3 ~ "no_cancer",
      TRUE ~ "indeterminado"
    ),
    pred_balanced = case_when(
      prob > 0.5 ~ "cancer",
      TRUE ~ "no_cancer"
    )
  )

# Matriz de confusión para clasificación balanceada
cm_balanced <- confusionMatrix(
  factor(diag_pred$pred_balanced, levels = c("cancer", "no_cancer")),
  factor(diag_pred$label, levels = c("cancer", "no_cancer"))
)
print(cm_balanced)

# Función para clasificar nuevos diagnósticos
clasificar_nuevos_diagnosticos <- function(nuevos_diagnosticos_vec) {
  cat(
    "Clasificando",
    length(nuevos_diagnosticos_vec),
    "nuevos diagnósticos...\n"
  )

  nuevos_diagnosticos <- tibble(diagnos = nuevos_diagnosticos_vec)

  # Generar embeddings
  nuevos_diagnosticos_emb <- nuevos_diagnosticos |>
    mutate(embedding = map(diagnos, ~ safe_embed(MODEL_NAME, .x)))

  # Calcular similitudes
  nuevos_diagnosticos_clasificados <- nuevos_diagnosticos_emb |>
    rowwise() |>
    mutate(
      sim_cancer = similitud_mediana(embedding, embeddings_cancer),
      sim_no_cancer = similitud_mediana(embedding, embeddings_no_cancer),
      diff_sim = sim_cancer - sim_no_cancer
    ) |>
    ungroup()

  # Predecir
  predicciones_prob <- predict(
    modelo,
    newdata = nuevos_diagnosticos_clasificados,
    type = "response"
  )

  # Clasificar con diferentes umbrales
  resultado <- nuevos_diagnosticos_clasificados |>
    mutate(
      prob_cancer = predicciones_prob,
      pred_conservative = case_when(
        prob_cancer > 0.7 ~ "cancer",
        prob_cancer < 0.3 ~ "no_cancer",
        TRUE ~ "indeterminado"
      ),
      pred_balanced = case_when(
        prob_cancer > 0.5 ~ "cancer",
        TRUE ~ "no_cancer"
      ),
      confianza = case_when(
        prob_cancer > 0.8 | prob_cancer < 0.2 ~ "alta",
        prob_cancer > 0.7 | prob_cancer < 0.3 ~ "media",
        TRUE ~ "baja"
      )
    )

  return(resultado)
}


# Ejemplo de uso con nuevos diagnósticos
nuevos_casos <- c(
  "inflamación de garganta",
  "tumor de páncreas",
  "fractura de tobillo",
  "carcinoma colorrectal",
  "tendinitis",
  "úlcera gástrica",
  "neumonía viral",
  "cáncer de pulmón",
  "lesión focal hepática",
  "tumor maligno de tiroides",
  "nevo melanocítico"
)

resultados_nuevos <- clasificar_nuevos_diagnosticos(nuevos_casos)

# Mostrar resultados
print(
  resultados_nuevos |>
    select(diagnos, prob_cancer, pred_conservative, confianza) |>
    arrange(desc(prob_cancer))
)

# Visualización final
p3 <- resultados_nuevos |>
  ggplot(aes(x = sim_no_cancer, y = sim_cancer, color = pred_conservative)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  labs(
    title = "Clasificación de nuevos diagnósticos",
    x = "Similitud con No Cáncer",
    y = "Similitud con Cáncer",
    color = "Predicción"
  ) +
  scale_color_manual(
    values = c(
      "cancer" = "#e74c3c",
      "no_cancer" = "#3498db",
      "indeterminado" = "#f39c12"
    )
  )

print(p3)
