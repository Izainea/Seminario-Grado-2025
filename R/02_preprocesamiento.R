library(tidymodels)

crear_pipeline <- function(data, prop = 0.8, seed = 2025) {
  message("--- Etapa 02: Pipeline de Preprocesamiento ---")
  
  set.seed(seed)
  # Split idéntico al notebook (80/20)
  data_split <- initial_split(data, prop = prop, strata = income)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # DEFINICIÓN DE LA RECETA (PIPELINE)
  receta_ml <- recipe(income ~ ., data = train_data) %>%
    # 1. Imputación: Los NAs en factores se vuelven nivel "?"
    step_unknown(all_nominal_predictors(), new_level = "?") %>% 
    # 2. One Hot Encoding
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
    
    # 🌟🌟🌟 FIX: Eliminación de columnas con Varianza Cero 🌟🌟🌟
    step_zv(all_predictors()) %>% # Nuevo paso crucial
    
    # 3. Standard Scaler (Z-score)
    step_normalize(all_numeric_predictors())
  
  return(list(
    split = data_split,
    train = train_data,
    test = test_data,
    recipe = receta_ml
  ))
}