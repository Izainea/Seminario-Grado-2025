library(tidymodels)
library(ranger)

ejecutar_grid_search_y_guardar <- function(objetos_prepro, seed = 2025) {
  message("--- Etapa 03: Grid Search y Guardado ---")
  
  # 1. Definición del modelo con Hiperparámetros a afinar (tune)
  rf_spec <- rand_forest(
    trees = tune(),      # n_estimators
    min_n = tune()       # equivalente aprox a max_depth/min_samples
  ) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
  
  # 2. Workflow
  rf_workflow <- workflow() %>%
    add_recipe(objetos_prepro$recipe) %>%
    add_model(rf_spec)
  
  # 3. Definir la Grilla (Grid)
  # Replicando valores similares al notebook:
  # n_estimators: 50, 100, 150
  # min_n: valores que controlan la profundidad
  mi_grilla <- expand.grid(
    trees = c(50, 100, 150), 
    min_n = c(10, 20, 30)
  )
  
  # 4. Cross-Validation (CV=5 como en el notebook)
  set.seed(seed)
  folds <- vfold_cv(objetos_prepro$train, v = 5)
  
  message("Ejecutando Grid Search (esto puede tardar)...")
  rf_grid_results <- tune_grid(
    rf_workflow,
    resamples = folds,
    grid = mi_grilla,
    metrics = metric_set(accuracy, roc_auc)
  )
  
  # 5. Seleccionar el mejor modelo
  best_params <- select_best(rf_grid_results, metric = "accuracy")
  message("Mejores parámetros encontrados:")
  print(best_params)
  
  # 6. Finalizar el workflow con los mejores parámetros
  final_workflow <- finalize_workflow(rf_workflow, best_params)
  
  # 7. Entrenar modelo final en todo el train set y evaluar en test
  final_fit <- last_fit(final_workflow, objetos_prepro$split)
  
  # 8. Guardar el modelo (el objeto workflow entrenado)
  # Extraemos el modelo entrenado del contenedor last_fit
  modelo_para_guardar <- extract_workflow(final_fit)
  
  saveRDS(modelo_para_guardar, "modelo/modelo_random_forest_optimizado.rds")
  message("Modelo guardado como 'modelo_random_forest_optimizado.rds'")
  
  # Retornar métricas
  metrics <- collect_metrics(final_fit)
  print(metrics)
  
  return(final_fit)
}