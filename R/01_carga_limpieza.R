library(tidyverse)
library(janitor)

cargar_y_limpiar_datos <- function(path_data = "data/adult.data", path_test = "data/adult.test") {
  message("--- Etapa 01: Carga y Limpieza (Archivos Raw) ---")
  
  # Nombres de columnas según la documentación de UCI (adult.names)
  col_names <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                 "marital_status", "occupation", "relationship", "race", "sex", 
                 "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
  
  # 1. Cargar Training Data
  df_train <- read_csv(path_data, col_names = col_names, na = c("?", "", "NA"), show_col_types = FALSE)
  
  # 2. Cargar Test Data (Suele tener una línea rara al principio y un punto al final de income)
  # skip=1 es común en adult.test
  df_test_raw <- read_csv(path_test, col_names = col_names, na = c("?", "", "NA"), skip = 1, show_col_types = FALSE)
  
  # Limpiar el punto extra que suele traer el target en el archivo de test ("<=50K." vs "<=50K")
  df_test_raw <- df_test_raw %>% 
    mutate(income = str_remove(income, "\\."))
  
  # 3. Unir todo para replicar el split aleatorio del notebook original
  # (El notebook unía todo y luego hacía train_test_split con random_state=2025)
  df_full <- bind_rows(df_train, df_test_raw)
  
  # 4. Limpieza idéntica al notebook
  df_clean <- df_full %>%
    clean_names() %>%
    select(-fnlwgt) %>% # Drop fnlwgt
    # Mapeo exacto: <=50K -> 0, >50K -> 1
    mutate(income = case_when(
      income == "<=50K" ~ "clase_0", # tidymodels prefiere letras al inicio
      income == ">50K"  ~ "clase_1"
    )) %>%
    filter(!is.na(income)) %>% # Seguridad
    mutate(income = factor(income, levels = c("clase_0", "clase_1"))) %>%
    mutate(across(where(is.character), as.factor)) # Strings a factores
  
  message("Datos cargados y unidos. Total filas: ", nrow(df_clean))
  return(df_clean)
}