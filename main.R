# main.R
source("R/01_carga_limpieza.R")
source("R/02_preprocesamiento.R")
source("R/03_model.R")

# 1. Carga (usando tus archivos específicos)
datos_limpios <- cargar_y_limpiar_datos(
  path_data = "data/adult.data", 
  path_test = "data/adult.test"
)



# 2. Pipeline y Split
datos_prep <- crear_pipeline(datos_limpios)

# 3. Grid Search, Entrenamiento Final y Guardado
resultados <- ejecutar_grid_search_y_guardar(datos_prep)