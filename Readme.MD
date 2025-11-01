# Clasificación de Ingresos con Random Forest en R

Este proyecto implementa un modelo de **clasificación supervisada** utilizando el algoritmo **Random Forest** para predecir si el ingreso anual de una persona excede los **$50,000 USD** con base en los datos del censo de 1994. El objetivo es explorar el desempeño de modelos de ensamble en un problema clásico de aprendizaje automático.

---

## 📊 Descripción del Dataset

**Fuente:** UCI Machine Learning Repository — [Adult Dataset](https://archive.ics.uci.edu/ml/datasets/adult)

**Contexto:**  
El dataset, también conocido como *Census Income Dataset*, fue extraído por Barry Becker del censo de 1994. Contiene información demográfica y laboral de individuos mayores de 16 años con condiciones de empleo y ganancias válidas.

**Tarea:**  
Predecir si el ingreso de una persona es **`>50K`** o **`<=50K`** al año.

| Característica       | Tipo         | Descripción                                    |
|----------------------|--------------|------------------------------------------------|
| `age`               | Entero       | Edad del individuo                             |
| `workclass`         | Categórica   | Tipo de empleo                                 |
| `education`         | Categórica   | Nivel educativo                                |
| `education-num`     | Entero       | Años de educación                              |
| `marital-status`    | Categórica   | Estado civil                                   |
| `occupation`        | Categórica   | Ocupación                                      |
| `relationship`      | Categórica   | Relación familiar                              |
| `race`              | Categórica   | Raza                                           |
| `sex`               | Binaria      | Sexo                                           |
| `capital-gain`      | Entero       | Ganancia de capital                            |
| `capital-loss`      | Entero       | Pérdida de capital                             |
| `hours-per-week`    | Entero       | Horas trabajadas por semana                    |
| `native-country`    | Categórica   | País de origen                                 |
| `income` *(target)* | Binaria      | Clase objetivo (`<=50K`, `>50K`)               |

**Número de instancias:** 48,842  
**Número de atributos:** 14 (sin incluir la variable objetivo)  
**Tipo de problema:** Clasificación binaria  

---


## ⚙️ Requisitos

### Dependencias principales

```R
install.packages(c("tidyverse", "randomForest", "caret", "pROC"))
````

### Librerías utilizadas

```R
library(tidyverse)
library(randomForest)
library(caret)
library(pROC)
```

---

## 🚀 Ejecución del Proyecto

1. Clonar este repositorio:

```bash
git clone https://github.com/tu_usuario/adult-income-randomforest.git
cd adult-income-randomforest
```

2. Ejecutar el script principal en R:

```R
source("modelo_random_forest.R")
```

3. Visualizar los resultados:

   * Reporte de métricas en consola.
   * Importancia de variables graficada.
   * Curva ROC para validación del modelo.

---

## 📈 Resultados Esperados

* **Accuracy promedio:** entre **84% y 86%**
* **Variables más relevantes:** `education-num`, `marital-status`, `hours-per-week`, `capital-gain`
* El modelo demuestra la capacidad de los ensambles para manejar relaciones no lineales y variables mixtas.

---

## 📚 Aprendizajes Clave

* La importancia del **preprocesamiento de variables categóricas** en R.
* Cómo interpretar la **importancia de variables** en Random Forest.
* Evaluación de desempeño mediante **validación cruzada** y métricas múltiples.

---

## 🧑‍💻 Autor

**Carlos Isaac Zainea Maya**
Científico de Datos | Universidad Santo Tomás
📧 [cizaineam@gmail.com](mailto:cizaineam@gmail.com)
🐙 [@izainea](https://github.com/izainea)

---

## 📜 Licencia

Este proyecto se distribuye bajo la licencia **MIT**, lo que permite su uso, modificación y distribución con atribución al autor original.
