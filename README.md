# Food Security and Global Health Region Analysis

Este repositorio contiene los datos y el código en R necesarios para analizar la relación entre las publicaciones sobre seguridad alimentaria y diversos indicadores de salud global, abarcando diferentes regiones de la OMS y grupos de ingresos.

## 📂 Estructura del Repositorio

El proyecto está dividido en dos carpetas principales:

### 1. `data/` (Datos)
Contiene los archivos de Excel con la información cruda y los datos procesados en cada paso del análisis:

* **`data.xlsx`**: Es la base de datos cruda original que contiene a nivel de publicación/país las variables bibliométricas y todos los indicadores globales (salud, economía, agricultura, etc.).
* **`step_1_region_global_health.xlsx`**: Resultados agregados del **Paso 1**. Contiene los resultados de los modelos de regresión simples evaluando las asociaciones entre publicaciones e indicadores en las distintas regiones de la OMS.
* **`step_2_region_global_health.xlsx`**: Resultados del **Paso 2**. Contiene los resultados de los modelos de efectos mixtos (Mixed-Effects Models), donde la "región" se utiliza como un efecto aleatorio para capturar la varianza regional.
* **`step_3_region_global_health.xlsx`**: Resultados del **Paso 3**. Incluye análisis de interacciones y moderaciones (slopes simples, percentiles 25 y 75) para entender cómo el efecto de la seguridad alimentaria cambia según ciertas covariables.

### 2. `src/` (Código Fuente)
Contiene los scripts de R que ejecutan toda la limpieza, análisis estadístico y visualización:

* **`Main Analysis.R`**: Es el motor principal del análisis. 
  * **Depuración**: Limpia la base de datos cruda (`data.xlsx`), transformando textos a números, eliminando comas, convirtiendo porcentajes a proporciones (0-1), y manejando símbolos de moneda ($).
  * **Agregación**: Agrupa los datos a nivel de país-año y calcula promedios ponderados por población (`Population in year`), separando el análisis por "Grupos de Ingresos" y por "Regiones de la OMS".
  * **Pre-análisis**: Categoriza decenas de indicadores en clústeres temáticos (ej. *Health System & Financing*, *Agricultural inputs*, *Dietary patterns*) y define si cada indicador actúa como variable dependiente o independiente.
  * **Modelamiento Estadístico**: Implementa un sofisticado sistema de selección de modelos. Detecta automáticamente si una variable requiere un modelo de conteo (Poisson, Binomial Negativo), proporciones (Quasi-binomial, GLMM Beta) o continuo (Linear Gaussian, LMM), y ejecuta las regresiones de los pasos 1, 2 y 3.

* **`R Plots.R`**: Es el script de visualización y post-procesamiento. 
  * Toma los resultados procesados (`step_1`, `step_2`, `step_3`) y aplica una **escala matemática** estandarizada (ver sección de Metodología abajo).
  * Construye el **Figura 1** (Heatmap de calor mostrando la dirección, magnitud y significancia de las asociaciones regionales).
  * Construye el **Figura 2** (Forest Plot mostrando los coeficientes, IRR y OR con sus respectivos intervalos de confianza bajo modelos de efectos mixtos).
  * Exporta los gráficos en alta resolución (`.png`, `.pdf`, `.svg`).

---

## 🔬 Metodología de Escalado (Scaling)

Para garantizar la reproducibilidad y facilitar la interpretación de los resultados en los manuscritos, todos los efectos en el archivo `R Plots.R` se escalan a una unidad de **"+100 publicaciones"**. 

Esto se implementa de la siguiente manera:

1. **Efectos Principales (Modelos Lineales - $\beta$)**:
   El coeficiente crudo y sus intervalos de confianza (95% CI) se multiplican por 100.
   `estimate_scaled = estimate_raw * 100`

2. **Modelos de Razón (IRR, OR)**:
   Dado que los estimadores crudos provienen de enlaces logarítmicos (*log-link* o *logit*), se multiplican por 100 antes de aplicar la exponenciación.
   `estimate_scaled = exp(estimate_raw * 100)`

3. **Interacciones (Step 3)**:
   Las pendientes simples (efecto de +100 publicaciones en niveles bajos y altos de un moderador, como el percentil 25 y 75) y los ratios de contraste se calculan propagando de forma exacta el factor `X_SCALE <- 100` a las estimaciones logarítmicas y sus intervalos de confianza.

---

## 🚀 Cómo utilizar este repositorio

1. Clona el repositorio a tu máquina local.
2. Asegúrate de tener instalados los paquetes requeridos de R (`dplyr`, `ggplot2`, `readxl`, `lme4`, `glmmTMB`, `MASS`, `lmtest`, `sandwich`, etc.).
3. Ejecuta `src/Main Analysis.R` si deseas regenerar las regresiones desde los datos crudos.
4. Ejecuta `src/R Plots.R` para aplicar la escala de +100 publicaciones y generar los gráficos listos para publicación.
