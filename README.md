# DengueMX

> :warning: Al **4 de julio 2022* la Secretaría de Salud no ha actualizado la base de datos abiertos por lo que 2022 se obtiene de los panoramas epidemiológicos. Espero esta solución sea temporal.

![Casos de dengue en México](images/Dengue_predict.png)

Repositorio de los datos abiertos y scrappeados de dengue en México junto con modelo de predicción de casos.

Brevemente el modelo sigue la siguiente estructura:
$$\textrm{Dengue} = \textrm{Clima} + \textrm{Dengue semanas pasadas} + \textrm{Semana} + \textrm{Año}$$

una explicación más técnica del modelo la puedes encontrar en [`MODELO.md`](MODELO.md).

## Historia de los casos

![Casos de dengue en México por entidad federativa](images/Dengue_estado.png)

## Estructura del repositorio 

+ `datos-abiertos` contienen los datos abiertos (históricos) de la DGE y su diccionario.
    + `datos_abiertos_dengue_*.zip` base de datos decargada de la [Dirección General de Epidemiología](https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-de-enfermedades-transmitidas-por-vector) correspondiente a la fecha `*`.
    + `diccionario` archivo con el diccionario de los datos (descargado el 19 de mayo 2022) contiene dos bases: 
        + `Descriptores_Dengue.xlsx` con los descriptores de las variables.
        + `Catalogos_Dengue.xlsx` con el catálogo de variables.    
+ `datos-limpios` contiene la base de datos de 2016 a 2022 de los datos de dengue ya limpios en formato tidy
    + `dengue_2016_2022_mx.rds` base de datos con la información de  dengue desde el registro de las semanas epidemiológicas de `2016` hasta el `2022`. Para leer usa `readr::read_rds`.
    + `dengue_2016_2022_mx.csv` base de datos con la información de dengue desde el registro de las semanas epidemiológicas de `2016` hasta el `2022`. Para leer usa `readr::read_csv` con `UTF-8` de encoding.
    + `dengue_for_model_mx.csv` contiene los datos de dengue de 2015 a la fecha con los datos de las semanas faltantes interpolados así como transformaciones a log_casos. Es input para el modelo. 
+ `datos-clima` contiene las bases de datos de 1985 a 2022 de precipitación promedio por entidad y temperaturas (mínima, máxima y promedio) por mes. Las bases de datos son descargadas de los resúmenes de [CONAGUA](https://smn.conagua.gob.mx/es/climatologia/temperaturas-y-lluvias/resumenes-mensuales-de-temperaturas-y-lluvias) para 2021-2022 así como de los [datos abiertos](https://datos.gob.mx/busca/dataset/temperatura-promedio-excel) para 1985-2019. 
    + `descarga_clima_1985_today.R` se encarga de descargar los datos de CONAGUA y de datos abiertos así como de usar `python` para procesar los `pdf` de CONAGUA (2021 en adelante). 
    + `images` imágenes auxiliares para visualizar las variables de clima como auxiliares al modelo. 
    + `processed` contiene las bases de datos de las variables climatológicas ya limpias en formato `tidy`. 
        + `Clima_info.csv` y `Clima_info.rds` son la misma base para leer con `readr`. Contienen temperaturas y precipitaciones por entidad desde 1985. Se generan por `descarga_clima_1985_today`. 
    + `pdf_reports` contiene los reportes que se descargan automáticamente de CONAGUA. 
        + `PREC_*.pdf` Reporte de precipitación para el año `*`.
        + `TMAX_*.pdf` Reporte de temperatura máxima para el año `*`.
        + `TMED_*.pdf` Reporte de temperatura promedio para el año `*`.
        + `TMIN_*.pdf` Reporte de temperatura mínima para el año `*`.
    + `Precipitacion.zip`, `Temperatura_Maxima_Excel.zip`, `Temperatura_Minima_Excel.zip`, `Temperatura_Promedio_Excel.zip` son los  [datos abiertos](https://datos.gob.mx/busca/dataset/temperatura-promedio-excel) que contienen la variable respectiva por entidad en formato Excel desde 1985 al 2020 (y 2021 parcialmente). 
    
        > **NOTA** Hay dos archivos con información del 2020 en estos `.zip` uno de ellos es incompleto. 
+ `images` Imágenes de los resultados con `Dengue_predict` incluyendo los resultados del modelo mientras que `Dengue` y `Dengue_estado` son visualizaciones de cómo va la enfermedad a la fecha. 
+ `orchestrate.sh` Mi script de Linux para correr en `crontab` y actualizar el modelo semanalmente.     
+ `panoramas_epidemiologicos_previos` contienen los `pdf` de anteriores panoramas epidemiológicos de dengue por año. 
    + `import` contiene el código de Python para importar los panoramas (un código por año).
    + `processed` contiene los datos scrappeados de los panoramas epidemiológicos previos
    + `unreadable` documentos `pdf` de panoramas epidemiológicos que no se pudieron leer. 
    + `2016`-`2021` carpetas con documentos en `pdf` de los panoramas epidemiológicos de la Secretaría de Salud. 
+ `scripts` para la descarga y el formateo de datos
    + `daterange.py` módulo auxiliar para `Descarga.py`. 
    + `Descarga.py` para descarga diaria de los datos màs nuevos
    + `format_dataset.R` para correr los datos nuevos y ponerlos para el modelo
    + `model_bayes.R` el modelo para predecir dengue a nivel nacional 
    + `model_bayes.stan` archivo de `Stan` con el modelo de dengue. 
    + **NO CORRER** `modelo_clima.R`  subset del modelo completo enfocado para hacer experimentos en la parte de clima del modelo. 
    + **NO CORRER** `predice_clima.stan`  subset del modelo completo enfocado para hacer experimentos en la parte de clima del modelo. 



## Colabora

La meta es tener un (mejor) modelo de dengue aunque sea a nivel nacional. Éste es sólo una prueba de concepto. Si te interesa escríbeme o manda un pull request. Algunas cosas en las que podrías colaborar:

+ Descargar datos de años previos. 
+ Mejorar el modelo (por ahora es sólo uno de juguete por falta de tiempo). 