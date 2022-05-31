# DengueMX

![Casos de dengue en México](images/Dengue_predict.png)

Repositorio de los datos abiertos y scrappeados de dengue en México.

+ `datos-abiertos` contienen los datos abiertos (históricos) de la DGE y su diccionario.
+ `datos-limpios` contiene la base de datos de 2016 a 2022 de los datos de dengue ya limpiosen formato tidy
+ `panoramas_epidemiologicos_previos` contienen los `pdf` de anteriores panoramas epidemiológicos de dengue por año. 
    + `import` contiene el código de Python para importar los panoramas (un código por año).
    + `processed` contiene los datos scrappeados de los panoramas epidemiológicos previos
+ `scripts` para la descarga y el formateo de datos

![Casos de dengue en México](images/Dengue_predict.png)

## Colabora

La meta es tener un (mejor) modelo de dengue aunque sea a nivel nacional para el próximo año. Éste es sólo una prueba de concepto. Si te interesa escríbeme o manda un pull request. Algunas cosas en las que podrías colaborar:

+ Descargar datos de años previos. 
+ Conseguir covariables para mejorar la predicción (ejemplo variables de clima).
+ Mejorar el modelo (por ahora es sólo uno de juguete por falta de tiempo). 