# Descripción del modelo
 MODIFICAR

## Datos

### Incidencia de dengue 
La información de 2016 a la fecha fue obtenida de la Secretaría de Salud a través de:

+ Panoramas epidemiológicos de dengue 2016-2020.

+ [Datos abiertos de la Secretaría de Salud](https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-de-enfermedades-transmitidas-por-vector) (2021 en adelante).

La incidencia de dengue se agrega de manera semanal por semana epidemiológica en una base similar a esta: 

---
| Casos (`nraw`) | Semana Epidemiológica (`epiweek`) | Año Epidemiológico (`year`) |
|-------|--------|-----|
|$d_1$  |    1   |  13  |
|$d_2$  |    2   |  13  |
|$d_3$  |    3   |  13  |
| ...   | ...    | ... |
|$d_{N_d}$  |    44   |  17  |
---

en el caso del modelo es la variable `nraw` dentro de `datos-limpios/dengue_for_model_mx.csv`. 

### Precipitación y temperaturas

La información de 1985 a la fecha se obtuvo de la CONAGUA a través de:

+ Reportes mensuales de precipitación y temperatura promedio. 

+ [Datos abiertos de CONAGUA](https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-de-enfermedades-transmitidas-por-vector) (hasta 2020).

Las variables climatológicas (_ej_ precipitación y temperatura) están agrupadas por mes y se presentan de la siguiente manera:

---
|Temperatura | Precipitación (`Precipitacion`) | Mes (`MES_NUM`) | Año Epidemiológico (`ANIO`) |
|-------|-------|--------|-----|
|$c^1_1$ |$c^2_1$ |    1   |  1  |
|$c^1_2$ |$c^2_2$ |    2   |  1  |
|$c^1_3$ |$c^2_3$ |    3   |  1  |
| ...    |  ...   |  ...   | ... |
|$c^1_{N_c}$ |$c^2_{N_c}$ |    44   |  15  |
---

en general se asume que existen $M$ variables climatológicas distintas.  

## Modelo

Brevemente el modelo sigue la siguiente estructura:
$$\textrm{Dengue} = \textrm{Clima} + \textrm{Dengue semanas pasadas} + \textrm{Semana} + \textrm{Año}$$

Para ello se realiza primero un modelo de _Clima_ y después los factores que se obtienen de dicho modelo se utilizan para el de _Dengue_. 


### Modelo de clima 

Se asume que las variables de clima siguen el siguiente modelo: 
$$c_j^{\textrm{std}} \sim \text{Normal}(\mu_j,\Sigma)$$
con $c_j^{\textrm{std}} = \Big( (c_j^1 - \bar{c}^1)/\text{sd}(c^1), (c_j^2 - \bar{c}^2)/\text{sd}(c^2),\dots, (c_j^{M} - \bar{c}^M)/\text{sd}(c^M) \Big)^T$ las variables de clima estandarizadas en el momento $j$. 

La media $\mu_j = (\mu_j^1,\mu_j^2,\dots, \mu_j^M)^T$ al tiempo $j$ está dada por una dependencia anual y una mensual:
$$\mu_j^k = \beta_{\text{Año} (j)}^k + \beta_{\text{Mes} (j)}^k$$
para $k = 1,2\dots, M$.

Las variables de año y mes siguen estructuras jerárquicas:
$$\beta_{\text{Año} (j)}^k  \sim \text{Normal}(\eta_{\text{Año}},\sigma^2_{\beta_\text{Año}}) \quad \text{y} \quad \beta_{\text{Mes} (j)}^k  \sim \text{Normal}(\eta_{\text{Mes}},\sigma^2_{\beta_\text{Mes}})$$
donde los hiperparámetros siguen una estructura dinámica dependiendo del mes/año anterior:
$$\eta_{\text{Año}} \sim \text{Normal}(\eta_{\text{Año} - 1},\sigma^2_{\eta_\text{Año}}) \quad \text{y} \quad \eta_{\text{Mes}} \sim \text{Normal}(\eta_{\text{Mes} - 1},\sigma^2_{\eta_\text{Mes}}).$$

Por otro lado la variable $\Sigma = S\Omega S$ con $S$ una matriz diagonal dada por 
$$S = \text{diag}(\tau_1, \tau_2, \dots, \tau_M)$$
con 
$$\tau_j \sim \text{HalfCauchy}(0,2.5) \text{ y } \Omega \sim \text{LKJCorr}(\eta).$$ 

Las varianzas siguen distribuciones Cauchy positivas:
$$\sigma^2_{\beta_\text{Año}} \sim \text{HalfCauchy}(0,2.5)$$

$$\sigma^2_{\beta_\text{Mes}} \sim \text{HalfCauchy}(0,2.5)$$

$$\sigma^2_{\eta_\text{Año}} \sim \text{HalfCauchy}(0,\sigma^2_{\eta})$$

$$\sigma^2_{\eta_\text{Mes}} \sim \text{HalfCauchy}(0,\sigma^2_{\eta})$$

$$\sigma^2_{\eta}  \sim \text{HalfCauchy}(0,2.5).$$

### Modelo de dengue

El modelo de dengue integra los factores mensuales del modelo de clima _i.e._ los $\beta_{\text{Mes} (j)}^k$ así como un componente autorregresivo (AR) de orden $R$ y componentes semanales y anuales. Si $g(d_{i})$ son los casos de dengue después de una transformación $g$ en el tiempo $i$ entonces

$$g(d_i) \sim \textrm{Normal}(m_i, \phi)$$

con $\phi \sim\text{HalfCauchy}(0, 2.5)$ y $m_i$ dada por:
$$m_i = \alpha_{\text{Año} (i)} + \alpha_{\text{Semana} (i)} + \sum_{k=1}^M \alpha^k_{\text{Clima}} \cdot \beta^k_{\text{Mes} (i)} + \sum_{p=1}^R \alpha^p_{\text{AR}} \cdot g(d_{i-p})$$

donde 
$$\alpha^p_{\text{AR}} \sim \text{Normal}(0, \sigma^2_{AR}) \text{ y } \alpha^k_{\text{Clima}} \sim \text{Normal}(0, \sigma^2_{\text{Clima}})$$ 
con las _a priori_ dadas por:
$$\sigma^2_{AR}\sim\text{HalfCauchy}(0, \sigma^2) \quad \text{y} \quad \sigma^2_{Clima}\sim\text{HalfCauchy}(0, \sigma^2).$$

Las variables anuales tienen una media compartida $$\alpha_{\text{Año}} \sim \text{Normal}(\theta_{\alpha_\text{Año}},\sigma^2_{\alpha_\text{Año}})$$

mientras que las semanales tienen una estructura dinámica dada por: $$\text{Normal}(\alpha_{\text{Semana} - 1},\sigma^2_{\alpha_\text{Semana}}).$$

donde $$\sigma^2_{\alpha_\text{Año}},\sigma^2_{\alpha_{\text{Semana}}}\sim\text{HalfCauchy}(0, \sigma^2) \text{ y  }\sigma^2\sim\text{HalfCauchy}(0, 2.5).$$ 