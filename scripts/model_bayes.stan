// DENGUE MODEL
//
//
// **DATA STRUCTURE**
//
//  |> Dengue
//  ---------------------------------------------------------------------------
//  `dengue` es un vector con `N_dengue` observaciones (quizá transformadas) dado por:
//   
//           | Dengue |
//           |  10.0  |
//           |  12.0  |
//           |  13.0  |
//
//  las observaciones son semanales y la matriz `anio_mes_semana_dengue` incluye el código
//  del mes, semana epidemiológica y anio de cada observación como sigue:
//
//             dengue                       anio_mes_semana_dengue
//            --------                      ---------------------
//           | Dengue |                    | anio | MES | SEMANA |
//           |  10.0  |                    |  12 |  10 |   43  |
//           |  12.0  |                    |  12 |  10 |   44  |
//           |  13.0  |                    |  12 |  11 |   45  |
//
//  donde las variables `N_anios_dengue` y `N_semanas_dengue` indican la cantidad de semanas
//  y anios registrados en la base. El modelo supone se registraron todos los meses al menos
//  más de una vez. El modelo para dengue es de la forma
// 
//  dengue[semana] = beta_dengue_semana + beta_dengue_anio                  //Efecto seasonal
//        + beta_dengue_AR[semana - 1]*dengue[semana - 1]                         //Efecto autorregresivo
//        + ...                                                            //de orden R
//        + beta_dengue_AR[semana - R]*dengue[semana - R]   
//        + beta_clima[variable 1]*beta_mensual_clima[variable 1, mes]     //Efecto variables
//        + ...                                                            //climatológicas
//        + ...
//        + beta_clima[variable N_vars]*beta_mensual_clima[variable N_vars, mes] 
//
//  donde `R = max_autorregresive_order_dengue`. 
//
//  |> Clima
//  ---------------------------------------------------------------------------
//  `datos_clima` es una base de datos con `N_vars` variables climatológicas registradas a 
//  lo largo de `N_clima` meses como sigue:
//   
//      | varname 1 | varname 2 | varname 3 | .... | varname N_vars |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |
//
// en la matriz `anio_mes_clima` se registra a qué mes y anio corresponde cada uno de esos datos
// es decir:
//
//                        datos_clima                           anio_mes_clima
//      ---------------------------------------------            ------------
//      | varname 1 | varname 2 | varname 3 | .... | varname N_vars |           | anio | MES |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |           | 1   | 10  |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |           | 2   | 11  |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |           | 3   | 11  |
//
// `N_anios_clima` es el total de anios registrados para las variables de clima.
// `N_meses_clima` es el total de meses registrados para las variables de clima.
//
//  El modelo general para clima es de la forma:
//  
//   variable_clima = beta_mensual[variable, mes] + beta_mensual[variable, anio] + error
//
// **NOTAS**
// TODO: Incluir una estructura de correlación entre las variables climáticas

data {
  
  //Datos para generar los efectos de clima en el mes (modelo clima)
  int<lower=1> N_clima;                           //Total de datos de clima
  int<lower=1> N_anios_clima;                      //Total de anios registrados
  int<lower=0> N_vars;                            //Total de variables climatológicas a incluir
  int<lower=1, upper=12> N_meses_clima;           //Total de meses en el anio 
  array[N_clima, N_vars] real datos_clima;        //Matriz de datos de clima
  array[N_clima, 2] int<lower=1> anio_mes_clima;   //Matriz de fechas asociadas a datos_clima
  
  int<lower=0> lag_dengue_clima;                   //Años de diferencia entre el de dengue y el de clima

  //Datos para generar el modelo de dengue
  int<lower=0> N_dengue;                           //Total de observaciones de dengue
  int<lower=0> N_anios_dengue;                      //Total de anios registrados en la base
  int<lower=0, upper=53> N_semanas_dengue;         //Total de semanas registradas en la base
  real dengue[N_dengue];                           //Casos o bien una transformación g(casos) de dengue
  array[N_dengue, 3] int<lower=1> anio_mes_semana_dengue; //anio mes y semana de los registros en `dengue`.
  
  //Parámetros para la predicción
  int<lower=0> N_predict;                                         //Total de tamanio de la matriz de covariables a predecir
  array[N_predict, 3] int<lower=1> anio_mes_semana_dengue_predict; //anio mes y semana de los registros a predecir de dengue
  
  //Hiperparámetros del modelo 
  int<lower=0> max_autorregresive_order_dengue;    //Orden del componente autorregresivo 
  
  //Datos para generar los apriori
}

transformed data {
  
  //Número de columnas para anio mes y semana en dengue
  int <lower=1> col_anio_d = 1;
  int <lower=1> col_mes_d = 2;
  int <lower=1> col_sem_d = 3;
  
  //Número de columnas para anio y mes en clima
  int <lower=1> col_anio_c = 1;
  int <lower=1> col_mes_c = 2;
  
}

parameters {

  vector[max_autorregresive_order_dengue]  beta_dengue_AR;
  vector[max_autorregresive_order_dengue]  error_dengue_AR;
  
  //Parametrizaciones de la media para poder centrar y que funcione mejor el MCMC
  matrix[N_meses_clima, N_vars] Z_clima_mes;
  matrix[N_anios_clima, N_vars]  Z_clima_anio;
  
  vector[N_semanas_dengue] Z_dengue_semana;
  vector[N_anios_dengue]    Z_dengue_anio;
  
  vector[N_vars] beta_dengue_clima;

  matrix[N_clima, N_vars] error_clima_vars;
  
  //Variance
  real<lower=0> sigma_sq;
  
  vector<lower=0>[N_vars]  sigma_mes_clima;
  vector<lower=0>[N_vars]  sigma_anio_clima;
  
  real<lower=0> sigma_semana_dengue;
  real<lower=0> sigma_anio_dengue;
  
  real<lower=0> sigma_AR;
  vector<lower=0>[N_vars] phi_clima;
  real<lower=0> phi_dengue;
  
}

transformed parameters {
  
  //Instanciamos variables
  real mu_clima[N_clima, N_vars];
  real mu_dengue[N_dengue];
  
  matrix[N_meses_clima, N_vars] beta_mes_clima;
  matrix[N_anios_clima, N_vars]  beta_anio_clima;
  
  vector[N_semanas_dengue]  beta_semana_dengue;
  vector[N_anios_dengue]     beta_anio_dengue;
  
  //Construcción de las dinámicas para dengue
  // beta_semana[t] = Normal(beta_semana[t-1], sigma^2)
  beta_semana_dengue[1] = sigma_semana_dengue*Z_dengue_semana[1];
  if (N_semanas_dengue > 1){
    for (week in 2:N_semanas_dengue){
      beta_semana_dengue[week] = beta_semana_dengue[week - 1] + sigma_semana_dengue*Z_dengue_semana[week];
    }
  }
  
  beta_anio_dengue[1] = sigma_anio_dengue*Z_dengue_anio[1];
  if (N_anios_dengue > 1){
    for (yr in 2:N_anios_dengue){
      beta_anio_dengue[yr] = beta_anio_dengue[yr - 1] + sigma_anio_dengue*Z_dengue_anio[yr];
    }
  }
  
  //Construcción de las dinámicas para clima
  // beta_mes[t] = Normal(beta_mes[t-1], sigma^2)
  for (varname in 1:N_vars){
    beta_mes_clima[1,varname] = sigma_mes_clima[varname] * Z_clima_mes[1,varname];
    if (N_meses_clima > 1){
      for (mes in 2:N_meses_clima){
        beta_mes_clima[mes,varname] = beta_mes_clima[mes - 1,varname] + 
          sigma_mes_clima[varname] * Z_clima_mes[mes,varname];
      }
    }
    
    beta_anio_clima[1,varname] = sigma_anio_clima[varname] * Z_clima_anio[1,varname];
    if (N_anios_clima > 1){
      for (yr in 2:N_anios_clima){
        beta_anio_clima[yr,varname] = beta_anio_clima[yr - 1,varname] + 
          sigma_anio_clima[varname] * Z_clima_anio[yr,varname];
      }
    }
    
    //Creamos la media de clima
    for (n in 1:N_clima){
      mu_clima[n,varname] = beta_mes_clima[anio_mes_clima[n,col_mes_c], varname] + 
          beta_anio_clima[anio_mes_clima[n,col_anio_c], varname] + error_clima_vars[n,varname];
    }
  }
  
  
  //Creamos la media de dengue
  for (n in 1:N_dengue){
    
    mu_dengue[n] = 0;
    
    //Efecto de semana y anio
    mu_dengue[n] += beta_semana_dengue[anio_mes_semana_dengue[n, col_sem_d]]; //Semana
    mu_dengue[n] += beta_anio_dengue[anio_mes_semana_dengue[n, col_anio_d] - lag_dengue_clima];    //anio
    
    //Efectos climáticos
    for (varname in 1:N_vars)
      mu_dengue[n] += beta_dengue_clima[varname]*beta_mes_clima[anio_mes_semana_dengue[n, col_mes_d], varname]; //Semana
    
    //Efecto de orden autorregresivo. TODO: Simplficiar como producto de vectores
    if (max_autorregresive_order_dengue > 0){
      for (r in 1:max_autorregresive_order_dengue){
        if (n - r > 0){
          mu_dengue[n] += beta_dengue_AR[r]*dengue[n - r];
        } else {
          mu_dengue[n] += error_dengue_AR[r];
        }
      }
    }
  }
}

model {
  
  error_dengue_AR     ~ std_normal();
  
  for (varname in 1:N_vars){
    Z_clima_mes[:,varname]   ~ std_normal();
    Z_clima_anio[:,varname]  ~ std_normal();
    error_clima_vars[:,varname]    ~ std_normal();
  }
  
  Z_dengue_semana     ~ std_normal();
  Z_dengue_anio        ~ std_normal();
  
  
  
  sigma_sq            ~ cauchy(0, 2.5);
  
  sigma_mes_clima     ~ normal(0, sigma_sq);
  sigma_anio_clima     ~ normal(0, sigma_sq);
  sigma_semana_dengue ~ normal(0, sigma_sq);
  sigma_anio_dengue    ~ normal(0, sigma_sq);
  
  sigma_AR            ~ cauchy(0, 2.5);
  phi_dengue          ~ cauchy(0, 2.5);
  phi_clima           ~ cauchy(0, 2.5);
  
  
  if (max_autorregresive_order_dengue > 1){
    for (r in 1:max_autorregresive_order_dengue){
      beta_dengue_AR[r] ~ normal(0.0, sigma_AR);
    }
  }
  
  for (varname in 1:N_vars){
    for (n in 1:N_clima){
      datos_clima[n,varname] ~ normal(log(100.0)*mu_clima[n, varname], phi_clima[varname]); 
    }
  }
    
  
  for (n in 1:N_dengue)
    dengue[n] ~ normal(log(100.0)*mu_dengue[n], phi_dengue);
}


/*generated quantities {
  vector[N + N_predict] mu_predict;
  vector[Nyears + Nyears_predict] beta_anio_dengue_predict;
  real dengue_predict[N + N_predict];
  
  int<lower=1> epiweek_predict_complete[N + N_predict]; //Week number
  int<lower=1> year_predict_complete[N + N_predict]; //Year number
  
  //Simulate beta for years
  for (yr in 1:(Nyears + Nyears_predict)){
    if (yr > year_observed_max){
      beta_anio_dengue_predict[yr] = normal_rng(beta_anio_dengue_predict[yr - 1], sigma_anio_dengue);
    } else {
      beta_anio_dengue_predict[yr] = beta_anio_dengue[yr];
    }
  }
  
  for (n in 1:(N + N_predict)){
    
    //Update either to previous or new
    if (n > N){ 
      year_predict_complete[n]    =  year_predict[n - N];
      epiweek_predict_complete[n] =  epiweek_predict[n - N];  
      
      mu_predict[n] = beta_semana_dengue[epiweek_predict_complete[n]] + 
        beta_anio_dengue_predict[year_predict_complete[n]];
        
      for (r in 1:max_autorregresive_order_dengue){
        if (n - r > N){
          mu_predict[n] += beta_dengue_AR[r]*dengue_predict[n - r];
        } else {
          mu_predict[n] += beta_dengue_AR[r]*dengue[n - r];
        }
      }  
        
    } else {
      year_predict_complete[n]    =  year[n];
      epiweek_predict_complete[n] =  epiweek[n];    
      mu_predict[n] = mu[n];
    }
    
    dengue_predict[n] = normal_rng(log(100.0)*mu_predict[n], phi);
    
  }
}*/