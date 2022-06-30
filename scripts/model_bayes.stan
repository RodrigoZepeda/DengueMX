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
//        + beta_dengue_AR[semana - 1]*dengue[semana - 1]                   //Efecto autorregresivo
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
//      | var 1 | var 2 | var 3 | .... | var N_vars |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |
//      |  10.0 | 13.1  | 14.2  | ...  | 20.1       |
//
// en la matriz `anio_mes_clima` se registra a qué mes y anio corresponde cada uno de esos datos
// es decir:
//
//                        datos_clima                           anio_mes_clima
//      ---------------------------------------------            ------------
//      | var 1 | var 2 | var 3 | .... | var N_vars |           | anio | MES |
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
// el modelo de clima está programado en predice_clima.stan

data {
  //Esta parte es igual al modelo predict_clima.stan
  //Datos para generar los efectos de clima en el mes (modelo clima)
  int<lower=0> N_clima;                           //Total de datos de clima
  int<lower=0, upper=12>       N_meses_clima;     //Número de meses
  int<lower=0, upper=N_clima>  N_anios_clima;     //Número de años
  int<lower=0> N_vars;                            //Total de variables climatológicas a incluir
  matrix[N_clima, N_vars]  y_clima;               //Temperatura y_clima lluvia
  array[N_clima, 2] int<lower=0> anio_mes_clima;  //Año y_clima mes
  
  
  //Datos para generar el modelo de dengue
  int<lower=0> N_dengue;                          //Total de observaciones de dengue
  int<lower=0> N_anios_dengue;                    //Total de anios registrados en la base
  int<lower=0, upper=53> N_semanas_dengue;        //Total de semanas registradas en la base
  vector[N_dengue] dengue;                        //Casos o bien una transformación g(casos) de dengue
  array[N_dengue, 3] int<lower=1> anio_mes_semana_dengue; //anio mes y semana de los registros en `dengue`.
  
  //Hiperparámetros del modelo 
  int<lower=0> max_autorregresive_order_dengue;    //Orden del componente autorregresivo 
  
  //Para predecir
  int<lower=0> N_dengue_predict;                   //Total de observaciones adicionales de dengue a predecir
  array[N_dengue + N_dengue_predict, 3] int<lower=1> anio_mes_semana_dengue_predict; //anio mes y semana de los registros en `dengue` que se van a predecir. Debe incluir los registros pasados anio_mes_semana_dengue %>% rbind(nuevos)
  
}

transformed data {
  
  //Normalización de las variables climáticas
  matrix[N_clima, N_vars] y_clima_std;  //Standarized normal outcome variable 
  vector[N_vars]    mu_y_clima;         //Mean of each of the variables
  vector[N_vars]    sd_y_clima;         //Variance of each of the variables
  
  array[N_dengue] int<lower=1>  anio_dengue;
  array[N_dengue + N_dengue_predict] int<lower=1>  anio_dengue_predict;
  
  int<lower=0>    max_anio_dengue;    //Último año registrado en el dengue
  int<lower=0>    min_anio_dengue;    //Lag entre el dengue y el clima
  int<lower=0>    max_anio_dengue_predict; //Último año futuro que se desea predecir
  int<lower=0>    Nyears_predict;     //Total de años extras a predecir 
  
  //Normalization of data in order to make all betas comparable
  for (varname in 1:N_vars){
    mu_y_clima[varname] = mean(y_clima[:,varname]);
    sd_y_clima[varname] = sd(y_clima[:,varname]);
    for (n in 1:N_clima)
      y_clima_std[n,varname] = (y_clima[n,varname] - mu_y_clima[varname]) / sd_y_clima[varname];
  }
  
  //Número de columnas para anio mes y semana en dengue
  int <lower=1> col_anio_d = 1;
  int <lower=1> col_mes_d  = 2;
  int <lower=1> col_sem_d  = 3;
  
  //Número de columnas para anio y mes en clima
  int <lower=1> col_anio_c = 1;
  int <lower=1> col_mes_c  = 2;
  
  //Número mínimo del año de dengue
  min_anio_dengue = min(anio_mes_semana_dengue[:, col_anio_d]);
  max_anio_dengue = max(anio_mes_semana_dengue[:, col_anio_d]);
  max_anio_dengue_predict = max(anio_mes_semana_dengue_predict[:, col_anio_d]);
  
  //Total de años a predecir después de N_anios_dengue
  Nyears_predict = max_anio_dengue_predict - max_anio_dengue;
  
  for (n in 1:N_dengue)
    anio_dengue[n] = anio_mes_semana_dengue[n, col_anio_d] - (min_anio_dengue - 1);
  
  anio_dengue_predict[1:N_dengue] = anio_dengue[1:N_dengue];
  for (n in (N_dengue + 1):(N_dengue + N_dengue_predict))
    anio_dengue_predict[n] = anio_mes_semana_dengue_predict[n, col_anio_d] - (min_anio_dengue - 1);
}

parameters {

  //------------------------------------------------
  //CLIMA Parámetros del modelo de clima
  //------------------------------------------------
  matrix[N_meses_clima, N_vars] Z_beta_month_clima; //Matrix with monthly effects
  matrix[N_anios_clima, N_vars] Z_beta_year_clima;  //Matrix with yearly effects
  
  corr_matrix[N_vars] Omega_clima;         // prior correlation
  vector<lower=0>[N_vars] tau_clima;       //Prior scale

  //Standard normals to improve the space
  vector[N_anios_clima] Z_year_prior_clima;
  vector[N_meses_clima] Z_month_prior_clima;

  //Variances
  real<lower=0> sigma_sq_month_clima;
  real<lower=0> sigma_sq_year_clima;
  real<lower=0> sigma_sq_year_prior_clima;
  real<lower=0> sigma_sq_month_prior_clima;
  real<lower=0> sigma_sq_super_clima;
  
  //------------------------------------------------
  //DENGUE Parámetros del modelo de dengue
  //------------------------------------------------

  vector[max_autorregresive_order_dengue]  beta_dengue_AR;
  vector[max_autorregresive_order_dengue]  error_dengue_AR;
  
  //Parametrizaciones de la media para poder centrar y que funcione mejor el MCMC
  vector[N_semanas_dengue] Z_dengue_semana;
  vector[N_anios_dengue]   Z_dengue_anio;
  
  //Conexión entre la beta del mes de clima y la del dengue
  vector[N_vars] beta_dengue_clima;
  
  //Variance
  real<lower=0> sigma_sq;
  real<lower=0> sigma_semana_dengue;
  real<lower=0> sigma_anio_dengue;
  real<lower=0> sigma_dengue_clima;
  real<lower=0> sigma_AR;
  real<lower=0> phi_dengue;
  
}

transformed parameters {
  
  //------------------------------------------------
  //CLIMA Parámetros del modelo de clima
  //------------------------------------------------
  
  vector[N_meses_clima]   beta_month_super_clima; //Hierarchical beta for months
  vector[N_anios_clima]   beta_year_super_clima;  //Hierarchical beta for years
  
  matrix[N_meses_clima, N_vars] beta_month_clima; //Matrix with monthly effects  
  matrix[N_anios_clima, N_vars] beta_year_clima;  //Matrix with yearly effects
  
  matrix[N_clima, N_vars] mu_clima;
  matrix[N_vars, N_vars] Sigma_clima;

  matrix[N_clima, N_vars] error_clima;             //Standarized normal outcome variable 
  vector[N_vars] mean_zero;

  //------------------------------------------------
  //DENGUE Parámetros del modelo de dengue
  //------------------------------------------------

  //Instanciamos variables
  vector[N_dengue] mu_dengue;
  vector[N_semanas_dengue]  beta_semana_dengue;
  vector[N_anios_dengue]    beta_anio_dengue;
  
  vector[N_dengue] error_dengue;             //Standarized normal outcome variable 
  
  //------------------------------------------------
  //CLIMA Transformaciones del modelo de clima
  //------------------------------------------------
  
  //Prior covariance
  Sigma_clima = quad_form_diag(Omega_clima, tau_clima);
    
  //Prior for years
  beta_year_super_clima[1] = sigma_sq_year_prior_clima*Z_year_prior_clima[1];
  if (N_anios_clima > 1){
    for (year in 2:N_anios_clima)
      beta_year_super_clima[year] = beta_year_super_clima[year - 1] + sigma_sq_year_prior_clima*Z_year_prior_clima[year];
  }
  
  //Prior for months
  beta_month_super_clima[1] =sigma_sq_month_prior_clima*Z_month_prior_clima[1];
  if (N_meses_clima > 1){
    for (month in 2:N_meses_clima)
      beta_month_super_clima[month] = beta_month_super_clima[month - 1] + sigma_sq_month_prior_clima*Z_month_prior_clima[month];
  }

  //Beta month year
  for (varname in 1:N_vars){
  
    for (year in 1:N_anios_clima)
      beta_year_clima[year, varname]   = beta_year_super_clima[year]   + sigma_sq_year_clima*Z_beta_year_clima[year, varname];
      
    for (month in 1:N_meses_clima)  
      beta_month_clima[month, varname] = beta_month_super_clima[month] + sigma_sq_month_clima*Z_beta_month_clima[month, varname];

  }

  //Mean
  for (n in 1:N_clima)
    mu_clima[n,:] = beta_year_clima[anio_mes_clima[n,1],:] + beta_month_clima[anio_mes_clima[n,2],:];
  
  //Error
  error_clima = y_clima_std - mu_clima;
  
  //Mean zero vector
  mean_zero = rep_vector(0.0, N_vars);
    
  //------------------------------------------------
  //DENGUE Transformaciones del modelo de dengue
  //------------------------------------------------
  
  //Construcción de las dinámicas para dengue
  // beta_semana[t] = Normal(beta_semana[t-1], sigma^2)
  beta_semana_dengue[1] = sigma_semana_dengue*Z_dengue_semana[1];
  if (N_semanas_dengue > 1){
    for (week in 2:N_semanas_dengue){
      beta_semana_dengue[week] = beta_semana_dengue[week - 1] + sigma_semana_dengue*Z_dengue_semana[week];
    }
  }
  
  //Dinámica de efecto anual de dengue
  beta_anio_dengue[1] = sigma_anio_dengue*Z_dengue_anio[1];
  if (N_anios_dengue > 1){
    for (yr in 2:N_anios_dengue){
      beta_anio_dengue[yr] = beta_anio_dengue[yr - 1] + sigma_anio_dengue*Z_dengue_anio[yr];
    }
  }
  
  //Creamos la media de dengue
  for (n in 1:N_dengue){
    
    //Efecto de semana y anio
    mu_dengue[n] = beta_semana_dengue[anio_mes_semana_dengue[n, col_sem_d]] + //Efecto semana
      beta_anio_dengue[anio_dengue[n]];  //Efecto anio
    
    //Efectos climáticos
    for (varname in 1:N_vars)
      mu_dengue[n] += beta_dengue_clima[varname]*beta_month_clima[anio_mes_semana_dengue[n, col_mes_d], varname]; //Semana
    
    //Efecto de orden autorregresivo. TODO: Simplficiar como producto de vectores
    
    for (r in 1:max_autorregresive_order_dengue){
      if (n - r > 0){
        mu_dengue[n] += beta_dengue_AR[r]*dengue[n - r];
      } else {
        mu_dengue[n] += error_dengue_AR[r];
      }
    }
  }
  
  //Normal errors
  error_dengue = dengue - mu_dengue;
}

model {
  
  //------------------------------------------------
  //CLIMA Verosimilitud del modelo de Clima
  //------------------------------------------------
  
  //Variances
  sigma_sq_super_clima        ~ cauchy(0, 2.5);
  sigma_sq_year_prior_clima   ~ cauchy(0, sigma_sq_super_clima);
  sigma_sq_month_prior_clima  ~ cauchy(0, sigma_sq_super_clima);
  sigma_sq_month_clima        ~ cauchy(0, 2.5);
  sigma_sq_year_clima         ~ cauchy(0, 2.5);
  
  //Prior variance for SUG
  tau_clima   ~ cauchy(0, 2.5);
  Omega_clima ~ lkj_corr(2);
  
  //For prior years
  Z_year_prior_clima  ~ std_normal();
  Z_month_prior_clima ~ std_normal();

  to_vector(Z_beta_year_clima)  ~ std_normal();
  to_vector(Z_beta_month_clima) ~ std_normal();
   
  for (n in 1:N_clima)
    error_clima[n,:] ~ multi_normal(mean_zero, Sigma_clima);
  
  //------------------------------------------------
  //DENGUE Verosimilitud del modelo de dengue
  //------------------------------------------------
  
  error_dengue_AR     ~ std_normal();
  Z_dengue_semana     ~ std_normal();
  Z_dengue_anio       ~ std_normal();
  
  sigma_sq            ~ cauchy(0, 2.5);
  sigma_semana_dengue ~ cauchy(0, sigma_sq);
  sigma_anio_dengue   ~ cauchy(0, sigma_sq);
  sigma_dengue_clima  ~ cauchy(0, 2.5);
  sigma_AR            ~ cauchy(0, 2.5);
  phi_dengue          ~ cauchy(0, 2.5);
  
  beta_dengue_AR      ~ normal(0.0, sigma_AR);
  beta_dengue_clima   ~ normal(0.0, sigma_dengue_clima);
  error_dengue        ~ normal(0.0, phi_dengue);
}


generated quantities {
  
  vector[N_dengue + N_dengue_predict] mu_dengue_predict;
  vector[N_dengue + N_dengue_predict] dengue_predict;
  vector[max_anio_dengue_predict - (min_anio_dengue - 1)]  beta_anio_dengue_predict;

  //Simulate beta for years
  for (yr in 1:(N_anios_dengue + Nyears_predict)){
    //Si no me he pasado, coloco la variable estimada
    if (yr <= N_anios_dengue){
      beta_anio_dengue_predict[yr] = beta_anio_dengue[yr];
    //Si ya me pasé, simulo el nuevo año
    } else {
      beta_anio_dengue_predict[yr] = normal_rng(beta_anio_dengue_predict[yr - 1], sigma_anio_dengue);
    }
  }
  
  //Predict the new values
  for (n in 1:(N_dengue + N_dengue_predict)){
    
    if (n <=  N_dengue){

      mu_dengue_predict[n] = mu_dengue[n];

    } else {
    
      //Efecto de semana y anio
      mu_dengue_predict[n] = beta_semana_dengue[anio_mes_semana_dengue_predict[n, col_sem_d]] + 
        beta_anio_dengue_predict[anio_dengue_predict[n]];  //Efecto anio
    
      //Efectos climáticos
      for (varname in 1:N_vars)
        mu_dengue_predict[n] += beta_dengue_clima[varname]*beta_month_clima[anio_mes_semana_dengue_predict[n, col_mes_d], varname]; //Semana
    
      //Efecto de orden autorregresivo. 
      for (r in 1:max_autorregresive_order_dengue){
        if (n - r > 0){
          if (n - r <= N_dengue){
            mu_dengue_predict[n] += beta_dengue_AR[r]*dengue[n - r];
          } else {
            mu_dengue_predict[n] += beta_dengue_AR[r]*dengue_predict[n - r];
          }
        } else {
          mu_dengue_predict[n] += error_dengue_AR[r];
        }
      }
    }

    //SIMULATE!
    dengue_predict[n] = normal_rng(mu_dengue_predict[n], phi_dengue);
  }
}
