//MODELO DE LAS LLUVIAS / TEMPERATURAS MENSUALES
//
//El modelo consiste en una serie de `Nvars` variables y_clima[n,1], y_clima[n,2], ..., y_clima[n, Nvars]
//medidas de manera mensual en un periodo de tamaño `N_clima`. Se supone dichas variables
//están correlacionadas por el año, mes y_clima el error_clima y_clima representan precipitación y_clima temperatura. 
//
//*MODELO*
//-----------------------------------------------------------------------------
//
//El modelo opera sobre las variables normalizadas:
//
//          y_clima_std[n, var] = (y_clima[n, var] - mean(y_clima[:,var])) / sd(y_clima[:, var])
//
//donde se asume que para cada n:
//
//          y_clima_std[n, :] = beta_mes[ mes[n], :] + beta_año[ año[n],:] + error_clima[n,:]
//
//donde los términos de error_clima a lo largo de n están correlacionados.
//Escrito de otra forma el modelo es:
//
//          y_clima_std[n, :] ~ NormalMulti(mu_clima[n,:], Sigma_clima)
//
//para mu_clima[n,:] = beta_mes[ mes[n], :] + beta_año[ año[n],:].
//
//Suponemos una estructura jerárquica en el efecto de meses y_clima años:
//
// beta_mes[mes,:] ~ Normal(beta_mes_jerárquico[mes], sigma_mes)
//
// beta_año[año,:] ~ Normal(beta_año_jerárquico[año], sigma_año)
//
//y_clima los jerárquicos de meses/años tienen una estructura dinámica: 
//  beta_mes_jerárquico[mes] ~ Normal(beta_mes_jerárquico[mes - 1], sigma_jerárquico_mes)
//  beta_año_jerárquico[año] ~ Normal(beta_año_jerárquico[año - 1], sigma_jerárquico_año)
//
//con beta_mes_jerárquico[1] ~ Normal(0.0, sigma_jerárquico_mes) y_clima
//
//    beta_año_jerárquico[1] ~ Normal(0.0, sigma_jerárquico_año).
//
//El código tiene algunas transformaciones para mejorar la velocidad de convergencia
//en particular transformaciones de la normal de la forma:
//
//   beta = alpha + sigma*Z con Z ~ Normal(0.0, 1.0)
//
//para simular beta ~ Normal(alpha, sigma)
//
//-----------------------------------------------------------------------------
//
data {
  int<lower=0>  N_clima;                              //Número de datos
  int<lower=0>  N_predict;                            //Número de datos a predecir  
  int<lower=0, upper=12> N_meses_clima;               //Número de meses
  int<lower=0, upper=N_clima>  N_anios_clima;         //Número de años
  int<lower=0>  N_vars;                               //Número de datos (2)
  matrix[N_clima, N_vars]  y_clima;                   //Temperatura y_clima lluvia
  array[N_clima, 2] int<lower=0>  anio_mes_clima;           //Año y_clima mes
  array[N_predict, 2] int<lower=0>  anio_mes_predict; //Años y_clima meses a predecir
  int<lower=0>  N_anios_clima_predict;                //Número de años (distintos) a predecir posteriores al último
}

transformed data {
  matrix[N_clima, N_vars] y_clima_std;     //Standarized normal outcome variable 
  vector[N_vars]    mu_y_clima;      //Mean of each of the variables
  vector[N_vars]    sd_y_clima;      //Variance of each of the variables
  
  //Normalization of data in order to make all betas comparable
  for (varname in 1:N_vars){
    mu_y_clima[varname] = mean(y_clima[:,varname]);
    sd_y_clima[varname] = sd(y_clima[:,varname]);
    for (n in 1:N_clima)
      y_clima_std[n,varname] = (y_clima[n,varname] - mu_y_clima[varname]) / sd_y_clima[varname];
  }
}

parameters {
  
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
  
}

transformed parameters {
  
  vector[N_meses_clima]   beta_month_super_clima; //Hierarchical beta for months
  vector[N_anios_clima]   beta_year_super_clima;  //Hierarchical beta for years
  
  matrix[N_meses_clima, N_vars] beta_month_clima; //Matrix with monthly effects  
  matrix[N_anios_clima, N_vars] beta_year_clima;  //Matrix with yearly effects
  
  matrix[N_clima, N_vars] mu_clima;
  matrix[N_vars, N_vars] Sigma_clima;

  matrix[N_clima, N_vars] error_clima;             //Standarized normal outcome variable 
  
  //Prior covariance
  Sigma_clima = quad_form_diag(Omega_clima, tau_clima);
    
  //Prior for years
  beta_year_super_clima[1] =sigma_sq_year_prior_clima*Z_year_prior_clima[1];
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
  for (n in 1:N_clima)
    error_clima[n,:] = y_clima_std[n,:] - mu_clima[n,:];

}

model {
  
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
    error_clima[n,:] ~ multi_normal(rep_vector(0.0, N_vars), Sigma_clima);
}

generated quantities {

  matrix[N_vars, N_predict + N_clima] y_std_predict;
  matrix[N_predict + N_clima, N_vars] y_predict;
  matrix[N_predict + N_clima, N_vars] mu_predict;
  matrix[N_anios_clima + N_anios_clima_predict,  N_vars] beta_year_predict;  //Matrix with yearly effects
  vector[N_anios_clima + N_anios_clima_predict] beta_year_super_predict;     //Vector with global yearly effects
  
  //Simulate global yearly effect if new year
  for (year in 1:(N_anios_clima + N_anios_clima_predict)){
    if (year <= N_anios_clima)
      beta_year_super_predict[year] = beta_year_super_clima[year];
    else
      beta_year_super_predict[year]  = normal_rng(beta_year_super_predict[year - 1], sigma_sq_year_prior_clima);
  }

  //Simulate local year effect if new year
  for (year in 1:(N_anios_clima + N_anios_clima_predict)){
    if (year <= N_anios_clima)
      beta_year_predict[year,:] = beta_year_clima[year,:];
    else
      if (N_anios_clima_predict > 0){
        for (varname in 1:N_vars)
          beta_year_predict[year, varname] = normal_rng(beta_year_super_predict[year], sigma_sq_year_clima);
      }
  }

  //Recover data from normalization
  for (n in 1:(N_clima + N_predict)){
  
    if (n <= N_clima)
      mu_predict[n, :] = mu_clima[n, :];
    else
      mu_predict[n, :] = beta_year_predict[anio_mes_predict[n - N_clima, 1],:] + beta_month_clima[anio_mes_predict[n - N_clima, 2],:];

  }

  for (n in 1:(N_clima + N_predict)){      
    
    //Simulate prediction
    y_std_predict[:, n] = multi_normal_rng(mu_predict[n, :], Sigma_clima);

    for (varname in 1:N_vars)
      y_predict[n, varname]     = y_std_predict[varname, n] * sd_y_clima[varname] + mu_y_clima[varname];
    
  }
}
