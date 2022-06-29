//MODELO DE LAS LLUVIAS / TEMPERATURAS MENSUALES
//
//El modelo consiste en una serie de `Nvars` variables y[n,1], y[n,2], ..., y[n, Nvars]
//medidas de manera mensual en un periodo de tamaño `N`. Se supone dichas variables
//están correlacionadas por el año, mes y el error y representan precipitación y temperatura. 
//
//*MODELO*
//-----------------------------------------------------------------------------
//
//El modelo opera sobre las variables normalizadas:
//          y_std[n, var] = (y[n, var] - mean(y[:,var])) / sd(y[:, var])
//donde se asume que para cada n:
//          y_std[n, :] = beta_mes[ mes[n], :] + beta_año[ año[n],:] + error[n,:]
//donde los términos de error a lo largo de n están correlacionados.
//Escrito de otra forma el modelo es:
//          y_std[n, :] ~ NormalMulti(mu[n,:], Sigma)
//para mu[n,:] = beta_mes[ mes[n], :] + beta_año[ año[n],:].
//Suponemos una estructura jerárquica en el efecto de meses y años:
// beta_mes[mes,:] ~ Normal(beta_mes_jerárquico[mes], sigma_mes)
// beta_año[año,:] ~ Normal(beta_año_jerárquico[año], sigma_año)
//y los jerárquicos de meses/años tienen una estructura dinámica: 
//  beta_mes_jerárquico[mes] ~ Normal(beta_mes_jerárquico[mes - 1], sigma_jerárquico_mes)
//  beta_año_jerárquico[año] ~ Normal(beta_año_jerárquico[año - 1], sigma_jerárquico_año)
//con beta_mes_jerárquico[1] ~ Normal(0.0, sigma_jerárquico_mes) y
//beta_año_jerárquico[1] ~ Normal(0.0, sigma_jerárquico_año).
//El código tiene algunas transformaciones para mejorar la velocidad de convergencia
//en particular transformaciones de la normal de la forma:
//   beta = alpha + sigma*Z con Z ~ Normal(0.0, 1.0)
//para simular beta ~ Normal(alpha, sigma)
//-----------------------------------------------------------------------------
//
data {
  int<lower=0>  N;                    //Número de datos
  int<lower=0>  N_predict;            //Número de datos a predecir  
  int<lower=0>  N_months;             //Número de meses
  int<lower=0>  N_years;              //Número de años
  int<lower=0>  N_vars;               //Número de datos (2)
  matrix[N, N_vars]  y;               //Temperatura y lluvia
  array[N, 2] int<lower=0>  anio_mes; //Año y mes
  array[N_predict, 2] int<lower=0>  anio_mes_predict; //Años y meses a predecir
  int<lower=0>  
  N_years_predict;                      //Número de años (distintos) a predecir posteriores al último
}

transformed data {
  matrix[N, N_vars] y_std;     //Standarized normal outcome variable 
  vector[N_vars]    mu_y;      //Mean of each of the variables
  vector[N_vars]    sd_y;      //Variance of each of the variables
  
  //Normalization of data in order to make all betas comparable
  for (varname in 1:N_vars){
    mu_y[varname] = mean(y[:,varname]);
    sd_y[varname] = sd(y[:,varname]);
    for (n in 1:N)
      y_std[n,varname] = (y[n,varname] - mu_y[varname]) / sd_y[varname];
  }
}

parameters {
  
  matrix[N_months, N_vars] Z_beta_month; //Matrix with monthly effects
  matrix[N_years,  N_vars] Z_beta_year;  //Matrix with yearly effects
  
  corr_matrix[N_vars] Omega;         // prior correlation
  vector<lower=0>[N_vars] tau;       //Prior scale

  //Standard normals to improve the space
  vector[N_years]  Z_year_prior;
  vector[N_months] Z_month_prior;

  //Variances
  real<lower=0> sigma_sq_month;
  real<lower=0> sigma_sq_year;
  real<lower=0> sigma_sq_year_prior;
  real<lower=0> sigma_sq_month_prior;
  real<lower=0> sigma_sq_super;
  
}

transformed parameters {
  
  vector[N_months]   beta_month_super; //Hierarchical beta for months
  vector[N_years]    beta_year_super;  //Hierarchical beta for years
  
  matrix[N_months, N_vars] beta_month; //Matrix with monthly effects  
  matrix[N_years,  N_vars] beta_year;  //Matrix with yearly effects
  
  matrix[N, N_vars] mu;
  matrix[N_vars, N_vars] Sigma;

  matrix[N, N_vars] error;             //Standarized normal outcome variable 
  
  Sigma = quad_form_diag(Omega, tau);
    
  //Prior for years
  beta_year_super[1] =sigma_sq_year_prior*Z_year_prior[1];
  if (N_years > 1){
    for (year in 2:N_years)
      beta_year_super[year] = beta_year_super[year - 1] + sigma_sq_year_prior*Z_year_prior[year];
  }
  
  //Prior for months
  beta_month_super[1] =sigma_sq_month_prior*Z_month_prior[1];
  if (N_months > 1){
    for (month in 2:N_months)
      beta_month_super[month] = beta_month_super[month - 1] + sigma_sq_month_prior*Z_month_prior[month];
  }

  //Beta month year
  for (varname in 1:N_vars){
  
    for (year in 1:N_years)
      beta_year[year, varname]   = beta_year_super[year]   + sigma_sq_year*Z_beta_year[year, varname];
      
    for (month in 1:N_months)  
      beta_month[month, varname] = beta_month_super[month] + sigma_sq_month*Z_beta_month[month, varname];

  }

  //Mean
  for (n in 1:N)
    mu[n,:] = beta_year[anio_mes[n,1],:] + beta_month[anio_mes[n,2],:];
  
  //Error
  for (n in 1:N)
    error[n,:] = y_std[n,:] - mu[n,:];

}

model {
  
  //Variances
  sigma_sq_super        ~ cauchy(0, 2.5);
  sigma_sq_year_prior   ~ cauchy(0, sigma_sq_super);
  sigma_sq_month_prior  ~ cauchy(0, sigma_sq_super);
  sigma_sq_month        ~ cauchy(0, 2.5);
  sigma_sq_year         ~ cauchy(0, 2.5);
  
  //Prior variance for SUG
  tau   ~ cauchy(0, 2.5);
  Omega ~ lkj_corr(2);
  
  //For prior years
  Z_year_prior  ~ std_normal();
  Z_month_prior ~ std_normal();

  to_vector(Z_beta_year)  ~ std_normal();
  to_vector(Z_beta_month) ~ std_normal();
   
  for (n in 1:N)
    error[n,:] ~ multi_normal(rep_vector(0.0, N_vars), Sigma);
}

generated quantities {

  matrix[N_vars, N_predict + N] y_std_predict;
  matrix[N_predict + N, N_vars] y_predict;
  matrix[N_predict + N, N_vars] mu_predict;
  matrix[N_years + N_years_predict,  N_vars] beta_year_predict;  //Matrix with yearly effects
  vector[N_years + N_years_predict] beta_year_super_predict;     //Vector with global yearly effects
  
  //Simulate global yearly effect if new year
  for (year in 1:(N_years + N_years_predict)){
    if (year <= N_years)
      beta_year_super_predict[year] = beta_year_super[year];
    else
      beta_year_super_predict[year]  = normal_rng(beta_year_super_predict[year - 1], sigma_sq_year_prior);
  }

  //Simulate local year effect if new year
  for (year in 1:(N_years + N_years_predict)){
    if (year <= N_years)
      beta_year_predict[year,:] = beta_year[year,:];
    else
      if (N_years_predict > 0){
        for (varname in 1:N_vars)
          beta_year_predict[year, varname] = normal_rng(beta_year_super_predict[year], sigma_sq_year);
      }
  }

  //Recover data from normalization
  for (n in 1:(N + N_predict)){
  
    if (n <= N)
      mu_predict[n, :] = mu[n, :];
    else
      mu_predict[n, :] = beta_year_predict[anio_mes_predict[n - N, 1],:] + beta_month[anio_mes_predict[n - N, 2],:];

  }

  for (n in 1:(N + N_predict)){      
    
    //Simulate prediction
    y_std_predict[:, n] = multi_normal_rng(mu_predict[n, :], Sigma);

    for (varname in 1:N_vars)
      y_predict[n, varname]     = y_std_predict[varname, n] * sd_y[varname] + mu_y[varname];
    
  }
}
