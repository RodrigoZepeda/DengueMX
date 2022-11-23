functions {
   vector scaler(vector y, int transform, real lambda){
     //Function that obtains y_transformed = scaler(y)
     //for some of the most common transformations for time series
     //Note: inv_scaler(scaler(y)) = y
     //List of transformations
     //0) None, 
     //1) Logarithm, ln(y)
     //2) Sqrt, sqrt(y)
     //3) BoxCox (user supplied lambda), (y^(lambda) - 1)/lambda
     //4) MinMax scaling (y - min(y))/(max(y) - min(y))
     //5) Normalization, (y - mean(y))/sd(y)
     //6) Normalization of logarithm (log(y) - mean(log(y)))/sd(log(y))
     //7) Normalization of sqrt (sqrt(y) - mean(sqrt(y)))/sd(sqrt(y))
     
     vector[num_elements(y)] y_transformed;
     //Case log, normalized log and box cox with lambda = 0
     if (transform == 1 || transform == 6 || (transform == 3 && lambda == 0)){
       y_transformed = log(y + 1);
     //Case sqrt or normalized sqrt
     } else if (transform == 2 || transform == 7){
       y_transformed = sqrt(y);
     //BoxCox
     } else if (transform == 3 && lambda != 0){
       for (n in 1:num_elements(y)){
         y_transformed[n] = (pow(y[n], lambda) - 1)/lambda;
       }
     //MinMax Scaling
     } else if (transform == 4){
       real miny = min(y);
       real maxy = max(y);
       for (n in 1:num_elements(y)){
         y_transformed[n] = (y[n] - miny)/(maxy - miny);
       }
     //Else set identity
     } else {
       y_transformed = y;
     }
     
     //Normalization
     if (transform == 5 || transform == 6 || transform == 7){
        real mu    = mean(y_transformed);
        real sigma = sd(y_transformed);
        for (n in 1:num_elements(y_transformed)){
          y_transformed[n] = (y_transformed[n] - mu) / sigma;
        }
     }
     
     return y_transformed;
   }
   
   vector inv_scaler(vector y_transformed, int transform, real lambda, vector y_original){
     //Inverse of the scaler obtains y = inv_scaler(y_transformed)
     //for some of the most common transformations for time series
     //Note: inv_scaler(scaler(y)) = y
     //List of transformations
     //0) None, 
     //1) Logarithm, ln(y)
     //2) Sqrt, sqrt(y)
     //3) BoxCox (user supplied lambda), (y^(lambda) - 1)/lambda
     //4) MinMax scaling (y - min(y))/(max(y) - min(y))
     //5) Normalization, (y - mean(y))/sd(y)
     //6) Normalization of logarithm (log(y) - mean(log(y)))/sd(log(y))
     //7) Normalization of sqrt (sqrt(y) - mean(sqrt(y)))/sd(sqrt(y))
     
     vector[num_elements(y_transformed)] y;
     vector[num_elements(y_original)] y_original_transformed;
     
     //Pre transform for mean
     if (transform == 7){
       y_original_transformed = sqrt(y_original);
     } else if (transform == 6){
       y_original_transformed = log(y_original + 1);
     } else {
       y_original_transformed = y_original;
     }
     
     //Normalization
     if (transform == 5 || transform == 6 || transform == 7){
        real mu    = mean(y_original_transformed);
        real sigma = sd(y_original_transformed);
        for (n in 1:num_elements(y_transformed)){
          y[n] = sigma*y_transformed[n] + mu;
        }
     } else {
       y = y_transformed;
     }
     
     //Case log, normalized log and box cox with lambda = 0
     if (transform == 1 || transform == 6 || (transform == 3 && lambda == 0)){
       y = exp(y) - 1;
     //Case sqrt or normalized sqrt
     } else if (transform == 2 || transform == 7){
       for (n in 1:num_elements(y)){
         y[n] = pow(y[n], 2.0);
       }
     //BoxCox
     } else if (transform == 3 && lambda != 0){
       for (n in 1:num_elements(y)){
         y[n] = pow(lambda*y[n] + 1, inv(lambda));
       }
     //MinMax Scaling
     } else if (transform == 4){
       real maxy = max(y);
       real miny = min(y);
       for (n in 1:num_elements(y)){
         y[n] = y[n]*(maxy - miny) + miny;
       }
     } 
     
     return y;
   }
}

data {
  //Global parameters
  //---------------------------------------------
  int<lower=0> N_states;  //Number of States + National
  
  //Weather parameters
  //---------------------------------------------
  int<lower=0> N_weather; //Total number of weather (montly) observations
  int<lower=0,upper=12> N_months_weather; //Number of months in weather model
  int<lower=0> N_years_weather; //Number of years in weather model
  matrix[N_weather, N_states] y_weather; //Weather measurements
  array[N_weather, 2] int<lower=0> year_month_weather_input; //Year and month for weather variables
  
  //Dengue observed parametrs
  //---------------------------------------------
  int<lower=1> N_dengue;                 //Number of weekly dengue observations
  int<lower=0, upper=53> N_weeks_dengue; //Total number of epiweeks in model
  int<lower=0> N_years_dengue;           //Total number of years in model
  array[N_dengue, 3]  int<lower=1> year_month_week_dengue_input; //Year/month/week of registry
  matrix[N_dengue, N_states] dengue; //Dengue cases by state including national 
  
  //Data transformation
  //---------------------------------------------
  int<lower=0> transform_weather;  //Parameter to choose transformation for weather
  int<lower=0> transform_dengue;  //Parameter to choose transformation for dengue cases
  real<lower=-5,upper=5> lambda_boxcox_dengue; //Parameter if boxcox transform chosen for dengue
  real<lower=-5,upper=5> lambda_boxcox_weather; //Parameter if boxcox transform chosen for weather
  
  //Model hyperparameters
  //---------------------------------------------
  int<lower=0> arma_p;            //Autorregresive component_p
  real<lower=0> eta_lkj_dengue;   //Parameter for correlation  of LKJ(eta)
  real<lower=0> eta_lkj_weather;  //Parameter for correlation  of LKJ(eta)
  real<lower=0> sigma_dengue_alpha_hyperprior_variance;
  real<lower=0> sigma_dengue_year_hyperprior_variance;
  real<lower=0> sigma_dengue_week_hyperprior_variance;
  
  //Prediction parameters
  //---------------------------------------------
  int<lower=0> N_predict_dengue;
  int<lower=0> N_predict_weather;
  array[N_predict_dengue, 3] int<lower=1> year_month_week_dengue_predict; //Year/month/week of registry
  array[N_predict_weather, 2] int<lower=1> year_month_weather_predict; //Year/month/week of registry
}

transformed data {
  
  //Normalization of weather data
  //---------------------------------------------
  matrix[N_weather, N_states] y_weather_transformed;
  for (edo in 1:N_states){
    y_weather_transformed[:,edo] =  scaler(y_weather[:,edo], transform_weather, lambda_boxcox_weather);
  }
  
  //Columns for year month in weather
  //---------------------------------------------
  int <lower=1> col_year_w  = 1;
  int <lower=1> col_month_w = 2;
  
  //Columns for year month and week in dengue
  //---------------------------------------------
  int <lower=1> col_year_d  = 1;
  int <lower=1> col_month_d = 2;
  int <lower=1> col_week_d  = 3;
  
  //Transformed time series 
  //---------------------------------------------
  matrix[N_dengue, N_states] dengue_transformed;
  for (edo in 1:N_states){
    dengue_transformed[:,edo] =  scaler(dengue[:,edo], transform_dengue, lambda_boxcox_dengue);
  }
  
  //Normalization of years to start in 1
  //---------------------------------------------
  int<lower=0> min_year_dengue          = min(year_month_week_dengue_input[:, col_year_d]);
  int<lower=0> max_year_dengue          = max(year_month_week_dengue_input[:, col_year_d]);
  int<lower=0> max_year_dengue_predict  = max(year_month_week_dengue_predict[:, col_year_d]);
  int<lower=0> min_year_weather         = min(year_month_weather_input[:, col_year_w]);
  int<lower=0> max_year_weather         = max(year_month_weather_input[:, col_year_w]);
  int<lower=0> max_year_weather_predict = max(year_month_weather_predict[:, col_year_w]);
  int<lower=0> year_differential_d_w    = min_year_dengue - min_year_weather;
  
  //WEATHER:
  //Total years to predict from last observed
  int<lower=0> N_years_weather_predict = max_year_weather_predict - max_year_weather;
  array[N_weather + N_predict_weather, 2]  int<lower=1> year_month_weather;
  for (n in 1:(N_weather + N_predict_weather)){
    if (n <= N_weather){
      year_month_weather[n,:] =  year_month_weather_input[n,:];
    } else {
      year_month_weather[n,:] =  year_month_weather_predict[n - N_weather,:];
    }
  }
  
  //Finally, years starting in 1
  for (n in 1:(N_weather + N_predict_weather)){
    year_month_weather[n, col_year_w] = year_month_weather[n, col_year_w] - (min_year_weather - 1);
  }
  
  //DENGUE:
  //---------------------------------------------
  int<lower=0> N_years_dengue_predict = max_year_dengue_predict - max_year_dengue;
  array[N_dengue + N_predict_dengue, 3]  int<lower=1> year_month_week_dengue;
  for (n in 1:(N_dengue + N_predict_dengue)){
    if (n <= N_dengue){
      year_month_week_dengue[n,:] =  year_month_week_dengue_input[n,:];
    } else {
      year_month_week_dengue[n,:] =  year_month_week_dengue_predict[n - N_dengue,:];
    }
  }
  
  //Finally, years starting in 1
  for (n in 1:(N_dengue + N_predict_dengue)){
    year_month_week_dengue[n, col_year_d] = year_month_week_dengue[n, col_year_d] - (min_year_dengue - 1);
  }
  
  //Matrix of zeros for sampling (weather)
  //---------------------------------------------
  array[N_weather] vector[N_states] Zeros_weather;
  for (n in 1:N_weather){
    Zeros_weather[n] = rep_vector(0.0, N_states);
  }
  
  //Matrix of zeros for predicting (weather)
  //---------------------------------------------
  array[N_weather + N_predict_weather] vector[N_states] Zeros_weather_predict;
  for (n in 1:(N_weather + N_predict_weather)){
    Zeros_weather_predict[n] = rep_vector(0.0, N_states);
  }
  
  //Matrix of zeros for sampling (dengue)
  //---------------------------------------------
  array[N_dengue] vector[N_states] Zeros_dengue;
  for (n in 1:N_dengue){
    Zeros_dengue[n] = rep_vector(0.0, N_states);
  }
  
  //Matrix of zeros for predicting (dengue)
  //---------------------------------------------
  array[N_dengue + N_predict_dengue] vector[N_states] Zeros_dengue_predict;
  for (n in 1:(N_dengue + N_predict_dengue)){
    Zeros_dengue_predict[n] = rep_vector(0.0, N_states);
  }
  
  //Array of lags (for each state)
  //Creates the following matrix
  //-------------------------
  // 0        0     0      0    ...   0
  //y[1]      0     0      0    ...   0
  //y[2]    y[1]    0      0    ...   0
  //y[3]    y[2]   y[1]    0    ...   0
  // ...
  //y[n-1] y[n-2] y[n-3] y[n-4] ... y[n-p]
  //-------------------------
  array[N_states] matrix[N_dengue, arma_p] previous_dengue;
  for (edo in 1:N_states){
    for (n in 1:N_dengue){
      for (p in 1:arma_p){
        if (n - p <= 0){
          previous_dengue[edo, n, p] = 0.0;
        } else {
          previous_dengue[edo, n, p] = dengue_transformed[n - p, edo];
        }
      }
    }
  }
  
  //Month indices
  //--------------------------------------------
  int<lower=0> month_index[N_dengue];
  for (n in 1:N_dengue)
    month_index[n] = year_month_week_dengue[n, col_month_d]; 
    
  int<lower=0> month_index_predict[N_dengue + N_predict_dengue];
  for (n in 1:(N_dengue + N_predict_dengue))
    month_index_predict[n] = year_month_week_dengue[n, col_month_d]; 
  
}

parameters {
  //Variances
  real<lower=0,upper=pi()/2> sigma_alpha_state_hyper_unif;
  real<lower=0,upper=pi()/2> sigma_beta_weather_unif;
  real<lower=0,upper=pi()/2> sigma_beta_weather_state_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_year_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_week_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_year_state_hyper_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_week_state_hyper_unif;
  real<lower=0,upper=pi()/2> sigma_weather_year_unif;
  real<lower=0,upper=pi()/2> sigma_weather_month_unif;
  real<lower=0,upper=pi()/2> sigma_weather_year_state_unif;
  real<lower=0,upper=pi()/2> sigma_weather_month_state_unif;
  real<lower=0,upper=pi()/2> sigma_weather_alpha_unif;
  real<lower=0,upper=pi()/2> sigma_weather_alpha_state_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_alpha_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_AR_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_delta_unif;
  real<lower=0,upper=pi()/2> sigma_dengue_AR_state_unif;
  vector<lower=0,upper=pi()/2>[N_states] tau_dengue_unif;  // prior scale
  vector<lower=0,upper=pi()/2>[N_states] tau_weather_unif; // prior scale
  
  //See
  //https://mc-stan.org/docs/stan-users-guide/multivariate-hierarchical-priors.html
  cholesky_factor_corr[N_states] L_Omega_dengue;
  cholesky_factor_corr[N_states] L_Omega_weather;
  
  //Standarized version for faster inference
  real alpha_dengue_std;
  row_vector[N_states] alpha_dengue_state_std;
  row_vector[N_states] sigma_dengue_alpha_state_std;
  row_vector[N_states] sigma_dengue_year_state_std;
  row_vector[N_states] sigma_dengue_week_state_std;
  real alpha_weather_std;
  row_vector[N_states] alpha_weather_state_std;
  real beta_dengue_weather_std;
  row_vector[N_states] beta_dengue_weather_state_std;
  vector[N_years_dengue] beta_dengue_year_std;
  vector[N_weeks_dengue] beta_dengue_week_std;
  matrix[N_weeks_dengue, N_states] eta_dengue_std;
  matrix[N_years_dengue, N_states] beta_dengue_year_state_std;
  matrix[N_weeks_dengue, N_states] beta_dengue_week_state_std;
  vector[N_years_weather] beta_weather_year_std;
  vector[N_months_weather] beta_weather_month_std;
  matrix[N_years_weather, N_states] beta_weather_year_state_std;
  matrix[N_months_weather, N_states] beta_weather_month_state_std;
  vector[arma_p] beta_dengue_AR_std;
  matrix<lower=-1,upper=1>[arma_p, N_states]  beta_dengue_AR_state;
}

transformed parameters {
  matrix[N_dengue, N_states]  mu_dengue;
  matrix[N_weather, N_states] mu_weather;
  matrix[N_dengue, N_states]  mu_week_dengue;
  matrix[N_weeks_dengue, N_states] delta_week_state;

  //Cauchys simulated as uniform and then transformed
  real sigma_dengue_delta              = 2.5*tan(sigma_dengue_delta_unif);
  real sigma_alpha_state_hyper         = 2.5*tan(sigma_alpha_state_hyper_unif);
  real sigma_dengue_alpha              = 2.5*tan(sigma_dengue_alpha_unif);
  real sigma_beta_weather_state        = 2.5*tan(sigma_beta_weather_state_unif);
  real sigma_beta_weather              = 2.5*tan(sigma_beta_weather_unif);
  real sigma_weather_alpha             = 2.5*tan(sigma_weather_alpha_unif);
  real sigma_weather_alpha_state       = 2.5*tan(sigma_weather_alpha_state_unif);
  real sigma_dengue_year               = 2.5*tan(sigma_dengue_year_unif);
  real sigma_dengue_week               = 2.5*tan(sigma_dengue_week_unif);
  real sigma_dengue_year_state_hyper   = 2.5*tan(sigma_dengue_year_state_hyper_unif);
  real sigma_dengue_week_state_hyper   = 2.5*tan(sigma_dengue_week_state_hyper_unif);
  real sigma_weather_year              = 2.5*tan(sigma_weather_year_unif);
  real sigma_weather_month             = 2.5*tan(sigma_weather_month_unif);
  real sigma_weather_year_state        = 2.5*tan(sigma_weather_year_state_unif);
  real sigma_weather_month_state       = 2.5*tan(sigma_weather_month_state_unif);
  real sigma_dengue_AR                 = 2.5*tan(sigma_dengue_AR_unif);
  real sigma_dengue_AR_state           = 2.5*tan(sigma_dengue_AR_state_unif);
  vector[N_states] tau_dengue          = 2.5*tan(tau_dengue_unif);
  vector[N_states] tau_weather         = 2.5*tan(tau_weather_unif);
  
  //Un-standarized coefficients
  row_vector[N_states] beta_dengue_weather_state;
  row_vector[N_states] alpha_dengue_state;
  row_vector[N_states] alpha_weather_state;
  row_vector[N_states] sigma_dengue_alpha_state;
  vector[N_years_dengue]  beta_dengue_year;
  vector[N_weeks_dengue]  beta_dengue_week;
  row_vector[N_states] sigma_dengue_year_state;
  row_vector[N_states] sigma_dengue_week_state;
  matrix[N_years_dengue, N_states] beta_dengue_year_state;
  matrix[N_weeks_dengue, N_states] beta_dengue_week_state;
  vector[N_years_weather]   beta_weather_year;
  vector[N_months_weather]  beta_weather_month;
  matrix[N_years_weather, N_states] beta_weather_year_state;
  matrix[N_months_weather, N_states] beta_weather_month_state;
  vector[arma_p] beta_dengue_AR;
  array[N_dengue] row_vector [N_states]  epsilon_dengue;
  array[N_weather] row_vector [N_states] epsilon_weather;
  matrix[N_states, N_states] chol_Sigma_dengue  = diag_pre_multiply(tau_dengue, L_Omega_dengue);
  matrix[N_states, N_states] chol_Sigma_weather = diag_pre_multiply(tau_weather, L_Omega_weather);
  
  //Beta year and beta week from standarized
  real alpha_dengue  = sigma_dengue_alpha*alpha_dengue_std;
  real alpha_weather = sigma_weather_alpha*alpha_weather_std;
  real beta_dengue_weather = sigma_beta_weather*beta_dengue_weather_std;
  
  //Unstandarize variables 
  sigma_dengue_alpha_state  = rep_row_vector(sigma_alpha_state_hyper, N_states) +
        sigma_dengue_alpha_hyperprior_variance*sigma_dengue_alpha_state_std;
  sigma_dengue_year_state   = rep_row_vector(sigma_dengue_year_state_hyper, N_states) +
        sigma_dengue_year_hyperprior_variance*sigma_dengue_year_state_std;
  sigma_dengue_week_state   = rep_row_vector(sigma_dengue_week_state_hyper, N_states) +
        sigma_dengue_week_hyperprior_variance*sigma_dengue_week_state_std;
  alpha_dengue_state        = rep_row_vector(alpha_dengue, N_states)  + 
        sigma_dengue_alpha_state.*alpha_dengue_state_std;
  alpha_weather_state       = rep_row_vector(alpha_weather, N_states) + 
        sigma_weather_alpha_state*alpha_weather_state_std;
  beta_dengue_weather_state = rep_row_vector(beta_dengue_weather, N_states) + 
        sigma_beta_weather_state*beta_dengue_weather_state_std;
  
  //Dynamic priors on years (dengue)
  beta_dengue_year[1] = sigma_dengue_year*beta_dengue_year_std[1];
  for (yr in 2:N_years_dengue)
    beta_dengue_year[yr] = beta_dengue_year[yr - 1] + sigma_dengue_year*beta_dengue_year_std[yr];
  
  //Initial random noise for weeks
  delta_week_state[1,:] = sigma_dengue_delta*eta_dengue_std[1,:];
  for (wk in 2:N_weeks_dengue)
    delta_week_state[wk,:] = delta_week_state[wk - 1,:] + sigma_dengue_delta*eta_dengue_std[wk,:];
  
  //Dynamic priors on weeks (dengue)
  beta_dengue_week[1] = sigma_dengue_week*beta_dengue_week_std[1];
  for (wk in 2:N_weeks_dengue)
    beta_dengue_week[wk] = beta_dengue_week[wk - 1] + sigma_dengue_week*beta_dengue_week_std[wk];
  
  //Dynamic priors on years (weather)
  beta_weather_year[1] = sigma_weather_year*beta_weather_year_std[1];
  for (yr in 2:N_years_weather)
    beta_weather_year[yr] = beta_weather_year[yr - 1] + sigma_weather_year*beta_weather_year_std[yr];
    
  //Dynamic priors on months (weather)
  beta_weather_month[1] = sigma_weather_month*beta_weather_month_std[1];
  for (mth in 2:N_months_weather)
    beta_weather_month[mth] = beta_weather_month[mth - 1] + sigma_weather_month*beta_weather_month_std[mth];
    
  beta_dengue_AR        = sigma_dengue_AR*beta_dengue_AR_std;
  
  //Variables to state level
  //beta_dengue_AR_state     = rep_matrix(beta_dengue_AR, N_states)     + sigma_dengue_AR_state*beta_dengue_AR_state_std;
  beta_dengue_year_state   = rep_matrix(beta_dengue_year, N_states)   + rep_matrix(sigma_dengue_year_state, N_years_dengue).*beta_dengue_year_state_std; 
  beta_dengue_week_state   = delta_week_state + rep_matrix(beta_dengue_week, N_states)   + 
    rep_matrix(sigma_dengue_week_state, N_weeks_dengue).*beta_dengue_week_state_std;
  beta_weather_year_state  = rep_matrix(beta_weather_year, N_states)  + sigma_weather_year_state*beta_weather_year_state_std; 
  beta_weather_month_state = rep_matrix(beta_weather_month, N_states) + sigma_weather_month_state*beta_weather_month_state_std;
  
  mu_weather = rep_matrix(alpha_weather_state, N_weather) + 
      beta_weather_year_state[year_month_weather[1:N_weather, col_year_w], :] +
      beta_weather_month_state[year_month_weather[1:N_weather, col_month_w], :];
    
  for (edo in 1:N_states){
    mu_dengue[:, edo] = rep_vector(alpha_dengue_state[edo], N_dengue) + 
      beta_dengue_year_state[year_month_week_dengue[1:N_dengue, col_year_d], edo] +
      beta_dengue_week_state[year_month_week_dengue[1:N_dengue, col_week_d], edo] +
      beta_dengue_weather_state[edo]*beta_weather_month_state[month_index, edo] +
      previous_dengue[edo]*beta_dengue_AR_state[:, edo];
  }
  
  //Error terms for weather
  for (n in 1:N_weather)
    epsilon_weather[n] = y_weather_transformed[n,:] - mu_weather[n,:];
  
  //Error terms for dengue
  for (n in 1:N_dengue)
    epsilon_dengue[n] = dengue_transformed[n,:] - mu_dengue[n,:];
}

model {
  
  //Standarized parameters
  {
    alpha_dengue_std        ~ std_normal();
    alpha_dengue_state_std  ~ std_normal();
    alpha_weather_std       ~ std_normal();
    alpha_weather_state_std ~ std_normal();
    beta_dengue_year_std    ~ std_normal();
    beta_dengue_week_std    ~ std_normal();
    beta_weather_year_std   ~ std_normal();
    beta_weather_month_std  ~ std_normal();
    beta_dengue_AR_std      ~ std_normal();
    beta_dengue_weather_std ~ std_normal();
    sigma_dengue_alpha_state_std  ~ std_normal();
    beta_dengue_weather_state_std ~ std_normal();
    sigma_dengue_year_state_std   ~ std_normal();
    sigma_dengue_week_state_std   ~ std_normal();
    
    for (p in 1:arma_p)
      beta_dengue_AR_state[p,:]    ~ normal(beta_dengue_AR[p], sigma_dengue_AR_state);
      
    to_vector(beta_dengue_year_state_std)   ~ std_normal();
    to_vector(beta_dengue_week_state_std)   ~ std_normal();
    to_vector(beta_weather_year_state_std)  ~ std_normal();
    to_vector(beta_weather_month_state_std) ~ std_normal();
    to_vector(eta_dengue_std)               ~ std_normal();
    
    L_Omega_dengue  ~ lkj_corr_cholesky(eta_lkj_dengue);
    L_Omega_weather ~ lkj_corr_cholesky(eta_lkj_weather);
  }
  
  //Prior variance for SUG
  epsilon_dengue  ~ multi_normal_cholesky(Zeros_dengue,  chol_Sigma_dengue);
  epsilon_weather ~ multi_normal_cholesky(Zeros_weather, chol_Sigma_weather);

}

generated quantities {
  
  matrix[N_states,N_states] Omega_dengue = multiply_lower_tri_self_transpose(L_Omega_dengue);
  matrix[N_states,N_states] Sigma_dengue = quad_form_diag(Omega_dengue, tau_dengue);
  
  matrix[N_states,N_states] Omega_weather = multiply_lower_tri_self_transpose(L_Omega_weather);
  matrix[N_states,N_states] Sigma_weather = quad_form_diag(Omega_weather, tau_weather);
  
  matrix[N_weather + N_predict_weather, N_states]  mu_weather_predicted;
  matrix[N_weather + N_predict_weather, N_states]  weather_predicted;
  array[N_weather + N_predict_weather] vector [N_states] epsilon_weather_predict;
  
  matrix[N_dengue + N_predict_dengue, N_states]  mu_dengue_predicted;
  matrix[N_dengue + N_predict_dengue, N_states]  dengue_predicted;
  array[N_dengue + N_predict_dengue]  vector [N_states]  epsilon_dengue_predict;
  
  matrix[N_years_weather + N_years_weather_predict, N_states] beta_weather_year_state_predict;
  matrix[N_years_dengue  + N_years_dengue_predict, N_states] beta_dengue_year_state_predict;
  
  //Fill previous beta (dengue)
  for (yr in 1:(N_years_dengue + N_years_dengue_predict)){
    if (yr <= N_years_dengue){
      beta_dengue_year_state_predict[yr,:] = beta_dengue_year_state[yr,:];
    } else {
      beta_dengue_year_state_predict[yr,:] = beta_dengue_year_state_predict[yr - 1,:];
    }
  }
  
  //Fill previous beta (weather)
  for (yr in 1:(N_years_weather + N_years_weather_predict)){
    if (yr <= N_years_weather){
      beta_weather_year_state_predict[yr,:] = beta_weather_year_state[yr,:];
    } else {
      beta_weather_year_state_predict[yr,:] = beta_weather_year_state_predict[yr - 1,:];
    }
  }
  
  //Predict
  epsilon_dengue_predict  = multi_normal_cholesky_rng(Zeros_dengue_predict, chol_Sigma_dengue);
  epsilon_weather_predict = multi_normal_cholesky_rng(Zeros_weather_predict, chol_Sigma_weather);
  
  mu_weather_predicted = rep_matrix(alpha_weather_state, N_weather + N_predict_weather) + 
      beta_weather_year_state_predict[year_month_weather[:, col_year_w], :] +
      beta_weather_month_state[year_month_weather[:, col_month_w], :];
  
  for (n in 1:(N_weather + N_predict_weather))
    mu_weather_predicted[n,:] += to_row_vector(epsilon_weather_predict[n]);
    
  for (edo in 1:N_states){
    mu_dengue_predicted[:,edo] = rep_vector(alpha_dengue_state[edo], N_dengue + N_predict_dengue) + 
      beta_dengue_year_state_predict[year_month_week_dengue[:, col_year_d], edo] +
      beta_dengue_week_state[year_month_week_dengue[:, col_week_d], edo] +
      beta_dengue_weather_state[edo]*
      beta_weather_month_state[month_index_predict, edo];
  }
  
  for (n in 1:(N_dengue + N_predict_dengue)){
    
    mu_dengue_predicted[n,:] += to_row_vector(epsilon_dengue_predict[n]);
    
    //Add autorregresive terms
    for (r in 1:arma_p){
      if (n - r > 0 && n - r <= N_dengue){
        mu_dengue_predicted[n,:] += beta_dengue_AR_state[r,:].*dengue_transformed[n - r, :];
      } else if (n - r > 0 && n - r > N_dengue){
        mu_dengue_predicted[n,:] += beta_dengue_AR_state[r,:].*mu_dengue_predicted[n - r, :];
      }
    }
  }
  
  //Backtransform
  for (edo in 1:N_states){
    weather_predicted[:,edo] = inv_scaler(mu_weather_predicted[:,edo], transform_weather, lambda_boxcox_weather, y_weather[:,edo]);
    dengue_predicted[:,edo]  = inv_scaler(mu_dengue_predicted[:,edo], transform_dengue, lambda_boxcox_dengue, dengue[:,edo]);
  }
}
