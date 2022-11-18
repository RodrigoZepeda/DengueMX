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
  //Dengue observed parametrs
  //---------------------------------------------
  int<lower=1> N_dengue;          //Number of weekly dengue observations
  int<lower=0> N_states;          //Number of States + National
  int<lower=0, upper=53> N_weeks; //Total number of epiweeks in model
  int<lower=0> N_years;           //Total number of years in model
  array[N_dengue, 2]  int<lower=1> year_week_dengue_input; //Year/month/week of registry
  matrix[N_dengue, N_states] dengue; //Dengue cases by state including national 
  
  //Data transformation
  //---------------------------------------------
  int<lower=0> transform_dengue;  //Parameter to choose transformation for dengue cases
  real<lower=-5,upper=5> lambda_boxcox; //Parameter if boxcox transform chosen
  
  //Model hyperparameters
  //---------------------------------------------
  int<lower=0> arma_p;    //Autorregresive component_p
  
  //Prediction parameters
  //---------------------------------------------
  int<lower=0> N_predict;
  array[N_predict, 2] int<lower=1> year_week_dengue_predict; //Year/month/week of registry
}

transformed data {
  //Columns for year month and week in dengue
  //---------------------------------------------
  int <lower=1> col_year_d  = 1;
  int <lower=1> col_week_d  = 2;
  
  //Transformed time series 
  //---------------------------------------------
  matrix[N_dengue, N_states] dengue_transformed;
  for (edo in 1:N_states){
    dengue_transformed[:,edo] =  scaler(dengue[:,edo], transform_dengue, lambda_boxcox);
  }
  
  //Normalization of years to start in 1
  //---------------------------------------------
  int<lower=0> min_year_dengue         = min(year_week_dengue_input[:, col_year_d]);
  int<lower=0> max_year_dengue         = max(year_week_dengue_input[:, col_year_d]);
  int<lower=0> max_year_dengue_predict = max(year_week_dengue_predict[:, col_year_d]);
  
  //Total years to predict from last observed
  int<lower=0> N_years_predict = max_year_dengue_predict - max_year_dengue;
  array[N_dengue + N_predict, 2]  int<lower=1> year_week_dengue;
  for (n in 1:(N_dengue + N_predict)){
    if (n <= N_dengue){
      year_week_dengue[n,:] =  year_week_dengue_input[n,:];
    } else {
      year_week_dengue[n,:] =  year_week_dengue_predict[n - N_dengue,:];
    }
  }
  
  //Finally, years starting in 1
  for (n in 1:(N_dengue + N_predict)){
    year_week_dengue[n, col_year_d] = year_week_dengue[n, col_year_d] - (min_year_dengue - 1);
  }
  
  //Matrix of zeros for sampling
  //---------------------------------------------
  array[N_dengue] vector[N_states] Zeros;
  for (n in 1:N_dengue){
    Zeros[n] = rep_vector(0.0, N_states);
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
}

parameters {
  
  //Variances
  real<lower=0,upper=pi()/2> sigma_year_unif;
  real<lower=0,upper=pi()/2> sigma_week_unif;
  real<lower=0,upper=pi()/2> sigma_year_state_unif;
  real<lower=0,upper=pi()/2> sigma_week_state_unif;
  real<lower=0,upper=pi()/2> sigma_alpha_unif;
  real<lower=0,upper=pi()/2> sigma_AR_unif;
  real<lower=0,upper=pi()/2> sigma_AR_state_unif;
  real<lower=0,upper=pi()/2> sigma_alpha_state_unif;
  vector<lower=0,upper=pi()/2>[N_states] tau_unif; // prior scale
  
  //See
  //https://mc-stan.org/docs/stan-users-guide/multivariate-hierarchical-priors.html
  cholesky_factor_corr[N_states] L_Omega;
  
  //Standarized version for faster inference
  real alpha_std;
  row_vector[N_states] alpha_state_std;
  vector[N_years] beta_year_std;
  vector[N_weeks] beta_week_std;
  vector[arma_p] beta_AR_std;
  matrix[N_years, N_states] beta_year_state_std;
  matrix[N_weeks, N_states] beta_week_state_std;
  matrix[arma_p, N_states]  beta_AR_state_std;
}

transformed parameters {
  matrix[N_dengue, N_states]  mu_dengue;
  
  //Cauchys simulated as uniform and then transformed
  real sigma_year        = 2.5*tan(sigma_year_unif);
  real sigma_week        = 2.5*tan(sigma_week_unif);
  real sigma_year_state  = 2.5*tan(sigma_year_state_unif);
  real sigma_week_state  = 2.5*tan(sigma_week_state_unif);
  real sigma_alpha       = 2.5*tan(sigma_alpha_unif);
  real sigma_AR          = 2.5*tan(sigma_AR_unif);
  real sigma_AR_state    = 2.5*tan(sigma_AR_state_unif);
  real sigma_alpha_state = 2.5*tan(sigma_alpha_state_unif);
  vector[N_states] tau   = 2.5*tan(tau_unif);
  
  //Un-standarized coefficients
  row_vector[N_states] alpha_state;
  vector[N_years]  beta_year;
  vector[N_weeks]  beta_week;
  vector[arma_p]   beta_AR;
  matrix[N_years, N_states] beta_year_state;
  matrix[N_weeks, N_states] beta_week_state;
  matrix[arma_p, N_states]  beta_AR_state;
  array[N_dengue] row_vector [N_states] epsilon;
  matrix[N_states, N_states] chol_Sigma = diag_pre_multiply(tau, L_Omega);
  
  //Beta year and beta week from standarized
  real alpha     = sigma_alpha*alpha_std;
  
  //Unstandarize variables    
  alpha_state = rep_row_vector(alpha, N_states) + sigma_alpha_state*alpha_state_std;
  
  beta_AR   = sigma_AR*beta_AR_std;
  
  //Dynamic priors on weeks and years
  beta_year[1] = sigma_year*beta_year_std[1];
  for (yr in 2:N_years)
    beta_year[yr] = beta_year[yr - 1] + sigma_year*beta_year_std[yr];
  
  //Dynamic priors on weeks and years  
  beta_week[1] = sigma_week*beta_week_std[1];
  for (wk in 2:N_weeks)
    beta_week[wk] = beta_week[wk - 1] + sigma_week*beta_week_std[wk];
  
  beta_AR_state   = rep_matrix(beta_AR, N_states)   + sigma_AR_state*beta_AR_state_std;
  beta_year_state = rep_matrix(beta_year, N_states) + sigma_year_state*beta_year_state_std; 
  beta_week_state = rep_matrix(beta_week, N_states) + sigma_week_state*beta_week_state_std;
  
  for (edo in 1:N_states){
    mu_dengue[:, edo] = rep_vector(alpha_state[edo], N_dengue) + 
      beta_year_state[year_week_dengue[1:N_dengue, col_year_d], edo] +
      beta_week_state[year_week_dengue[1:N_dengue, col_week_d], edo] +
      previous_dengue[edo]*beta_AR_state[:, edo];
  }
  
  //Error term
  for (n in 1:N_dengue)
    epsilon[n] = dengue_transformed[n,:] - mu_dengue[n,:];
}

model {
  
  //Standarized parameters
  {
    beta_year_std   ~ std_normal();
    beta_week_std   ~ std_normal();
    beta_AR_std     ~ std_normal();
    alpha_std       ~ std_normal();
    alpha_state_std ~ std_normal();
    
    to_vector(beta_AR_state_std)   ~ std_normal();
    to_vector(beta_year_state_std) ~ std_normal();
    to_vector(beta_week_state_std) ~ std_normal();
    
    L_Omega ~ lkj_corr_cholesky(1);
  }
  
  //Prior variance for SUG
  epsilon ~ multi_normal_cholesky(Zeros, chol_Sigma);

}

generated quantities {
  
  matrix[N_states,N_states] Omega = multiply_lower_tri_self_transpose(L_Omega);
  matrix[N_states,N_states] Sigma = quad_form_diag(Omega, tau);
  
  matrix[N_dengue + N_predict, N_states]  mu_dengue_predicted;
  matrix[N_dengue + N_predict, N_states]  dengue_predicted;
  vector[N_dengue + N_predict]  dengue_predicted_sum;
  matrix[N_dengue + N_predict, N_states]  dengue_predicted_transformed;
  matrix[N_dengue + N_predict, N_states]  dengue_mean_predicted;
  matrix[N_years + N_years_predict, N_states] beta_year_state_predict;
  
  //Fill previous beta
  for (yr in 1:(N_years + N_years_predict)){
    if (yr <= N_years){
      beta_year_state_predict[yr,:] = beta_year_state[yr,:];
    } else {
      beta_year_state_predict[yr,:] = to_row_vector(normal_rng(beta_year_state_predict[yr - 1,:], sigma_year));
      beta_year_state_predict[yr,:] = to_row_vector(normal_rng(beta_year_state_predict[yr - 1,:], sigma_year_state));
    }
  }
  
  for (edo in 1:N_states){
    for (n in 1:(N_dengue + N_predict)){
      
      //Prediction
      mu_dengue_predicted[n,edo] = alpha_state[edo] + 
        beta_year_state_predict[year_week_dengue[n, col_year_d], edo] +
        beta_week_state[year_week_dengue[n, col_week_d], edo];
        
      //Add autorregresive terms
      for (r in 1:arma_p){
        if (n - r > 0 && n - r <= N_dengue){
          mu_dengue_predicted[n,edo] += beta_AR_state[r,edo]*dengue_transformed[n - r, edo];
        } else if (n - r > 0 && n - r > N_dengue){
          mu_dengue_predicted[n,edo] += beta_AR_state[r,edo]*mu_dengue_predicted[n - r, edo];
        }
      }
    }
  }
  
  //FIXME: There should be a way not to need this
  for (n in 1:(N_dengue + N_predict)){
    dengue_predicted_transformed[n,:] = to_row_vector(multi_normal_cholesky_rng(to_vector(mu_dengue_predicted[n,:]), chol_Sigma));
  }
  
  for (edo in 1:N_states){
    dengue_predicted[:,edo] = inv_scaler(dengue_predicted_transformed[:,edo], transform_dengue, lambda_boxcox, dengue[:,edo]);
    dengue_mean_predicted[:, edo] = inv_scaler(mu_dengue_predicted[:,edo], transform_dengue, lambda_boxcox, dengue[:,edo]);
  }
  
  for (n in 1:(N_dengue + N_predict)){
    dengue_predicted_sum[n] = sum(dengue_predicted[n,1:(N_states - 1)]);
  }
}
