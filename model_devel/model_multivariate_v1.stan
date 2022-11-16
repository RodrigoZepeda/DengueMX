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
  int<lower=0> N_states;         //Number of States + National
  int<lower=0, upper=53> N_weeks; //Total number of epiweeks in model
  int<lower=0> N_years;           //Total number of years in model
  array[N_dengue, 2] int<lower=1> year_week_dengue_input; //Year/month/week of registry
  matrix[N_dengue, N_states] dengue; //Dengue cases by state including national 
  int<lower=0> transform_dengue;  //Parameter to choose transformation for dengue cases
  real<lower=-5,upper=5> lambda_boxcox; //Parameter if boxcox transform chosen
  
  //Model hyperparameters
  //---------------------------------------------
  int<lower=0> arma_p;    //Autorregresive component_p
  int<lower=0> arma_q;    //Autorregresive component_q
  
  //Prediction parameters
  //---------------------------------------------
  int<lower=0> N_dengue_predict; //Number of weeks to predict
  array[N_dengue + N_dengue_predict, 2] int<lower=1> year_week_dengue_predict_input; //year/month/week of dengue for prediction model
}

transformed data {
  //Columns for year month and week in dengue
  int <lower=1> col_year_d  = 1;
  int <lower=1> col_week_d  = 2;
  
  //Transformed time series as well as min, max, sd, mean
  matrix[N_dengue, N_states] dengue_transformed;
  vector[N_states] mean_states;
  vector[N_states] sd_states;
  vector[N_states] min_states;
  vector[N_states] max_states;
  for (edo in 1:N_states){
    mean_states[edo] = mean(dengue[:,edo]);
    sd_states[edo]   = sd(dengue[:,edo]);
    min_states[edo]  = min(dengue[:,edo]);
    max_states[edo]  = max(dengue[:,edo]);
    dengue_transformed[:,edo] =  scaler(dengue[:,edo], transform_dengue, lambda_boxcox);
  }
  
  //Normalization of years to start in 1
  int<lower=0> min_year_dengue         = min(year_week_dengue_input[:, col_year_d]);
  int<lower=0> max_year_dengue         = max(year_week_dengue_input[:, col_year_d]);
  int<lower=0> max_year_dengue_predict = max(year_week_dengue_predict_input[:, col_year_d]);
  
  //Total years to predict from last observed
  int<lower=0> Nyears_predict = max_year_dengue_predict - max_year_dengue;
  
  array[N_dengue, 2] int<lower=1> year_week_dengue;
  year_week_dengue[:, col_week_d] =  year_week_dengue_input[:, col_week_d]; 
  for (n in 1:N_dengue){
    year_week_dengue[n, col_year_d] = year_week_dengue_input[n, col_year_d] - (min_year_dengue - 1);
  }
  
  array[N_dengue + N_dengue_predict, 2] int<lower=1> year_week_dengue_predict;
  year_week_dengue_predict[:, col_week_d] =  year_week_dengue_predict_input[:, col_week_d];
  for (n in 1:(N_dengue + N_dengue_predict)){
    year_week_dengue_predict[n, col_year_d] = year_week_dengue_predict_input[n, col_year_d] - (min_year_dengue - 1);
  }
}

parameters {
  
  //Standarized version for faster inference
  real alpha_std;
  vector[N_states] alpha_state_std;
  vector[N_years] beta_year_std;
  vector[N_weeks] beta_week_std;
  vector[arma_p] beta_AR_std;
  vector[arma_q] beta_MA_std;
  matrix[N_years, N_states] beta_year_state_std;
  matrix[N_weeks, N_states] beta_week_state_std;
  matrix[arma_p, N_states] beta_AR_state_std;
  matrix[arma_q, N_states] beta_MA_state_std;
  
  real<lower=0> sigma_year;
  real<lower=0> sigma_week;
  real<lower=0> sigma_year_state;
  real<lower=0> sigma_week_state;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_AR;
  real<lower=0> sigma_AR_state;
  real<lower=0> sigma_MA;
  real<lower=0> sigma_MA_state;
  real<lower=0> sigma_alpha_state;
  
  corr_matrix[N_states] Omega;   // prior correlation
  vector<lower=0>[N_states] tau; // prior scale

}

transformed parameters {
  matrix[N_dengue, N_states]  mu_dengue;
  
  //Un-standarized coefficients
  real alpha;
  vector[N_states] alpha_state;
  vector[N_years] beta_year;
  vector[N_weeks] beta_week;
  vector[arma_p] beta_AR;
  vector[arma_q] beta_MA;
  matrix[N_years, N_states] beta_year_state;
  matrix[N_weeks, N_states] beta_week_state;
  matrix[arma_p, N_states] beta_AR_state;
  matrix[arma_q, N_states] beta_MA_state;
  matrix[N_dengue, N_states] epsilon;
  
  //Beta year and beta week from standarized
  beta_year = sigma_year*beta_year_std;
  beta_week = sigma_week*beta_week_std;
  beta_AR   = sigma_AR*beta_AR_std;
  beta_MA   = sigma_MA*beta_MA_std;
  alpha     = sigma_alpha*alpha_std;
  
  for (edo in 1:N_states){
    
    //Unstandarize variables
    alpha_state[edo] = alpha + sigma_alpha_state*alpha_state_std[edo];
    
    beta_AR_state[:, edo]   = beta_AR   + sigma_AR_state*beta_AR_state_std[:, edo];
    beta_MA_state[:, edo]   = beta_MA   + sigma_MA_state*beta_MA_state_std[:, edo];
    beta_year_state[:, edo] = beta_year + sigma_year_state*beta_year_state_std[:, edo]; 
    beta_week_state[:, edo] = beta_week + sigma_week_state*beta_week_state_std[:, edo];
    
    for (n in 1:N_dengue){
      
      //Add time covariates
      mu_dengue[n,edo] = alpha_state[edo] + 
        beta_year_state[year_week_dengue[n, col_year_d], edo] + 
        beta_week_state[year_week_dengue[n, col_week_d], edo];
      
      //Add autorregresive terms
      for (r in 1:arma_p){
        if (n - r > 0){
          mu_dengue[n,edo] += beta_AR_state[r,edo]*dengue_transformed[n - r, edo];
        } 
      }
      
      //Add moving average
      for (r in 1:arma_q){
        if (n - r > 0){
          mu_dengue[n,edo] += beta_MA_state[r,edo]*epsilon[n - r, edo];
        }
      }
    }
    
    //Calculate the errors
    epsilon[:,edo] = dengue_transformed[:,edo] - mu_dengue[:,edo];
      
  }
  
  //Prior covariance
  matrix[N_states, N_states] Sigma = quad_form_diag(Omega, tau);

}

model {
  
  sigma_year  ~ cauchy(0.0, 2.5);
  sigma_week  ~ cauchy(0.0, 2.5);
  sigma_alpha ~ cauchy(0.0, 2.5);
  sigma_AR    ~ cauchy(0.0, 2.5);
  sigma_MA    ~ cauchy(0.0, 2.5);
  
  sigma_year_state  ~ cauchy(0.0, 2.5);
  sigma_week_state  ~ cauchy(0.0, 2.5);
  sigma_alpha_state ~ cauchy(0.0, 2.5);
  sigma_AR_state    ~ cauchy(0.0, 2.5);
  sigma_MA_state    ~ cauchy(0.0, 2.5);
  
  {
    beta_year_std   ~ std_normal();
    beta_week_std   ~ std_normal();
    beta_AR_std     ~ std_normal();
    beta_MA_std     ~ std_normal();
    alpha_std       ~ std_normal();
    alpha_state_std ~ std_normal();
    
    for (edo in 1:N_states){
      beta_AR_state_std[:, edo]   ~ std_normal();
      beta_MA_state_std[:, edo]   ~ std_normal();
      beta_year_state_std[:, edo] ~ std_normal();
      beta_week_state_std[:, edo] ~ std_normal();
    }
  }
  
  //Prior variance for SUG
  tau   ~ cauchy(0.0, 2.5);
  Omega ~ lkj_corr(2);

  //Model for dengue
  for (n in 1:N_dengue){
    epsilon[n,:] ~ multi_normal(rep_vector(0.0, N_states), Sigma);
  }
}

generated quantities {
  matrix[N_dengue, N_states]  dengue_predicted;
  matrix[N_dengue, N_states]  dengue_predicted_transformed;
  
  //FIXME: There should be a way not to need this
  for (n in 1:N_dengue){
    dengue_predicted_transformed[n,:] = to_row_vector(multi_normal_rng(to_vector(mu_dengue[n,:]), Sigma));
  }
  
  for (edo in 1:N_states){
    dengue_predicted[:,edo] = inv_scaler(dengue_predicted_transformed[:,edo], transform_dengue, lambda_boxcox, dengue[:,edo]);
  }
}
