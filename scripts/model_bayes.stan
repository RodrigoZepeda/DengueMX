// Dengue model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  int<lower=1> N_predict;
  int<lower=0> max_autorregresive_order; //AR component
  int<lower=1> Nyears; //Number of years
  int<lower=0> Nyears_predict; //Number of years
  int<lower=1,upper=12> Nmonths; //Number of months
  int<lower=1,upper=53> Nweeks; //Number of epiweeks
  int<lower=1> epiweek[N]; //Week number
  int<lower=1> year[N]; //Year number
  int<lower=1> epiweek_predict[N_predict]; //Week number
  int<lower=1> year_predict[N_predict]; //Year number
  real<lower=0> log_cases[N]; //Number of cases per time moment N
}

transformed data {
  int<lower=0> year_observed_max = max(year);
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[max_autorregresive_order]  beta_AR;
  vector[max_autorregresive_order]  error_AR;
  
  //Mean parametrizations for centering
  vector[Nweeks] Z_week;
  vector[Nyears] Z_year;
  
  //Variance
  real<lower=0> sigma_sq;
  real<lower=0> sigma_week;
  real<lower=0> sigma_yr;
  real<lower=0> sigma_AR;
  real<lower=0> phi;
  
}

transformed parameters {
  real mu[N];
  vector[Nweeks]  beta_week;
  vector[Nyears]  beta_year;
  
  beta_week[1] = sigma_week*Z_week[1];
  if (Nweeks > 1){
    for (week in 2:Nweeks){
      beta_week[week] = beta_week[week - 1] + sigma_week*Z_week[week];
    }
  }
  
  beta_year[1] = sigma_yr*Z_year[1];
  if (Nyears > 1){
    for (yr in 2:Nyears){
      beta_year[yr] = beta_year[yr - 1] + sigma_yr*Z_year[yr];
    }
  }
  
  
  for (n in 1:N){
    mu[n] = beta_week[epiweek[n]] + beta_year[year[n]];
    
    //Add AR to model
    if (max_autorregresive_order > 0){
      //Loop through AR
      for (r in 1:max_autorregresive_order){
        if (n - r > 0){
          mu[n] += beta_AR[r]*log_cases[n - r];
        } else {
          mu[n] += error_AR[r];
        }
      }
    }
  }
}

model {
  sigma_sq   ~ cauchy(0, 2.5);
  sigma_week ~ normal(0, sigma_sq);
  sigma_yr   ~ normal(0, sigma_sq);
  sigma_AR   ~ cauchy(0, 2.5);
  error_AR   ~ std_normal();
  phi        ~ cauchy(0, 2.5);
  Z_week     ~ std_normal();
  Z_year     ~ std_normal();
  
  if (max_autorregresive_order > 1){
    for (r in 1:max_autorregresive_order){
      beta_AR[r] ~ normal(0, sigma_AR);
    }
  }
  
  for (n in 1:N)
    log_cases[n] ~ normal(log(100.0)*mu[n], phi);
}

generated quantities {
  vector[N + N_predict] mu_predict;
  vector[Nyears + Nyears_predict] beta_year_predict;
  real log_cases_predict[N + N_predict];
  
  int<lower=1> epiweek_predict_complete[N + N_predict]; //Week number
  int<lower=1> year_predict_complete[N + N_predict]; //Year number
  
  //Simulate beta for years
  for (yr in 1:(Nyears + Nyears_predict)){
    if (yr > year_observed_max){
      beta_year_predict[yr] = normal_rng(beta_year_predict[yr - 1], sigma_yr);
    } else {
      beta_year_predict[yr] = beta_year[yr];
    }
  }
  
  for (n in 1:(N + N_predict)){
    
    //Update either to previous or new
    if (n > N){ 
      year_predict_complete[n]    =  year_predict[n - N];
      epiweek_predict_complete[n] =  epiweek_predict[n - N];  
      
      mu_predict[n] = beta_week[epiweek_predict_complete[n]] + 
        beta_year_predict[year_predict_complete[n]];
        
      for (r in 1:max_autorregresive_order){
        if (n - r > N){
          mu_predict[n] += beta_AR[r]*log_cases_predict[n - r];
        } else {
          mu_predict[n] += beta_AR[r]*log_cases[n - r];
        }
      }  
        
    } else {
      year_predict_complete[n]    =  year[n];
      epiweek_predict_complete[n] =  epiweek[n];    
      mu_predict[n] = mu[n];
    }
    
    log_cases_predict[n] = normal_rng(log(100.0)*mu_predict[n], phi);
    
  }
}