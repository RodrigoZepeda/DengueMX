// Dengue model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  int<lower=1> N_predict;
  int<lower=1> Nyears; //Number of years
  int<lower=0> Nyears_predict; //Number of years
  int<lower=1,upper=12> Nmonths; //Number of months
  int<lower=1,upper=53> Nweeks; //Number of epiweeks
  int<lower=1> epiweek[N]; //Week number
  int<lower=1> year[N]; //Year number
  int<lower=1> epiweek_predict[N_predict]; //Week number
  int<lower=1> year_predict[N_predict]; //Year number
  int<lower=0> cases[N]; //Number of cases per time moment N
}

transformed data {
  int<lower=0> year_observed_max = max(year);
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[Nweeks]  beta_week;
  vector[Nyears]  beta_year;
  real<lower=0>   beta; 
  real<lower=0>   phi;
  
  //Variance
  real<lower=0> sigma_sq;
  real<lower=0> sigma_week;
  real<lower=0> sigma_yr;
  
}

transformed parameters {
  real mu[N];
  
  for (n in 1:N)
    mu[n] = beta_week[epiweek[n]] + beta_year[year[n]];
}

model {
  sigma_sq   ~ double_exponential(0, 1);
  sigma_week ~ double_exponential(0, sigma_sq);
  sigma_yr   ~ double_exponential(0, sigma_sq);
  phi        ~ double_exponential(0, 1);
  
  beta_week[1] ~ double_exponential(0, sigma_week);
  for (week in 2:Nweeks)
    beta_week[week]  ~ double_exponential(beta_week[week - 1], sigma_week);
  
  beta_year[1]  ~ double_exponential(0, sigma_yr);
  for (yr in 2:Nyears)  
    beta_year[yr]  ~ double_exponential(beta_year[yr - 1], sigma_yr);
  
  for (n in 1:N)
    cases[n] ~ neg_binomial_2_log(log(100.0)*mu[n], phi);
}

generated quantities {
  vector[N + N_predict] mu_predict;
  vector[Nyears + Nyears_predict] beta_year_predict;
  real cases_predict[N + N_predict];
  int<lower=1> epiweek_predict_complete[N + N_predict]; //Week number
  int<lower=1> year_predict_complete[N + N_predict]; //Year number
  
  //Simulate beta for years
  for (yr in 1:(Nyears + Nyears_predict)){
    if (yr > year_observed_max){
      beta_year_predict[yr] = double_exponential_rng(beta_year_predict[yr - 1], sigma_yr);
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
    } else {
      year_predict_complete[n]    =  year[n];
      epiweek_predict_complete[n] =  epiweek[n];    
      mu_predict[n] = mu[n];
    }
    
    cases_predict[n] = neg_binomial_2_log_rng(log(100.0)*mu_predict[n], phi);
    
  }
}




















