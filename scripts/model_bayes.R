#Bayesian model
pacman::p_load(tidyverse, cmdstanr, bayestestR, lubridate, posterior)

dengue_data <- read_csv("datos-limpios/dengue_for_model_mx.csv")  %>%
  filter(year > 2015)

#Create year, month and week variable
dengue_data <- dengue_data %>%
  mutate(year    = year(fecha)) %>%
  mutate(epiweek = epiweek(fecha)) %>%
  mutate(month   = month(fecha)) %>%
  mutate(normalized_year = year - min(year) + 1)

options(mc.cores = parallel::detectCores())

predict_df <- tibble(fecha = seq(max(dengue_data$fecha) + weeks(1),
                    max(dengue_data$fecha) + weeks(52),
                    by = "1 week"))

predict_df <- predict_df %>%
  mutate(year = year(fecha)) %>%
  mutate(epiweek = epiweek(fecha)) %>%
  mutate(month = month(fecha)) %>%
  mutate(normalized_year = year - min(dengue_data$year) + 1)

chains = 1; iter_warmup = 500; nsim = 1000; pchains = 4; 
datos  <- list(
  N         = nrow(dengue_data),
  N_predict = nrow(predict_df),
  Nyears   = length(unique(dengue_data$normalized_year)),
  Nyears_predict = length(setdiff(predict_df$year, dengue_data$year)),
  Nmonths  = length(unique(dengue_data$month)),
  Nweeks   = length(unique(dengue_data$epiweek)),
  epiweek  = dengue_data$epiweek,
  epiweek_predict = predict_df$epiweek,
  month           = dengue_data$month,
  month_predict   = predict_df$month,
  year            = dengue_data$normalized_year,
  year_predict    = predict_df$normalized_year,
  log_cases        = dengue_data$log_nraw
) 

# function form 2 with an argument named `chain_id`
initf2 <- function(chain_id = 1) {
  list(beta_week    = rnorm(datos$Nweeks),
       beta_year    = rnorm(datos$Nyears),
       beta_month   = rnorm(datos$Nmonths),
       y_err        = runif(datos$N, -0.5, 0.5),
       mu           = rnorm(datos$N),
       sigma_sq     = abs(rnorm(datos$N))
  )}



# generate a list of lists to specify initial values
init_ll <- lapply(1:chains, function(id) initf2(chain_id = id))

cpp_options <- list(cxx_flags = "-O3 -march=native", stan_threads = TRUE)

dengue_model <- cmdstan_model("scripts/model_bayes.stan", cpp_options = cpp_options)

if (!dir.exists("cmdstan")){dir.create("cmdstan")}
model_sample <- dengue_model$sample(data = datos, chains = chains, 
                                  seed = 436356, 
                                  iter_warmup = iter_warmup,
                                  adapt_delta = 0.95, 
                                  iter_sampling = nsim - iter_warmup,
                                  init = init_ll,
                                  max_treedepth = 2^(11),
                                  output_dir = "cmdstan",                                  
                                  threads_per_chain = 4)


modelo_ajustado <- summarise_draws(model_sample$draws("log_cases_predict"), 
                                   ~ quantile(., probs = c(0.005, 0.025, 0.05, 
                                                           0.125, 0.25, 0.325,0.4, 0.5,
                                                           0.6, 0.675,0.75, 0.875, 0.95, 
                                                           0.975, 0.995)))
prediction <- modelo_ajustado %>% bind_cols(bind_rows(dengue_data,predict_df))

prediction %>%
  filter(year(fecha) <= 2022) %>%
ggplot(aes(x = fecha)) +
  geom_line(aes(y = exp(`50%`)), color = "firebrick") +
  #geom_line(aes(y = n), color = "black") +
  geom_ribbon(aes(ymin = exp(`2.5%`), ymax = exp(`97.5%`)), alpha = 0.1, fill = "firebrick") +
  geom_point(aes(y = n.value), data = dengue_data) +
  theme_classic()
