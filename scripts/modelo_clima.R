#Bayesian model
rm(list = ls())
pacman::p_load(clusterGeneration, tidyverse, cmdstanr, bayestestR, lubridate, posterior, ggtext, glue)

clima_data  <- read_rds("datos-clima/processed/Clima_info.rds") %>%
  mutate(MES_NUM = as.numeric(MES_NUM)) %>%
  select(-ANUAL, -FECHA_PROXY, -MES) %>%
  filter(ENTIDAD == "NACIONAL") %>%
  select(-ENTIDAD) %>%
  pivot_wider(id_cols = c(ANIO, MES_NUM), names_from = VARIABLE, values_from = VALOR) %>%
  arrange(ANIO, MES_NUM) %>%
  filter(!is.nan(Precipitacion)) %>%
  mutate(Precipitacion = sqrt(Precipitacion))

anio_mes <- clima_data %>%
  select(ANIO, MES_NUM) %>%
  mutate(ANIO = ANIO - (min(ANIO) - 1)) %>%
  as.matrix()

clima_vars <- clima_data %>%
  select(-ANIO, -MES_NUM) %>%
  as.matrix()

options(mc.cores = parallel::detectCores())

#Months to predict
N_predict        <- 36

N_years_predict  <- 0
anio_mes_predict <- matrix(NA, ncol = 2, nrow = 0)
for (n in 1:N_predict){
  if (n == 1){
    año_baseline <- max(anio_mes[,1])
    mes_baseline <- anio_mes[nrow(anio_mes), 2]
  }
  
  if (mes_baseline == 12){
    mes_baseline    <- 1
    año_baseline    <- año_baseline + 1
    N_years_predict <- N_years_predict + 1
  } else {
    mes_baseline <- mes_baseline + 1
  }
  
  anio_mes_predict <- anio_mes_predict %>%
    rbind(matrix(c(año_baseline, mes_baseline), ncol = 2, nrow = 1))
}
  
chains = 1; iter_warmup = 100; nsim = 200; pchains = 1; 
datos  <- list(
  N         = nrow(clima_data),
  N_predict = N_predict,
  N_months  = length(unique(clima_data$MES_NUM)),
  N_years   = length(unique(clima_data$ANIO)),
  N_vars    = ncol(clima_vars),
  y         = clima_vars,
  anio_mes  = anio_mes,
  anio_mes_predict = anio_mes_predict,
  N_years_predict = N_years_predict
) 

# function form 2 with an argument named `chain_id`
initf2 <- function(chain_id = 1) {
  list(beta_month            = matrix(rnorm(datos$N_months*datos$N_vars), ncol = datos$N_vars),
       beta_year             = matrix(rnorm(datos$N_years*datos$N_vars), ncol = datos$N_vars),
       tau                   = abs(rnorm(datos$N_vars)),
       Omega                 = rcorrmatrix(datos$N_vars),
       Z_year_prior          = rnorm(datos$N_years),
       Z_month_prior         = rnorm(datos$N_months),
       sigma_sq_month        = abs(rnorm(1)),
       sigma_sq_year         = abs(rnorm(1)),
       sigma_sq_year_prior   = abs(rnorm(1)),
       sigma_sq_month_prior  = abs(rnorm(1)),
       sigma_sq_super        = abs(rnorm(1))
       )
}


# generate a list of lists to specify initial values
init_ll <- lapply(1:chains, function(id) initf2(chain_id = id))

cpp_options <- list(stan_threads = TRUE)

clima_model <- cmdstan_model("scripts/predice_clima.stan", cpp_options = cpp_options)

if (!dir.exists("cmdstan")){dir.create("cmdstan")}
model_sample <- clima_model$sample(data = datos, chains = chains, 
                                    seed = 436356, 
                                    iter_warmup = iter_warmup,
                                    adapt_delta = 0.95, 
                                    iter_sampling = nsim - iter_warmup,
                                    init = init_ll,
                                    max_treedepth = 2^(10),
                                    output_dir = "cmdstan",                                  
                                    threads_per_chain = 4)

#Get each chain simulation to check when does the maximum per year occur and give a CI

predictions <- model_sample$summary("y_predict", ~quantile(.x, probs = c(0.0, 0.025, 0.5, 0.95, 0.975))) %>%
  mutate(n   = as.numeric(str_remove_all(variable, "y_predict\\[|,[0-9]+\\]"))) %>%
  mutate(var = as.numeric(str_remove_all(variable, "y_predict\\[[0-9]+,|\\]"))) %>%
  left_join(as_tibble(anio_mes) %>% mutate(n = 1:n())) %>%
  left_join(tibble(varname = colnames(clima_vars)) %>% mutate(var = 1:n())) %>%
  dplyr::select(varname, ANIO, MES_NUM, `0%`, `2.5%`, `50%`, `95%`, `97.5%`, n) %>%
  mutate(ANIO = ANIO - 1 + min(clima_data$ANIO)) %>%
  full_join( 
    clima_data %>% 
    pivot_longer(cols = c(everything(), -ANIO, -MES_NUM), names_to = "varname", values_to = "obs")
  )

predictions %>%
  mutate(across(c(obs, `0%`:`97.5%`), ~if_else(varname == "Precipitacion", .^2, .))) %>%
  mutate(lower_ci = if_else(varname == "Precipitacion", `0%`, `2.5%`)) %>%
  mutate(upper_ci = if_else(varname == "Precipitacion", `95%`, `97.5%`)) %>%
  mutate(varname = case_when(
    varname == "Precipitacion"        ~ "Precipitación (mm)",
    varname == "Temperatura_Maxima"   ~ "Temperatura Máxima (ºC)",
    varname == "Temperatura_Promedio" ~ "Temperatura Promedio (ºC)",
    varname == "Temperatura_Minima"   ~ "Temperatura Mínima (ºC)",
    TRUE ~ varname
  )) %>%
  ggplot(aes(x = months(n - 1) + ymd("1985/01/01") )) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5) +
  geom_line(aes(y = `50%`), color = "black") +
  geom_point(aes(y = obs, color = varname)) +
  facet_wrap(~varname, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(
    strip.text    = element_text(size = 20),
    plot.caption  = element_markdown(),
    plot.subtitle = element_markdown(),
    plot.title    = element_markdown(size = 24),
    legend.position = "none"
  ) +
  labs(
    x = "",
    y = "",
    title    = "Proyección de variables climatológicas en **México**",
    subtitle = "_Modelo estructural bayesiano de series de tiempo_",
    caption  = glue("**Fuente:** Datos Abiertos + Reportes mensuales de CONAGUA |", 
               " **Github** RodrigoZepeda/DengueMX")
  ) +
  scale_color_manual(values = c("Precipitación (mm)" = "darkblue",
                                "Temperatura Máxima (ºC)" = "red",
                                "Temperatura Promedio (ºC)" = "red",
                                "Temperatura Mínima (ºC)" = "red"))
ggsave("datos-clima/climate_pred_model.pdf", width = 10, height = 10)

