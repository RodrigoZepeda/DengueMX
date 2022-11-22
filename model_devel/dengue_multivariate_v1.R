#!/usr/bin/env Rscript

#Bayesian model
rm(list = ls())
pacman::p_load(clusterGeneration, cmdstanr, bayestestR, lubridate, 
               posterior, ggtext, glue, ggrepel, tidyverse, cli, clusterGeneration)
set.seed(348695)
days_to_predict <- 365
entidades <- c("BAJA CALIFORNIA SUR","CAMPECHE", "CHIAPAS", "NACIONAL",
               "COAHUILA","COLIMA","GUERRERO",
               "JALISCO","MICHOACÁN","MORELOS","NAYARIT","NUEVO LEÓN","OAXACA",
               "PUEBLA","QUINTANA ROO","SINALOA","TABASCO","TAMAULIPAS", "VERACRUZ",
               "YUCATÁN")
#CLIMA
#------------------------------------------------------------
#Archivos con la info de precipitación y temperatura
clima_data  <- read_rds("datos-clima/processed/Clima_info.rds") %>%
  mutate(MES_NUM = as.numeric(MES_NUM)) %>%
  filter(ANIO >= 2010) %>%
  filter(FECHA_PROXY <= ymd("2022/06/01")) |>
  filter(ENTIDAD %in% entidades) |>
  select(-ANUAL, -MES) %>%
  pivot_wider(id_cols = c(ENTIDAD, FECHA_PROXY, ANIO, MES_NUM), names_from = VARIABLE, values_from = VALOR) %>%
  arrange(FECHA_PROXY, ENTIDAD) %>%
  filter(!is.nan(Precipitacion)) %>%
  select(-starts_with("Temperatura")) 

#Colocamos en el formato para saber asignar mes/día de temp
y_weather <- clima_data %>%
  pivot_wider(names_from = ENTIDAD, values_from = Precipitacion) %>%
  arrange(FECHA_PROXY) |>
  select(-FECHA_PROXY, -ANIO, -MES_NUM) |>
  as.matrix()

year_month_weather_input <- clima_data %>%
  arrange(FECHA_PROXY) |>
  dplyr::select(ANIO, MES_NUM) %>%
  distinct() %>%
  as.matrix()

#FIXME check for repeated values
year_month_weather_predict <- tibble(
  fecha = seq(max(clima_data$FECHA_PROXY), 
              max(clima_data$FECHA_PROXY) + days(days_to_predict), by = "1 day")) |>
  mutate(year  = epiyear(fecha)) |>
  mutate(month = month(fecha)) |>
  dplyr::select(year, month) |>
  distinct() |>
  as.matrix()

#DENGUE
#------------------------------------------------------------

#Check pacf(sqrt(dengue_data$nraw)) for AR(p)
arma_p <- 6

#Create year, month and week variable
dengue_data <- read_csv("datos-limpios/dengue_2016_2022_mx.csv", show_col_types = FALSE)  |>
  dplyr::select(Estado, n, fecha) |>
  filter(year(fecha) > 2015) |>
  #filter(fecha <= ymd("2022/06/01")) |>
  filter(fecha < max(fecha)) |> #Drop last observation is always lower
  group_by(fecha, Estado) |>
  mutate(rep = 1:n()) |>
  ungroup() |>
  filter(rep == 1) |>
  identity() 

#Complete for states with 0 cases
dengue_data <- dengue_data |>
  full_join(
    tibble(fecha = unique(dengue_data$fecha)) |>
      expand_grid(Estado = unique(dengue_data$Estado)),  by = c("Estado", "fecha") 
  ) |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  arrange(fecha, Estado) |>
  mutate(year = epiyear(fecha)) |>
  mutate(epiweek = epiweek(fecha)) 

dengue_nal <- dengue_data |>
  group_by(fecha, year, epiweek) |>
  summarise(n = sum(n), .groups = "drop")

dengue_data <- dengue_data |>
  bind_rows(
    dengue_nal |>
      mutate(Estado = "NACIONAL") |>
      arrange(fecha)
  ) 

dengue_data <- dengue_data |>
  filter(Estado %in% entidades) |>
  identity()

year_week_dengue_input <- dengue_data |>
  mutate(month   = month(fecha)) |>
  dplyr::select(year, month, epiweek) |>
  distinct() |>
  as.matrix()

year_week_dengue_predict <- tibble(
  fecha = seq(max(dengue_data$fecha), 
              max(dengue_data$fecha) + days(days_to_predict), by = "1 day")) |>
  mutate(year    = epiyear(fecha)) |>
  mutate(month   = month(fecha)) |>
  mutate(epiweek = epiweek(fecha)) |>
  dplyr::select(year, month, epiweek) |>
  distinct() |>
  as.matrix()

dengue_cases <- dengue_data |>
  pivot_wider(id_cols = Estado, values_from = n, names_from = fecha) |>
  arrange(Estado) |>
  dplyr::select(-Estado) |>
  as.matrix() |>
  t()

#------------------------------------------------------------
#MODELO
#------------------------------------------------------------

options(mc.cores = max(parallel::detectCores() - 2, 1))
chains = 4; iter_warmup = 250; nsim = 500; pchains = 4; 
cpp_options  <- list(stan_threads = TRUE)

#Chequeo de que haya más warmup que nsim
if (nsim <= iter_warmup){
  cli_abort("Total de simulaciones {.code nsim} debe ser mayor a iteraciones {.code iter_warmup}")
}

datos  <- list(
  #Variables generales
  N_states                = ncol(dengue_cases),
  
  #Variables de clima
  N_weather                = nrow(y_weather),
  N_months_weather         = length(unique(clima_data$MES_NUM)),
  N_years_weather          = length(unique(clima_data$ANIO)),
  y_weather                = y_weather,
  year_month_weather_input = year_month_weather_input,
    
  #Variables de dengue
  N_dengue                     = nrow(dengue_cases),
  N_years_dengue               = length(unique(dengue_data$year)),
  N_weeks_dengue               = length(unique(dengue_data$epiweek)),
  year_month_week_dengue_input = year_week_dengue_input,
  dengue                       = dengue_cases,
  
  #Data transformation
  transform_weather      = 7,
  transform_dengue       = 7,
  lambda_boxcox_dengue   = 0,
  lambda_boxcox_weather  = 0,
  
  #Hiperparámetros
  arma_p          = arma_p,
  eta_lkj_dengue  = 2.0,
  eta_lkj_weather = 2.0,
  sigma_dengue_alpha_hyperprior_variance = 0.01,
  sigma_dengue_year_hyperprior_variance  = 0.01,
  sigma_dengue_week_hyperprior_variance  = 0.01,
    
  #Predicción
  N_predict_weather                = nrow(year_month_weather_predict),
  N_predict_dengue                 = nrow(year_week_dengue_predict),
  year_month_week_dengue_predict   = year_week_dengue_predict,
  year_month_weather_predict       = year_month_weather_predict
  
) 

dengue_model <- cmdstan_model("model_devel/model_multivariate_v1.stan", cpp_options = cpp_options)


t0 <- Sys.time()
model_sample <- dengue_model$sample(data = datos, chains = chains, 
                                    seed = 87934, 
                                    iter_warmup = iter_warmup,
                                    adapt_delta = 0.995, 
                                    init = 1,
                                    iter_sampling = nsim - iter_warmup,
                                    max_treedepth = 2^(10),
                                    output_dir = tempdir(),                                  
                                    threads_per_chain = 4)
t1 <- Sys.time() - t0
print(t1)

df <- model_sample$summary("dengue_predicted") |>
  mutate(state  = as.numeric(str_remove_all(variable, ".*\\[[0-9]+,|\\]"))) |>
  mutate(nval   = as.numeric(str_remove_all(variable, ".*\\[|,[0-9]+\\]"))) |>
  left_join(
    dengue_data |> 
      dplyr::select(Estado) |>
      arrange(Estado)|>
      distinct() |>
      mutate(state = 1:n()),
    by = "state"
  ) |>
  left_join(
    tibble(fecha = seq(min(dengue_data$fecha), 
                       max(dengue_data$fecha) + days(days_to_predict), by = "7 days")) |>
    mutate(nval = 1:n()),
    by = "nval"
  )

ggplot() +
  geom_ribbon(aes(x = fecha, ymin = q5, ymax = q95, fill = Estado), alpha = 0.25, data = df) +
  geom_line(aes(y = mean, x = fecha, color = Estado), alpha = 0.5, data = df) +
  geom_point(aes(x = fecha, y = n, color = Estado), data = dengue_data) +
  geom_point(aes(x = fecha, y = n), color = "white", size = 0.5, data = dengue_data) +
  facet_wrap(~ Estado, scales = "free_y", ncol = 4) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = NULL,
    x = NULL
  ) +
  coord_cartesian(xlim = c(max(df$fecha, na.rm = T) - years(4), 
                           max(df$fecha, na.rm = T)))


df <- model_sample$summary("weather_predicted") |>
  mutate(state  = as.numeric(str_remove_all(variable, ".*\\[[0-9]+,|\\]"))) |>
  mutate(nval   = as.numeric(str_remove_all(variable, ".*\\[|,[0-9]+\\]"))) |>
  left_join(
    clima_data |> 
      dplyr::select(ENTIDAD) |>
      arrange(ENTIDAD)|>
      distinct() |>
      mutate(state = 1:n()),
    by = "state"
  ) |>
  left_join(
    tibble(fecha = seq(min(clima_data$FECHA_PROXY), 
                       max(clima_data$FECHA_PROXY) + days(days_to_predict), by = "7 days")) |>
      mutate(MES_NUM  = month(fecha)) |>
      mutate(ANIO_NUM = epiyear(fecha)) |>
      dplyr::distinct(ANIO_NUM, MES_NUM, .keep_all = TRUE) |>
      mutate(nval = 1:n()),
    by = "nval"
  )

ggplot() +
  geom_ribbon(aes(x = fecha, ymin = q5, ymax = q95, fill = ENTIDAD), alpha = 0.25, data = df) +
  geom_line(aes(y = mean, x = fecha, color = ENTIDAD), alpha = 0.5, data = df) +
  geom_point(aes(x = FECHA_PROXY, y = Precipitacion, color = ENTIDAD), data = clima_data) +
  geom_point(aes(x = FECHA_PROXY, y = Precipitacion), color = "white", size = 0.5, data = clima_data) +
  facet_wrap(~ ENTIDAD, scales = "free_y", ncol = 4) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = NULL,
    x = NULL,
    title = "Precipitación predicha"
  )
