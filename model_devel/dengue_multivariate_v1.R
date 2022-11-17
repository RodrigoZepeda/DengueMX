#!/usr/bin/env Rscript

#Bayesian model
rm(list = ls())
pacman::p_load(clusterGeneration, cmdstanr, bayestestR, lubridate, 
               posterior, ggtext, glue, ggrepel, tidyverse, cli, clusterGeneration)


#DENGUE
#------------------------------------------------------------

#Check pacf(sqrt(dengue_data$nraw)) for AR(p)
arma_p <- 6
arma_q <- 3

#Create year, month and week variable
dengue_data <- read_csv("datos-limpios/dengue_2016_2022_mx.csv", show_col_types = FALSE)  |>
  dplyr::select(Estado, n, fecha) |>
  filter(year(fecha) > 2015) |>
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
  mutate(epiweek = epiweek(fecha)) |>
  filter(Estado %in% c("BAJA CALIFORNIA SUR","CAMPECHE", "CHIAPAS",
                       #"COAHUILA","COLIMA","GUERRERO",
                       #"JALISCO","MICHOACÁN","MORELOS","NAYARIT","NUEVO LEÓN","OAXACA",
                       #"PUEBLA","QUINTANA ROO","SINALOA","TABASCO","TAMAULIPAS", "VERACRUZ",
                       "YUCATÁN")) |>
  identity()

year_week_dengue_input <- dengue_data |>
  dplyr::select(year, epiweek) |>
  distinct() |>
  as.matrix()

dengue_data <- dengue_data |>
  bind_rows(
    dengue_data |> group_by(fecha, epiweek, year) |>
      summarise(n = sum(n), .groups = "drop") |>
      mutate(Estado = "ZZZ_TOTAL")
  ) 

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
chains = 4; iter_warmup = 500; nsim = 1000; pchains = 4; 
cpp_options  <- list(stan_threads = TRUE)

#Chequeo de que haya más warmup que nsim
if (nsim <= iter_warmup){
  cli_abort("Total de simulaciones {.code nsim} debe ser mayor a iteraciones {.code iter_warmup}")
}

datos  <- list(
  #Variables de dengue
  N_dengue               = nrow(dengue_cases),
  N_states               = ncol(dengue_cases),
  N_years                = length(unique(dengue_data$year)),
  N_weeks                = length(unique(dengue_data$epiweek)),
  year_week_dengue_input = year_week_dengue_input,
  dengue                 = dengue_cases,
  transform_dengue       = 7,
  lambda_boxcox          = 0,
  
  #Hiperparámetros
  arma_p = arma_p,
  arma_q = arma_q,
  
  #Predicción
  N_dengue_predict = 0,
  year_week_dengue_predict_input = year_week_dengue_input
  
) 

dengue_model <- cmdstan_model("model_devel/model_multivariate_v1.stan", cpp_options = cpp_options)

#Valores iniciales 
initf2 <- function(chain_id = 1) {
  list(tau                   = abs(rnorm(datos$N_states)),
       Omega                 = rcorrmatrix(datos$N_states)
  )
}
init_ll      <- lapply(1:chains, function(id) initf2(chain_id = id))

t0 <- Sys.time()
model_sample <- dengue_model$sample(data = datos, chains = chains, 
                                    seed = 87934, 
                                    iter_warmup = iter_warmup,
                                    adapt_delta = 0.95, 
                                    init = init_ll,
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
    dengue_data |> 
      dplyr::select(fecha) |>
      arrange(fecha)|>
      distinct() |>
      mutate(nval = 1:n()),
    by = "nval"
  )

ggplot() +
  geom_ribbon(aes(x = fecha, ymin = q5, ymax = q95, fill = Estado), alpha = 0.25, data = df) +
  geom_line(aes(y = mean, x = fecha, color = Estado), alpha = 0.5, data = df) +
  geom_point(aes(x = fecha, y = n, color = Estado), data = dengue_data) +
  geom_point(aes(x = fecha, y = n), color = "white", size = 0.5, data = dengue_data) +
  facet_wrap(~ Estado, scales = "free_y") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma)

epsilon <- model_sample$summary("epsilon")
