#Bayesian model
rm(list = ls())
pacman::p_load(clusterGeneration, tidyverse, cmdstanr, bayestestR, lubridate, posterior, ggtext, glue)

dengue_data <- read_csv("datos-limpios/dengue_for_model_mx.csv")  

clima_data  <- read_rds("datos-clima/processed/Clima_info.rds") %>%
  mutate(MES_NUM = as.numeric(MES_NUM)) %>%
  select(-ANUAL, -FECHA_PROXY, -MES) %>%
  filter(ENTIDAD == "NACIONAL") %>%
  select(-ENTIDAD) %>%
  pivot_wider(id_cols = c(ANIO, MES_NUM), names_from = VARIABLE, values_from = VALOR) %>%
  arrange(ANIO, MES_NUM) %>%
  filter(!is.nan(Precipitacion)) %>%
  #FIXME THIS IS ONLY TO MAKE MODEL RUN FASTER FOR EXPERIMENTS
  filter(ANIO >= 2010) %>%
  select(-Temperatura_Maxima, -Temperatura_Promedio)

clima_vars <- clima_data %>%
  select(-ANIO, -MES_NUM) %>%
  as.matrix

anio_mes <- clima_data %>%
  select(ANIO, MES_NUM) %>%
  mutate(ANIO = ANIO - (min(ANIO) - 1)) %>%
  as.matrix

#Check pacf(dengue_data$log_nraw) for AR(p)
max_autocorrelation_order <- 5

#Create year, month and week variable
dengue_data <- dengue_data %>%
  mutate(year    = year(fecha)) %>%
  mutate(epiweek = epiweek(fecha)) %>%
  filter(year > 2015) %>%
  mutate(month   = month(fecha)) %>%
  mutate(normalized_year = year - min(year) + 1)

ano_mes_semana_dengue <- dengue_data %>%
  select(year, month, epiweek) %>%
  mutate(year = year - !!min(clima_data$ANIO) + 1) %>%
  as.matrix

options(mc.cores = parallel::detectCores())

chains = 1; iter_warmup = 100; nsim = 200; pchains = 1; 
datos  <- list(
  #Variables de clima
  N_clima         = nrow(clima_data),
  N_meses_clima   = length(unique(clima_data$MES_NUM)),
  N_anios_clima   = length(unique(clima_data$ANIO)),
  N_vars          = ncol(clima_vars),
  y_clima         = clima_vars,
  anio_mes_clima  = anio_mes,
  
  #Variables de dengue
  N_dengue               = nrow(dengue_data),
  N_anios_dengue         = length(unique(dengue_data$year)),
  N_semanas_dengue       = length(unique(dengue_data$epiweek)),
  dengue                 = dengue_data$log_nraw,
  anio_mes_semana_dengue = ano_mes_semana_dengue,
  
  #Hiperparámetros
  max_autorregresive_order_dengue = 5
) 

# function form 2 with an argument named `chain_id`
initf2 <- function(chain_id = 1) {
  list(tau_clima                   = abs(rnorm(datos$N_vars)),
       Omega_clima                 = rcorrmatrix(datos$N_vars),
       Z_year_prior_clima          = rnorm(datos$N_anios_clima),
       Z_month_prior_clima         = rnorm(datos$N_meses_clima),
       sigma_sq_month_clima        = abs(rnorm(1)),
       sigma_sq_year_clima         = abs(rnorm(1)),
       sigma_sq_year_prior_clima   = abs(rnorm(1)),
       sigma_sq_month_prior_clima  = abs(rnorm(1)),
       sigma_sq_super_clima        = abs(rnorm(1))
  )
}


# generate a list of lists to specify initial values
init_ll      <- lapply(1:chains, function(id) initf2(chain_id = id))

cpp_options  <- list(stan_threads = TRUE)

dengue_model <- cmdstan_model("scripts/model_bayes.stan", cpp_options = cpp_options)

if (!dir.exists("cmdstan")){dir.create("cmdstan")}
model_sample <- dengue_model$sample(data = datos, chains = chains, 
                                  seed = 436356, 
                                  iter_warmup = iter_warmup,
                                  adapt_delta = 0.95, 
                                  iter_sampling = nsim - iter_warmup,
                                  init = init_ll,
                                  max_treedepth = 2^10,
                                  output_dir = "cmdstan",                                  
                                  threads_per_chain = 4)

#Get each chain simulation to check when does the maximum per year occur and give a CI
los_cases       <- model_sample$summary("mu_dengue", ~ exp(log(100)*quantile(.x, probs = c(0.0, 0.025, 0.5, 0.95, 0.975)))) %>%
  mutate(weeknum = as.numeric(str_remove_all(variable, "mu_dengue\\[|\\]"))) %>%
  left_join(
    dengue_data %>% mutate(weeknum = 1:n())
  )

ggplot(los_cases, aes(x = weeks(weeknum - 1) + min(!!dengue_data$fecha))) +
  geom_point(aes(y = nraw), color = "black") +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.1, fill = "#12757E") +
  geom_line(aes(y = `50%`), color = "#12757E") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  labs(
    x = "",
    y = "Casos probables",
    title = glue::glue("<span style = 'color:#92AF75;'>Casos probables de dengue</span> ",
                       "en México por fecha de inicio de síntomas"),
    caption = glue::glue("Elaborada el {today()}.\nFuente: Datos Abiertos de la Secretaría de Salud 2020-{year(today())} y ",
                         "Panoramas Epidemiológicos de Dengue 2017-2019."),
    subtitle = glue::glue("Modelo bayesiano de series de tiempo | Github: RodrigoZepeda/DengueMX")
  ) 
ggsave("images/Dengue_predict.pdf", width = 8, height = 5)
ggsave("images/Dengue_predict.png", width = 8, height = 5, dpi = 750, bg = "white")


