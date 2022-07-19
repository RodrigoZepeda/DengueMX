#!/usr/bin/env Rscript

#Bayesian model
rm(list = ls())
pacman::p_load(clusterGeneration, cmdstanr, bayestestR, lubridate, 
               posterior, ggtext, glue, ggrepel, tidyverse)

N_dengue_predict <- 52*2 #Weeks to predict from now

#------------------------------------------------------------
#CLIMA
#------------------------------------------------------------

#Archivos con la info de precipitación y temperatura
clima_data  <- read_rds("datos-clima/processed/Clima_info.rds") %>%
  mutate(MES_NUM = as.numeric(MES_NUM)) %>%
  filter(ANIO >= 2010) %>%
  select(-ANUAL, -FECHA_PROXY, -MES) %>%
  filter(ENTIDAD == "NACIONAL") %>%
  select(-ENTIDAD) %>%
  pivot_wider(id_cols = c(ANIO, MES_NUM), names_from = VARIABLE, values_from = VALOR) %>%
  arrange(ANIO, MES_NUM) %>%
  filter(!is.nan(Precipitacion)) %>%
  mutate(Precipitacion = sqrt(Precipitacion)) %>%
  select(-starts_with("Temperatura"))

#Colocamos en el formato para saber asignar mes/día de temp
clima_vars <- clima_data %>%
  select(-ANIO, -MES_NUM) %>%
  as.matrix

anio_mes <- clima_data %>%
  select(ANIO, MES_NUM) %>%
  mutate(ANIO = ANIO - (min(ANIO) - 1)) %>%
  as.matrix

#------------------------------------------------------------
#DENGUE
#------------------------------------------------------------

#Archivo con la información de dengue la variable importante es nraw = casos de dengue reportados 
dengue_data               <- read_csv("datos-limpios/dengue_for_model_mx.csv")  

#Check pacf(sqrt(dengue_data$nraw)) for AR(p)
max_autocorrelation_order <- 6

#Create year, month and week variable
dengue_data <- dengue_data %>%
  mutate(year    = year(fecha)) %>%
  mutate(epiweek = epiweek(fecha)) %>%
  filter(year > 2015) %>%
  mutate(month   = month(fecha)) %>%
  mutate(normalized_year = year - min(year) + 1) %>%
  filter(fecha < max(fecha)) %>% #Drop last observation is always lower
  identity()

anio_mes_semana_dengue <- dengue_data %>%
  select(year, month, epiweek) %>%
  mutate(year = year - !!min(clima_data$ANIO) + 1) %>%
  as.matrix

#Generate prediction matrix for model 
anio_mes_semana_dengue_predict <- dengue_data %>%
  select(fecha, year, month, epiweek)

fecha_baseline <- max(anio_mes_semana_dengue_predict$fecha)

for (n in 1:N_dengue_predict){
  anio_mes_semana_dengue_predict <- anio_mes_semana_dengue_predict %>%
    bind_rows(
      tibble(fecha     = fecha_baseline + weeks(n)) %>%
        mutate(year    = year(fecha)) %>%
        mutate(month   = month(fecha)) %>%
        mutate(epiweek = epiweek(fecha))
    )
}

anio_mes_semana_dengue_predict_stan <- anio_mes_semana_dengue_predict %>%
  select(year, month, epiweek) %>%
  mutate(year = year - !!min(clima_data$ANIO) + 1) %>%
  as.matrix

#------------------------------------------------------------
#MODELO
#------------------------------------------------------------

options(mc.cores = parallel::detectCores())
chains = 4; iter_warmup = 1000; nsim = 2000; pchains = 4; 
cpp_options  <- list(stan_threads = TRUE)

#Chequeo de que haya más warmup que nsim
if (nsim <= iter_warmup){
  stop("Nsim: Total de simulaciones debe ser mayor a iteraciones iter_warmup")
}

#Normalizamos la variable de dengue
mean_d <- mean(sqrt(dengue_data$nraw))
sd_d   <- sd(sqrt(dengue_data$nraw))

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
  dengue                 = (sqrt(dengue_data$nraw) - mean_d)/sd_d,
  anio_mes_semana_dengue = anio_mes_semana_dengue,
  
  #Hiperparámetros
  max_autorregresive_order_dengue = max_autocorrelation_order,
  
  #Predicción
  N_dengue_predict = N_dengue_predict,
  anio_mes_semana_dengue_predict = anio_mes_semana_dengue_predict_stan
  
) 

#Valores iniciales 
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

init_ll      <- lapply(1:chains, function(id) initf2(chain_id = id))

dengue_model <- cmdstan_model("scripts/model_bayes.stan", cpp_options = cpp_options)

if (!dir.exists("cmdstan")){dir.create("cmdstan")}
model_sample <- dengue_model$sample(data = datos, chains = chains, 
                                  seed = 436356, 
                                  iter_warmup = iter_warmup,
                                  adapt_delta = 0.95, 
                                  iter_sampling = nsim - iter_warmup,
                                  init = init_ll,
                                  max_treedepth = 2^(10),
                                  output_dir = tempdir(),                                  
                                  threads_per_chain = 4)

#------------------------------------------------------------
#GRÁFICO
#------------------------------------------------------------

#Chequeo de fecha posible del pico
transformed_cases <- model_sample$draws("mu_dengue_predict") %>% 
  as_draws_df() %>% 
  select(-starts_with(".")) %>% 
  t() %>%
  as_tibble() %>%
  bind_cols(anio_mes_semana_dengue_predict)

#Obtenemos la fecha del pico de cada uno de los años
max_chain <- transformed_cases %>% 
  pivot_longer(cols = starts_with("V")) %>%
  group_by(year, name) %>%
  slice(which.max(value)) %>%
  ungroup() %>%
  select(-name) %>%
  group_by(year) %>%
  summarise(mediana = median(fecha),
            lower   = quantile(fecha, 0.1, type = 1),
            upper   = quantile(fecha, 0.9, type = 1)) 

#Nos quedamos sólo con los picos futuros
max_chain <- max_chain %>%
  filter(year < max(year) & year >= max(dengue_data$year)) 

#Obtenemos los resultados del modelo
modelo_ajustado <- summarise_draws(model_sample$draws("dengue_predict"), 
                                   ~ quantile( ((.*sd_d) + mean_d)^2, probs = c(0.005, 0.025, 0.05, 0.1,
                                                           0.125, 0.25, 0.325,0.4, 0.5,
                                                           0.6, 0.675,0.75, 0.875, 0.9, 0.95, 
                                                           0.975, 0.995)))
prediction <- modelo_ajustado %>% 
  bind_cols(anio_mes_semana_dengue_predict) %>%
  left_join(dengue_data)

predplot <- ggplot(prediction) +
  geom_vline(aes(xintercept = mediana), data = max_chain, linetype = "dotted") +
  geom_ribbon(aes(x = fecha, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.1,
              fill = "#12757E") +
  geom_ribbon(aes(x = fecha, ymin = `10%`, ymax = `90%`), alpha = 0.15,
              fill = "#12757E") +
  geom_line(aes(x = fecha, y =  `50%`, color = "Predicho",
                linetype = "Predicho"), alpha = 0.5) +
  geom_point(aes(color = "Observado", linetype = "Observado",
                 x = fecha, y = nraw), alpha = 0.5) +
  #geom_line(aes(color = "red", size = color, y = model2)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  labs(
    x = "",
    y = "Casos probables",
    title = glue::glue("<span style = 'color:#92AF75;'>**Casos probables de dengue**</span> ",
                       "en México por fecha de inicio de síntomas"),
    caption = glue::glue("Elaborada el {today()} con datos hasta el {max(dengue_data$fecha)}<br>
                         **Fuente:** Datos Abiertos de la ", 
                         "Secretaría de Salud 2020-{year(today())} y ",
                         "Panoramas Epidemiológicos de Dengue 2017-2019."),
    subtitle = glue::glue("Modelo bayesiano de series de tiempo | **Github:** RodrigoZepeda/DengueMX")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        plot.subtitle    = element_markdown(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia semanal:", 
                     values = c("Observado" = "#92AF75", "Predicho" = "#12757E")) +
  scale_linetype_manual("Incidencia semanal:", 
                        values = c("Observado" = "solid", "Predicho" = "dashed")) +
  geom_label_repel(aes(x = mediana, y = 20000, 
                 label = glue("Fecha del pico: {mediana}\nentre {lower} y {upper}")), 
             data = max_chain, size = 2)
ggsave("images/Dengue_predict.pdf", predplot, width = 8, height = 5)
ggsave("images/Dengue_predict.png", predplot, width = 8, height = 5, dpi = 750, bg = "white")
