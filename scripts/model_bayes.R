#Bayesian model
rm(list = ls())
pacman::p_load(tidyverse, cmdstanr, bayestestR, lubridate, posterior, ggtext, glue)

dengue_data <- read_csv("datos-limpios/dengue_for_model_mx.csv")  

clima_data  <- read_rds("datos-clima/processed/Clima_info.rds") %>%
  mutate(MES_NUM = as.numeric(MES_NUM)) %>%
  filter(ANIO >= 2014) %>%
  select(-ANUAL, -FECHA_PROXY, -MES) %>%
  filter(ENTIDAD == "NACIONAL") %>%
  select(-ENTIDAD) %>%
  pivot_wider(id_cols = c(ANIO, MES_NUM), names_from = VARIABLE, values_from = VALOR) %>%
  arrange(ANIO, MES_NUM) %>%
  filter(!is.nan(Precipitacion))

clima_stan <- clima_data %>%
  select(-ANIO, -MES_NUM) %>%
  as.matrix

clima_fechas_stan <- clima_data %>%
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
  N_clima          = nrow(clima_stan),
  N_anios_clima    = length(unique(clima_fechas_stan[,1])),
  N_vars           = ncol(clima_stan),
  N_meses_clima    = length(unique(clima_fechas_stan[,2])),
  datos_clima      = clima_stan,
  anio_mes_clima   = clima_fechas_stan,
  
  #Qué tan después empiezan los casos de dengue resp a clima
  lag_dengue_clima = min(ano_mes_semana_dengue[,1]) - 1,
    
  #Variables de dengue
  N_dengue              = nrow(dengue_data),
  N_anios_dengue        = length(unique(dengue_data$year)),
  N_semanas_dengue      = length(unique(dengue_data$epiweek)),
  dengue                = dengue_data$log_nraw,
  anio_mes_semana_dengue = ano_mes_semana_dengue,
  
  #Parámetros de predicción
  N_predict = 2,
  anio_mes_semana_dengue_predict = ano_mes_semana_dengue[1:2,],
  
  #Hiperparámetros
  max_autorregresive_order_dengue = 5
) 

# function form 2 with an argument named `chain_id`
initf2 <- function(chain_id = 1) {
  list(beta_semana_dengue    = rnorm(datos$N_semanas_dengue),
       beta_anio_dengue     = rnorm(datos$N_anios_dengue),
       sigma_sq              = abs(rnorm(1)))
  }


# generate a list of lists to specify initial values
init_ll <- lapply(1:chains, function(id) initf2(chain_id = id))

cpp_options <- list(stan_threads = TRUE)

dengue_model <- cmdstan_model("scripts/model_bayes.stan", cpp_options = cpp_options)

if (!dir.exists("cmdstan")){dir.create("cmdstan")}
model_sample <- dengue_model$sample(data = datos, chains = chains, 
                                  seed = 436356, 
                                  iter_warmup = iter_warmup,
                                  adapt_delta = 0.99, 
                                  iter_sampling = nsim - iter_warmup,
                                  init = init_ll,
                                  max_treedepth = 2^(11),
                                  output_dir = "cmdstan",                                  
                                  threads_per_chain = 4)

#Get each chain simulation to check when does the maximum per year occur and give a CI
log_cases       <- model_sample$draws("mu_dengue") %>% 
  as_draws_df() %>% 
  select(-starts_with(".")) %>% 
  t() %>%
  bind_cols(bind_rows(dengue_data, predict_df)) %>% 
  select(-n.value, -n, -nraw, -t, -logn, -log_nraw)

max_chain <- log_cases %>% 
  pivot_longer(cols = starts_with("...")) %>%
  group_by(year, name) %>%
  slice(which.max(value)) %>%
  ungroup() %>%
  select(-name) %>%
  group_by(year) %>%
  summarise(mediana = median(fecha),
            lower   = quantile(fecha, 0.1, type = 1),
            upper   = quantile(fecha, 0.9, type = 1)) %>%
  filter(year < max(year) & year >= min(predict_df$year)) 

modelo_ajustado <- summarise_draws(model_sample$draws("log_cases_predict"), 
                                   ~ quantile(., probs = c(0.005, 0.025, 0.05, 0.1,
                                                           0.125, 0.25, 0.325,0.4, 0.5,
                                                           0.6, 0.675,0.75, 0.875, 0.9, 0.95, 
                                                           0.975, 0.995)))
prediction <- modelo_ajustado %>% 
  bind_cols(bind_rows(dengue_data, predict_df))


ggplot(prediction) +
  geom_vline(aes(xintercept = mediana), data = max_chain, linetype = "dotted") +
  geom_ribbon(aes(x = fecha, ymin = exp(`2.5%`), ymax = exp(`97.5%`)), alpha = 0.1,
              fill = "#12757E") +
  geom_ribbon(aes(x = fecha, ymin = exp(`10%`), ymax = exp(`90%`)), alpha = 0.15,
              fill = "#12757E") +
  geom_line(aes(x = fecha, y = exp(`50%`), color = "Predicho",
                linetype = "Predicho", size = "Predicho"), alpha = 0.5) +
  geom_point(aes(color = "Observado", linetype = "Observado", size = "Observado",
                 x = fecha, y = nraw), alpha = 0.5) +
  #geom_line(aes(color = "red", size = color, y = model2)) +
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
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        legend.position = "bottom",
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        plot.subtitle    = element_text(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia semanal:", 
                     values = c("Observado" = "#92AF75", "Predicho" = "#12757E")) +
  scale_linetype_manual("Incidencia semanal:", 
                        values = c("Observado" = "solid", "Predicho" = "dashed")) +
  scale_size_manual("Incidencia semanal:", 
                    values = c("Observado" = 1, "Predicho" = 0.5)) +
  geom_label(aes(x = mediana, y = 20000, 
                 label = glue("Fecha del pico: {mediana}\nentre {lower} y {upper}")), 
             data = max_chain, size = 2, fill = "#FBFFFB")
ggsave("images/Dengue_predict.pdf", width = 8, height = 5)
ggsave("images/Dengue_predict.png", width = 8, height = 5, dpi = 750, bg = "white")


