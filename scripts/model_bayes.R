#Bayesian model
pacman::p_load(tidyverse, cmdstanr, bayestestR, lubridate, posterior, ggtext, glue)

dengue_data <- read_csv("datos-limpios/dengue_for_model_mx.csv")  

#Check pacf(dengue_data$log_nraw) for AR(p)
max_autocorrelation_order <- 5

#Create year, month and week variable
dengue_data <- dengue_data %>%
  mutate(year    = year(fecha)) %>%
  mutate(epiweek = epiweek(fecha)) %>%
  filter(year > 2015) %>%
  mutate(month   = month(fecha)) %>%
  mutate(normalized_year = year - min(year) + 1)

options(mc.cores = parallel::detectCores())

predict_df <- tibble(fecha = seq(max(dengue_data$fecha) + weeks(1),
                    max(dengue_data$fecha) + weeks(55),
                    by = "1 week"))

predict_df <- predict_df %>%
  mutate(year = year(fecha)) %>%
  mutate(epiweek = epiweek(fecha)) %>%
  mutate(month = month(fecha)) %>%
  mutate(normalized_year = year - min(dengue_data$year) + 1)

chains = 4; iter_warmup = 1000; nsim = 11000; pchains = 4; 
datos  <- list(
  N                        = nrow(dengue_data),
  N_predict                = nrow(predict_df),
  Nyears                   = length(unique(dengue_data$normalized_year)),
  Nyears_predict           = length(setdiff(predict_df$year, dengue_data$year)),
  Nmonths                  = length(unique(dengue_data$month)),
  Nweeks                   = length(unique(dengue_data$epiweek)),
  epiweek                  = dengue_data$epiweek,
  epiweek_predict          = predict_df$epiweek,
  month                    = dengue_data$month,
  month_predict            = predict_df$month,
  max_autorregresive_order = max_autocorrelation_order, 
  year                     = dengue_data$normalized_year,
  year_predict             = predict_df$normalized_year,
  log_cases                = dengue_data$log_nraw
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
log_cases       <- model_sample$draws("log_cases_predict") %>% 
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


predplot <- ggplot(prediction) +
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
    caption = glue::glue("Elaborada el {today()}.<br>**Fuente:** Datos Abiertos de la Secretaría de Salud 2020-{year(today())} y ",
                         "Panoramas Epidemiológicos de Dengue 2017-2019."),
    subtitle = glue::glue("Modelo bayesiano de series de tiempo | **Github:** RodrigoZepeda/DengueMX")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        plot.caption = element_markdown(),
        legend.position = "bottom",
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        plot.subtitle    = element_markdown(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia semanal:", 
                     values = c("Observado" = "#92AF75", "Predicho" = "#12757E")) +
  scale_linetype_manual("Incidencia semanal:", 
                        values = c("Observado" = "solid", "Predicho" = "dashed")) +
  scale_size_manual("Incidencia semanal:", 
                    values = c("Observado" = 1, "Predicho" = 0.5)) +
  geom_label(aes(x = mediana, y = 20000, 
                 label = glue("Fecha del pico: {mediana}\nentre {lower} y {upper}")), 
             data = max_chain, size = 2)
ggsave("images/Dengue_predict.pdf", predplot, width = 8, height = 5)
ggsave("images/Dengue_predict.png", predplot, width = 8, height = 5, dpi = 750, bg = "white")


