rm(list = ls())
#devtools::install_github("AlbertoAlmuinha/boostime")
pacman::p_load(bsts, tidyverse, lubridate, glue, ggtext, cmdstanr)

#THIS MODEL IS A PROOF OF CONCEPT OF COURSE IT CAN GET BETTER IN SEVERAL WAYS
#I JUST DON'T HAVE THE TIME RIGHT NOW

#Limpieza de la base
dengue_all <- read_rds("datos-limpios/dengue_2016_2022_mx.rds") %>%
  mutate(fecha = ymd(fecha)) %>%
  group_by(fecha) %>%
  summarise(n = sum(n))

missing_dates <- tibble(fecha = seq(min(dengue_all$fecha, na.rm = T),
                                    max(dengue_all$fecha, na.rm = T), by = "1 week"))

dengue_all <- dengue_all %>%
  right_join(missing_dates, by = "fecha") %>%
  arrange(fecha) %>%
  mutate(n.value = round(na.approx(n, maxgap = 4, rule = 2))) %>%
  mutate(nraw = n.value) %>%
  mutate(n = rollmean(nraw, 7,  fill = 0, align = "right")) %>%
  mutate(n = rollmean(n, 3,  fill = 0, align = "right")) %>%
  filter(fecha >= ymd("2015/03/01")) %>%
  mutate(t = as.numeric(fecha - ymd("2015/03/01")) + 1) %>%
  mutate(logn = log(n + 1)) %>%
  mutate(log_nraw = log(nraw + 1))

dengue_all %>% write_excel_csv("datos-limpios/dengue_2015_2022_mx.csv")

#splits <- initial_time_split(dengue_all, prop = 0.6)

#model_arima_catboost <- boost_arima() %>%
#  set_engine("arima_catboost", verbose = 0) %>%
#  fit(logn ~ 1 + fecha + epiweek(fecha) + epiyear(fecha) + month(fecha), data = training(splits))

#model_calibrate <- model_arima_catboost %>%
#  modeltime_table() %>%
#  modeltime_calibrate(new_data = testing(splits)) %>%
#  modeltime_refit(data = dengue_all)

#model_forecast <- model_calibrate %>%
#  modeltime_forecast(h = "1 year", actual_data = dengue_all)

#Some checks
# ggplot(dengue_all) +
#   geom_line(aes(x = 2*pi*epiweek(fecha)/52, y = n, color = as.factor(epiyear(fecha)))) +
#   theme_bw() +
#   coord_polar()
#dengue_ts <- ts(dengue_all$n, freq=365.25/7,
#                start=decimal_date(min(dengue_all$fecha)))
#TSstudio::ts_decompose(dengue_ts, type = "multiplicative")

#Create BSTS model
ss               <- AddSemilocalLinearTrend(list(), dengue_all$logn)
ss               <- AddSeasonal(ss, dengue_all$logn, nseasons = 52)
ss               <- AddSeasonal(ss, dengue_all$logn, season.duration = 52,
                                nseasons = length(unique(year(dengue_all$fecha))))
bsts.model       <- bsts(dengue_all$logn, state.specification = ss,
                         niter = 2000, ping=500, seed=2016)
burn             <- SuggestBurn(0.1, bsts.model)

t_horizon <- 52 - epiweek(max(dengue_all$fecha)) + 1
p         <- predict.bsts(bsts.model, horizon = t_horizon, burn = burn, quantiles = c(0.1, .9))

tibble(
  fecha    = c(dengue_all$fecha, max(dengue_all$fecha) + weeks(0:(t_horizon - 1))),
  pred     = c(dengue_all$n, exp(p$mean)) - 1,
  #model2      = exp(model_forecast$.value[1:(length(dengue_all$n) + t_horizon)]),
  #model2_low  = exp(model_forecast$.conf_lo[1:(length(dengue_all$n) + t_horizon)]),
  #model2_up   = exp(model_forecast$.conf_hi[1:(length(dengue_all$n) + t_horizon)]),
  lower_ci = c(rep(0, length(dengue_all$n)), exp(p$interval[1,])) - 1,
  upper_ci = c(rep(0, length(dengue_all$n)), exp(p$interval[2,])) - 1,
  color    = c(rep("Observado", length(dengue_all$n)), rep("Predicho", t_horizon))
) %>%
ggplot(aes(x = fecha, y = pred)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25,
              fill = "#12757E") +
  #geom_ribbon(aes(ymin = model2_low, ymax = model2_up), alpha = 0.25,
  #            fill = "red") +
  geom_line(aes(color = color, linetype = color, size = color)) +
 #geom_line(aes(color = "red", size = color, y = model2)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  labs(
    x = "",
    y = "Casos probables",
    title = glue::glue("<span style = 'color:#92AF75;'>Casos probables de dengue</span> ",
                       "en México por fecha de inicio de síntomas"),
    caption = glue::glue("Elaborada el {today()}"),
    subtitle = glue::glue("Fuente: Datos Abiertos de la Secretaría de Salud y ",
                          "Panoramas Epidemiológicos de Dengue 2017-2019")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        plot.subtitle = element_text(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia", values = c("Observado" = "#92AF75", "Predicho" = "#12757E")) +
  scale_linetype_manual("Incidencia", values = c("Observado" = "solid", "Predicho" = "dashed")) +
  scale_size_manual("Incidencia", values = c("Observado" = 1, "Predicho" = 0.5))
ggsave("images/Dengue_predict.pdf", width = 8, height = 4)
ggsave("images/Dengue_predict.png", width = 8, height = 4, dpi = 750, bg = "white")
