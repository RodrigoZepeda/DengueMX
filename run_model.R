rm(list = ls())
pacman::p_load(bsts, tidyverse, lubridate, glue, ggtext)

#THIS MODEL IS A PROOF OF CONCEPT OF COURSE IT CAN GET BETTER IN SEVERAL WAYS
#I JUST DON'T HAVE THE TIME RIGHT NOW

#Limpieza de la base
dengue_all <- read_rds("datos-limpios/dengue_2016_2022_mx.rds") %>%
  group_by(fecha) %>%
  summarise(n = sum(n)) %>%
  mutate(n = rollmean(n, 7,  fill = 0, align = "right")) %>% 
  mutate(n = rollmean(n, 3,  fill = 0, align = "right")) %>%
  filter(fecha >= ymd("2015/03/01")) %>%
  mutate(t = as.numeric(fecha - ymd("2015/03/01")) + 1) %>%
  mutate(logn = log(n))

#Create BSTS model
ss               <- AddSemilocalLinearTrend(list(), dengue_all$logn)
ss               <- AddSeasonal(ss, dengue_all$logn, nseasons = 52)
bsts.model       <- bsts(dengue_all$logn, state.specification = ss, niter = 500, ping=0, seed=2016)
burn             <- SuggestBurn(0.1, bsts.model)

t_horizon <- 50
p         <- predict.bsts(bsts.model, horizon = t_horizon, burn = burn, quantiles = c(0.1, .9))

tibble(
  fecha    = c(dengue_all$fecha, max(dengue_all$fecha) + weeks(0:(t_horizon - 1))),
  pred     = c(dengue_all$n, exp(p$mean)),
  lower_ci = c(rep(0, length(dengue_all$n)), exp(p$interval[1,])),
  upper_ci = c(rep(0, length(dengue_all$n)), exp(p$interval[2,])),
  color    = c(rep("Observado", length(dengue_all$n)), rep("Predicho", t_horizon))
) %>%
ggplot(aes(x = fecha, y = pred)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25,
              fill = "#143D59") +
  geom_line(aes(color = color, linetype = color, size = color)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  labs(
    x = "",
    y = "Casos probables",
    title = glue::glue("Incidencia de <span style = 'color:#F4B41A;'>casos probables de dengue</span> ", 
                       "en México por fecha de inicio de síntomas"),
    caption = glue::glue("Elaborada el {today()}"),
    subtitle = glue::glue("Fuente: Datos Abiertos de la Secretaría de Salud y ", 
                          "Panoramas Epidemiológicos de Dengue 2017-2019")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia", values = c("Observado" = "#F4B41A", "Predicho" = "#143D59")) +
  scale_linetype_manual("Incidencia", values = c("Observado" = "solid", "Predicho" = "dashed")) +
  scale_size_manual("Incidencia", values = c("Observado" = 1, "Predicho" = 0.5))
ggsave("images/Dengue_predict.pdf", width = 8, height = 4)
ggsave("images/Dengue_predict.png", width = 8, height = 4, dpi = 750, bg = "white")
