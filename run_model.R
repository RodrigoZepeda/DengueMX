rm(list = ls())
pacman::p_load(cmdstanr, tidyverse, lubridate, glue, ggtext)

dengue_all <- read_rds("processed/dengue_2016_2022_mx.rds")

dengue_all %>%
  group_by(fecha) %>%
  summarise(n = sum(n)) %>%
  mutate(n = rollmean(n, 7,  fill = 0, align = "right")) %>% 
  mutate(n = rollmean(n, 3,  fill = 0, align = "right")) %>%
  ggplot() +
  geom_line(aes(x = fecha, y = n), size = 1, color = "firebrick") +
  labs(
    x = "",
    y = "Casos probables",
    title = glue::glue("Incidencia de <span style = 'color:firebrick;'>casos probables de dengue</span> ", 
                       "en México por fecha de inicio de síntomas"),
    caption = glue::glue("Elaborada el {today()}"),
    subtitle = glue::glue("Fuente: Datos Abiertos de la Secretaría de Salud y ", 
    "Panoramas Epidemiológicos de Dengue 2017-2019")
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_minor_breaks = "1 month", date_breaks = "3 months",
               date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        plot.subtitle = element_text(size = 8, face = "italic", color = "gray25"))
ggsave("Dengue.pdf", width = 8, height = 4)
