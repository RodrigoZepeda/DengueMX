rm(list = ls())
pacman::p_load(bsts, tidyverse, lubridate, glue, ggtext, cmdstanr, reticulate)

#Limpieza de la base
experimento <- read_csv("predicciones_DARTS_experimento.csv")
dengue_all  <- read_csv("datos-limpios/dengue_for_model_mx.csv")

ggplot(experimento) +
  geom_line(aes(x = date, y = median), data = experimento, color = "black") +
  geom_ribbon(aes(x = date, ymin = lower_ci, y = median, ymax = upper_ci), alpha = 0.25,
              fill = "#12757E", data = experimento) +
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25,
  #            fill = "#12757E") +
  #geom_ribbon(aes(ymin = model2_low, ymax = model2_up), alpha = 0.25,
  #            fill = "red") +
  geom_line(aes(color = "Observado", linetype = "Observado", size = "Observado",
                x = fecha, y = nraw), data = dengue_all) +
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
        plot.subtitle    = element_text(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia", values = c("Observado" = "#92AF75", "Predicho" = "#12757E")) +
  scale_linetype_manual("Incidencia", values = c("Observado" = "solid", "Predicho" = "dashed")) +
  scale_size_manual("Incidencia", values = c("Observado" = 1, "Predicho" = 0.5))
ggsave("images/Dengue_predict.pdf", width = 8, height = 4)
ggsave("images/Dengue_predict.png", width = 8, height = 4, dpi = 750, bg = "white")
