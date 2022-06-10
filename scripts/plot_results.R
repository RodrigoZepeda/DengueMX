#!/usr/bin/env Rscript

pacman::p_load(bsts, tidyverse, lubridate, glue, ggtext, cmdstanr, reticulate)

#Limpieza de la base
experimento <- read_csv("predicciones_DARTS_experimento.csv")
dengue_all  <- read_csv("datos-limpios/dengue_for_model_mx.csv")

ggplot(experimento) +
  geom_line(aes(x = fecha, y = n, color = "Predicho", size = "Predicho"),
           data = dengue_all) +
  geom_line(aes(x = date, y = median, color = "Predicho",
                linetype = "Predicho", size = "Predicho"), data = experimento) +
  geom_ribbon(aes(x = date, ymin = lower_ci, y = median, ymax = upper_ci), alpha = 0.25,
              fill = "#12757E", data = experimento) +
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.25,
  #            fill = "#12757E") +
  #geom_ribbon(aes(ymin = model2_low, ymax = model2_up), alpha = 0.25,
  #            fill = "red") +
  geom_point(aes(color = "Observado", linetype = "Observado", size = "Observado",
                x = fecha, y = nraw), data = dengue_all, alpha = 0.5) +
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
    subtitle = glue::glue("Modelo de interpolación jerárquica neuronal (N-HiTS) | Github: RodrigoZepeda/DengueMX")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        plot.subtitle    = element_text(size = 8, face = "italic", color = "gray25")) +
  scale_color_manual("Incidencia", values = c("Observado" = "#92AF75", "Predicho" = "#12757E")) +
  scale_linetype_manual("Incidencia", values = c("Observado" = "solid", "Predicho" = "dashed")) +
  scale_size_manual("Incidencia", values = c("Observado" = 1, "Predicho" = 0.5))
ggsave("images/Dengue_predict.pdf", width = 8, height = 4)
ggsave("images/Dengue_predict.png", width = 8, height = 4, dpi = 750, bg = "white")
