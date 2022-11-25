#!/usr/bin/env Rscript

#Bayesian model
rm(list = ls())
pacman::p_load(clusterGeneration, cmdstanr, bayestestR, lubridate, 
               posterior, ggtext, glue, ggrepel, tidyverse, cowplot, ggcorrplot)
# 
# #Archivos con la info de precipitación y temperatura
# clima_data  <- read_rds("datos-clima/processed/Clima_info.rds") %>%
#   mutate(MES_NUM = as.numeric(MES_NUM)) %>%
#   filter(ANIO >= 2016) %>%
#   pivot_wider(id_cols = c(FECHA_PROXY, ENTIDAD), names_from = VARIABLE, values_from = VALOR) %>%
#   filter(!is.nan(Precipitacion)) 
# 
# #Colocamos en el formato para saber asignar mes/día de temp
# clima_vars <- clima_data %>%
#   group_by(ENTIDAD) |>
#   #mutate(across(c(Precipitacion:Temperatura_Minima), ~ (. - mean(.))/sd(.))) |>
#   ungroup() |>
#   mutate(MES_NUM = month(FECHA_PROXY)) |>
#   mutate(ANIO_NUM = year(FECHA_PROXY))
# 
# dengue_data <- read_csv("datos-limpios/dengue_2016_2022_mx.csv", show_col_types = FALSE) |>
#   mutate(MES_NUM = month(fecha)) |>
#   group_by(Estado, Anio, MES_NUM) |>
#   mutate(Estado = if_else(Estado == "MÉXICO", "ESTADO DE MÉXICO", Estado)) |>
#   summarise(n = sum(n), .groups = "drop") |>
#   group_by(Estado) |>
#   #mutate(n = (n - mean(n))/sd(n)) |>
#   ungroup()
#   
# jointdist <- clima_vars |> 
#   inner_join(dengue_data, by = c("ENTIDAD" = "Estado","MES_NUM","ANIO_NUM" = "Anio")) |>
#   arrange(FECHA_PROXY, ENTIDAD)
# 
# nlags  <- 12
# k <- 0
# vars_df <- c("Precipitacion","Temperatura_Minima","Temperatura_Maxima","Temperatura_Promedio")
# for (variable in vars_df){
#   k <- k + 1
#   matcor <- matrix(NA, nrow = length(unique(jointdist$ENTIDAD)), ncol = nlags + 1)
#   for (i in 1:length(unique(jointdist$ENTIDAD))){
#     entidad <- unique(jointdist$ENTIDAD)[i]
#     
#     ent_df <- jointdist |>
#       filter(ENTIDAD == entidad)
#     
#     for (j in 0:nlags){
#       matcor[i,j + 1] = cor(sqrt(ent_df$n + 1), lag(sqrt(ent_df[,variable] + 1), j), use = "complete.obs")
#     }
#   }
#   
#   matcor           <- as_tibble(matcor, .name_repair = "unique")
#   colnames(matcor) <- paste0("lag",0:nlags)
#   matcor$Entidad   <- unique(jointdist$ENTIDAD)
#   matcor$Variable  <- variable
#   
#   if (variable == vars_df[1]){
#     res_matrix <- matcor
#   } else {
#     res_matrix <- res_matrix |> bind_rows(matcor)
#   }
#   
# }
# 
# 
# res_matrix <- res_matrix |>
#   pivot_longer(cols = starts_with("lag"), names_to = "lag", 
#                names_transform =  ~ as.numeric(str_remove_all(.,"lag")), values_to = "cor") |>
#   mutate(Entidad = factor(Entidad, levels = rev(unique(jointdist$ENTIDAD)), ordered = TRUE)) |>
#   mutate(Variable = case_when(
#     str_detect(Variable,"Precip") ~ "Precipitation",
#     str_detect(Variable,"Max") ~ "Max. temperature",
#     str_detect(Variable,"Min") ~ "Min. temperature",
#     str_detect(Variable,"Pro") ~ "Avg. temperature",
#   )) 
# 
# ggplot(res_matrix) +
#   geom_tile(aes(x = lag, y = Entidad, fill = abs(cor)), color = "white") +
#   geom_text(aes(x = lag, y = Entidad, label = round(cor,2)), color = "white", size = 2) +
#   facet_wrap(~ Variable, nrow = 1) +
#   scale_fill_viridis_b() +
#   coord_equal() +
#   theme_minimal() +
#   scale_x_continuous(breaks = 0:nlags) +
#   labs(
#     x = "Lag",
#     y = NULL,
#     title = "Correlation between dengue cases and weather variables"
#   ) +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 20, hjust = 0.5),
#     strip.text = element_text(size = 14, hjust = 0.5),
#     panel.grid = element_blank()
#   )
# ggsave("Cor_weather.pdf", width = 14, height = 8)
# 


dengue_data <- read_csv("datos-limpios/dengue_2016_2022_mx.csv", show_col_types = FALSE) |>
  group_by(Estado, Anio, Semana_Epidemiologica) |>
  mutate(Estado = if_else(Estado == "MÉXICO", "ESTADO DE MÉXICO", Estado)) |>
  summarise(n = sum(n), .groups = "drop") 

dengue_data <- dengue_data |>
  left_join(
    tibble(fecha = seq(ymd("2014/01/01"), today(), by = "1 day")) |>
      mutate(Semana_Epidemiologica = epiweek(fecha)) |>
      mutate(Anio = epiyear(fecha)) |>
      distinct(Anio, Semana_Epidemiologica, .keep_all = TRUE)
  )

dengue_data <- dengue_data |>
  full_join(
    expand_grid(
      Estado = unique(dengue_data$Estado),
      fecha  = unique(dengue_data$fecha)
    )
  )

dengue_data <- dengue_data |>
  mutate(n = if_else(is.na(n), 0, n))

nlags <- 3
k <- 0
for (lag in 0:nlags){
  k      <- k + 1
  matcor <- matrix(NA, nrow = length(unique(dengue_data$Estado)), ncol = length(unique(dengue_data$Estado)))
  for (i in 1:length(unique(dengue_data$Estado))){
    
    entidad <- unique(dengue_data$Estado)[i]
    
    ent_df <- dengue_data |>
      filter(Estado == entidad)
    
    for (j in 1:length(unique(dengue_data$Estado))){
      entidadj <- unique(dengue_data$Estado)[j]
      ent_dj   <- dengue_data |>
        filter(Estado == entidadj)
      matcor[i,j] = cor(ent_df$n, lag(ent_dj$n, lag, default = NA), use = "complete.obs")
    }
  }
  
  matcor           <- as_tibble(matcor, .name_repair = "unique")
  colnames(matcor) <- unique(dengue_data$Estado)
  matcor$Entidad   <- unique(dengue_data$Estado)
  matcor$Lags      <- lag
  
  if (lag == 0){
    res_matrix <- matcor
  } else {
    res_matrix <- res_matrix |> bind_rows(matcor)
  }
}


res_matrix <- res_matrix |>
  pivot_longer(cols = c(AGUASCALIENTES:ZACATECAS), names_to = "Entidad_2", values_to = "cor") |>
  mutate(Entidad_2 = factor(Entidad_2, levels = rev(unique(dengue_data$Estado)), ordered = TRUE)) 

ggplot(res_matrix) +
  geom_tile(aes(x = Entidad, y = Entidad_2, fill = abs(cor)), color = "white") +
  geom_text(aes(x = Entidad, y = Entidad_2, label = round(cor,2)), color = "white", size = 1) +
  facet_wrap(~ Lags, ncol = 2) +
  scale_fill_viridis_b() +
  coord_equal() +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    title = "Correlation between dengue cases"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text = element_text(size = 14, hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
    axis.text.y = element_text(size = 5)
  )
ggsave("Cor_state.pdf", width = 14, height = 10)

