rm(list = ls())
pacman::p_load(tidyverse, lubridate, readxl, glue, zoo, ggtext)

#DATA ------

# | > List files----
dengue_data <- list.files(path = "datos-abiertos/", pattern = ".*.zip", full.names = TRUE)

# | > Clean duplicates----
dengue <- read_csv(dengue_data) %>%
  mutate(Fecha_Actualizacion = case_when(
    str_detect(FECHA_ACTUALIZACION,"/") ~ as.Date(FECHA_ACTUALIZACION, "%d/%m/%Y"),
    str_detect(FECHA_ACTUALIZACION,"-") ~ as.Date(FECHA_ACTUALIZACION, "%Y-%m-%d"),
    TRUE ~ NA_Date_
  )) %>%
  group_by(ID_REGISTRO) %>%
  arrange(Fecha_Actualizacion) %>%
  mutate(A = 1:n()) %>%
  arrange(desc(A)) %>%
  distinct(ID_REGISTRO, .keep_all = T) %>%
  ungroup() %>%
  mutate(Fecha_Sintomas = case_when(
    str_detect(FECHA_SIGN_SINTOMAS,"/") ~ as.Date(FECHA_SIGN_SINTOMAS, "%d/%m/%Y"),
    str_detect(FECHA_SIGN_SINTOMAS,"-") ~ as.Date(FECHA_SIGN_SINTOMAS, "%Y-%m-%d"),
    TRUE ~ NA_Date_
  )) %>%
  dplyr::select(-A, -FECHA_ACTUALIZACION, -FECHA_SIGN_SINTOMAS)

#DICCIONARIO -----

# | > Estatus del caso ----
caso   <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                     sheet = "CATÁLOGO ESTATUS_CASO")
dengue <- dengue %>%
  left_join(caso %>% rename(ESTATUS_CASO = CLAVE), by = "ESTATUS_CASO") %>%
  rename(`Estatus Caso` = `DESCRIPCIÓN`) %>%
  dplyr::select(-ESTATUS_CASO)

# | > PCR ----
pcr   <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                     sheet = "CATALOGO RESULTADO_PCR ")
dengue <- dengue %>%
  left_join(pcr %>% rename(RESULTADO_PCR = CLAVE), by = "RESULTADO_PCR") %>%
  rename(`Resultado PCR` = `DESCRIPCIÓN`) %>%
  select(-RESULTADO_PCR)

# | > Dictamen ----
dictamen   <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                    sheet = "CATÁLOGO DICTAMEN")
dengue <- dengue %>%
  left_join(dictamen %>% rename(DICTAMEN = CLAVE), by = "DICTAMEN") %>%
  rename(`Dictamen` = `DESCRIPCIÓN`) %>%
  select(-DICTAMEN)

# | > Institución ----
institucion   <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                         sheet = "CATÁLOGO INSTITUCION")
dengue <- dengue %>%
  left_join(institucion %>% rename(INSTITUCION = CLAVE),
            by = c("INSTITUCION_UM_NOTIF" = "INSTITUCION")) %>%
  rename(`Institución Unidad Médica Notificante` = `DESCRIPCIÓN`) %>%
  select(-INSTITUCION_UM_NOTIF)

# | > Paciente ----
paciente   <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                            sheet = "CATÁLOGO TIPO_PACIENTE")
dengue <- dengue %>%
  left_join(paciente %>% rename(TIPO_PACIENTE = CLAVE),
            by = "TIPO_PACIENTE") %>%
  rename(`Tipo de Paciente` = `DESCRIPCIÓN`) %>%
  select(-TIPO_PACIENTE)

# | > Sexo ----
sexo   <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                         sheet = "CATÁLOGO SEXO")
dengue <- dengue %>%
  left_join(sexo %>% rename(SEXO = CLAVE),
            by = "SEXO") %>%
  rename(`Sexo` = `DESCRIPCIÓN`) %>%
  select(-SEXO)

# | > Municipios y entidades----
municipio  <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                     sheet = "CATÁLOGO MUNICIPIO")
entidad    <- read_excel("datos-abiertos/diccionario/Catalogos_Dengue.xlsx",
                         sheet = "CATÁLOGO ENTIDAD")

munent     <- municipio %>%
  left_join(entidad, by = "CLAVE_ENTIDAD") %>%
  mutate(CLAVE_MUNICIPIO = as.numeric(CLAVE_MUNICIPIO)) %>%
  mutate(CLAVE_ENTIDAD = as.numeric(CLAVE_ENTIDAD))

tipo_loc   <- c("ASIG","RES","UM_NOTIF")

dengue <- dengue %>%
  mutate(across(matches("MUNICIPIO|ENTIDAD"),
                ~ as.numeric(.)))

for (localidad in tipo_loc){
  join_key_pairs        <- c("CLAVE_MUNICIPIO", "CLAVE_ENTIDAD")
  names(join_key_pairs) <- c(glue("MUNICIPIO_{localidad}"), glue("ENTIDAD_{localidad}"))
  dengue <- dengue %>%
    left_join(munent, by = join_key_pairs) %>%
    rename(!!glue("Municipio_{localidad}") := `MUNICIPIO`) %>%
    rename(!!glue("Entidad_{localidad}") := `ENTIDAD_FEDERATIVA`) %>%
    rename(!!glue("Abreviatura_entidad_{localidad}") := `ABREVIATURA`) %>%
    select(-!!names(join_key_pairs))
}

# | > Catálogo si/no----
dengue <- dengue %>%
  mutate(across(c(HABLA_LENGUA_INDIG:TOMA_MUESTRA),
                ~ case_when(
                  as.numeric(.) == 1 ~ "Sí",
                  as.numeric(.) == 2 ~ "No",
                  TRUE ~ NA_character_
                )))

dengue %>%
  write_excel_csv("panoramas_epidemiologicos_previos/processed/dengue_datos_abiertos_con_diccionario_mx.csv") %>%
  write_rds("panoramas_epidemiologicos_previos/processed/dengue_datos_abiertos_con_diccionario_mx.rds")

#Create dataset that matches from 2016 to 2022
dengue_2020_2022 <- dengue %>%
  mutate(Semana_Epidemiologica = epiweek(Fecha_Sintomas)) %>%
  mutate(Anio = epiyear(Fecha_Sintomas)) %>%
  group_by(Semana_Epidemiologica, Anio, Entidad_UM_NOTIF) %>%
  tally() %>%
  rename(Estado = Entidad_UM_NOTIF) %>%
  mutate(Semana_Epidemiologica = as.numeric(Semana_Epidemiologica)) %>%
  ungroup()

# dengue_2017_2019  <- read_csv(
#   c("processed/2019.csv","processed/2018.csv","processed/2017.csv",
#     "processed/2020.csv", "processed/2021.csv")) %>%
#   select(Estado, `Probables (año actual)`, Semana_Epidemiologica, Anio) %>%
#   rename(n = `Probables (año actual)`) %>%
#   mutate(Semana_Epidemiologica = as.numeric(Semana_Epidemiologica)) %>%
#   arrange(Anio, Semana_Epidemiologica) %>%
#   group_by(Anio, Estado) %>%
#   mutate(n = if_else(n < lag(n, default = 0), lag(n, default = 0), n)) %>%
#   mutate(Incidencia = n - lag(n, default = 0)) %>%
#   select(-n) %>%
#   mutate(n = Incidencia)

dengue_pasado  <- read_csv(c("panoramas_epidemiologicos_previos/processed/2016.csv",
                             "panoramas_epidemiologicos_previos/processed/2017.csv",
                             "panoramas_epidemiologicos_previos/processed/2018.csv",
                             "panoramas_epidemiologicos_previos/processed/2019.csv",
                             "panoramas_epidemiologicos_previos/processed/2020.csv")) %>%
  select(Estado, `Probables (año pasado)`, Semana_Epidemiologica, Anio) %>%
  rename(n = `Probables (año pasado)`) %>%
  mutate(Semana_Epidemiologica = as.numeric(Semana_Epidemiologica)) %>%
  mutate(Anio = Anio - 1) %>%
  filter(!(Semana_Epidemiologica == 50 & Anio == 2015)) %>%
  filter(!(Semana_Epidemiologica == 46 & Anio == 2017)) %>%
  filter(!(Semana_Epidemiologica == 49 & Anio == 2019)) %>%
  filter(!(Semana_Epidemiologica == 42 & Anio == 2018)) %>%
  arrange(Anio, Semana_Epidemiologica) %>%
  group_by(Anio, Estado) %>%
  mutate(n = if_else(n < lag(n, default = 0), lag(n, default = 0), n)) %>%
  mutate(Incidencia = n - lag(n, default = 0)) %>%
  mutate(Incidencia = if_else(Incidencia < 0, 0, Incidencia)) %>%
  select(-n) %>%
  mutate(n = Incidencia)

dengue_all <- dengue_pasado %>%
  bind_rows(dengue_2020_2022) %>%
  identity()

fechas <- tibble(fecha = seq(ymd("2015/01/01"), max(dengue$Fecha_Sintomas), by = 1)) %>%
  mutate(Semana_Epidemiologica = epiweek(fecha)) %>%
  mutate(Anio = epiyear(fecha)) %>%
  arrange(fecha) %>%
  group_by(Semana_Epidemiologica, Anio) %>%
  mutate(dist = 1:n()) %>%
  filter(dist == 1) %>%
  select(-dist)

dengue_all <- dengue_all %>%
  #bind_rows(dengue_2020_2022) %>%
  left_join(fechas, by = c("Semana_Epidemiologica", "Anio")) %>%
  mutate(Estado = str_remove_all(Estado,"\\*| \\*")) %>%
  mutate(Estado = str_replace_all(Estado,"MEXICO","MÉXICO")) %>%
  mutate(Estado = case_when(
    str_detect(Estado,"CIUDAD|DISTRITO|CD") ~ "CIUDAD DE MÉXICO",
    str_detect(Estado,"MICHO") ~ "MICHOACÁN",
    str_detect(Estado,"COAH") ~ "COAHUILA",
    str_detect(Estado,"VERACRUZ") ~ "VERACRUZ",
    str_detect(Estado,"YUCATAN") ~ "YUCATÁN",
    str_detect(Estado,"POTOS") ~ "SAN LUIS POTOSÍ",
    str_detect(Estado,"NUEVO LEON") ~ "NUEVO LEÓN",
    str_detect(Estado,"QUERETARO") ~ "QUERÉTARO",
    TRUE ~ Estado
  )) %>%
  filter(!is.na(Estado))

dengue_all %>%
  write_excel_csv("datos-limpios/dengue_2016_2022_mx.csv") %>%
  write_rds("datos-limpios/dengue_2016_2022_mx.rds")

dengue_all_plot <- dengue_all

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

dengue_all %>% write_excel_csv("datos-limpios/dengue_for_model_mx.csv")

#Create plot
dengue_all_plot %>%
  group_by(fecha) %>%
  summarise(n = sum(n)) %>%
  mutate(n = rollmean(n, 7,  fill = 0, align = "right")) %>%
  ggplot() +
  geom_line(aes(x = fecha, y = n), size = 1, color = "#12757E") +
  labs(
    x = "",
    y = "Casos probables",
    title = glue::glue("Incidencia de <span style = 'color:#12757E;'>casos probables de dengue</span> ",
                       "en México por fecha de inicio de síntomas"),
    caption = glue::glue("Elaborada el {today()}"),
    subtitle = glue::glue("Fuente: Datos Abiertos de la Secretaría de Salud y ",
                          "Panoramas Epidemiológicos de Dengue 2017-2019")
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_minor_breaks = "1 month", date_breaks = "3 months",
               date_labels = "%b-%y", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_markdown(),
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        plot.subtitle = element_text(size = 8, face = "italic", color = "gray25")) +
  coord_cartesian(xlim = c(ymd("2015/03/01"), today()))
ggsave("images/Dengue.pdf", width = 8, height = 4)
ggsave("images/Dengue.png", width = 8, height = 4, dpi = 750, bg = "white")

#Create plot by state
set.seed(236857)
colors <- colorRampPalette(c("#92AF75","#12757E"))(32)
dengue_all_plot %>%
  ungroup() %>%
  arrange(fecha, Estado) %>%
  group_by(Estado) %>%
  mutate(n = rollmean(n, 7,  fill = 0, align = "right")) %>%
  ggplot() +
  #geom_point(aes(x = fecha, y = n), size = 0.1, color = "gray75",
  #           data = dengue_all) +
  #geom_line(aes(x = fecha, y = n), size = 0.75, color = "firebrick") +
  #geom_area(aes(x = fecha, y = n), size = 0.75, fill = "firebrick") +
  geom_area(aes(x = fecha, y = n, fill = Estado)) +
  facet_wrap(~Estado, scales = "free_y",nrow = 8) +
  labs(
    x = "",
    y = "",
    title = glue::glue("<br><span style = 'color:#12757E;'>Dengue</span> ",
                       ""),
    caption = glue::glue("Fuente: Datos Abiertos de la Secretaría de Salud (2020-{year(today())}) y ",
                         "Panoramas Epidemiológicos de Dengue 2017-2019. Elaborada el {today()}"),
    subtitle = glue::glue("<span style = 'color:#92AF75;'>Casos probables por fecha de inicio de síntomas</span>")
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_minor_breaks = "6 months", date_breaks = "1 year",
               date_labels = "%Y", expand = c(0, 0)) +
  theme(axis.text.x      = element_text(angle = 90, hjust = 1),
        plot.title       = element_markdown(size = 50, family = "Helvetica"),
        plot.subtitle    = element_markdown(size = 20, color = "gray25", family = "Helvetica",
                                            face = "italic"),
        panel.spacing    = unit(1, "lines"),
        panel.grid       = element_blank(),
        panel.background = element_rect(fill = "#FBFFFB"),
        plot.background  = element_rect(fill = "#FBFFFB"),
        axis.title.y     = element_markdown(color = "black"),
        axis.text        = element_text(color = "black"),
        legend.position  = "none",
        panel.border     = element_rect(color = "black", fill = NA, size = 1)) +
  coord_cartesian(xlim = c(ymd("2015/03/01"), today())) +
  scale_fill_manual(values = sample(colors))
ggsave("images/Dengue_estado.pdf", width = 10, height = 14)
ggsave("images/Dengue_estado.png", width = 10, height = 14, dpi = 750)

