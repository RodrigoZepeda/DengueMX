rm(list = ls())

#Python to tabulapy read the pdf's
reticulate::use_condaenv("Dengue")
pacman::p_load(tidyverse, RCurl, glue, lubridate, foreach, readxl, ggtext, reticulate, MetBrewer)

paleta_color <- met.brewer("Renoir", n = 32, type = "continuous")

#Python tabula-py config
tabula <- import("tabula")

#Descarga la base de datos de temperatura y precipitación
#la base de datos contiene hasta junio del 2021 por lo que es necesario descargar
#así mismo los archivos en pdf y parsearlos con tabula py


dir_name <- "datos-clima"

message("Descarga hasta 2021")

#Leave some time in January for them to update
archivonames <- c("Precipitacion","Temperatura_Maxima_Excel",
                  "Temperatura_Promedio_Excel","Temperatura_Minima_Excel")

clima_db <- foreach (archivo_num=1:length(archivonames), .combine = dplyr::bind_rows) %do% {
    
    fname   <- archivonames[archivo_num]
    pdf_url <- glue("http://201.116.60.46/DatosAbiertos/{fname}.zip")
    
    message(glue("Downloading: {fname}"))
    
    download.file(pdf_url, glue("{dir_name}/{fname}.zip"), method = "curl", quiet = T)
    
    unzip(glue("{dir_name}/{fname}.zip"), exdir = dir_name, overwrite = T)
    
    fzip <- list.files(glue("{dir_name}/{fname}"), full.names = TRUE)
    
    db <- foreach(fnum=1:length(fzip), .combine = dplyr::bind_rows) %do% {
      
      #Ojo en el año 2020 hay uno repetido incompleto
      if (fzip[fnum] != "datos-clima/Precipitacion/2020Precip.xls"){
        
        anio_db <- str_remove_all(fzip[fnum], 
                                  glue("{dir_name}/{fname}/|[^\\d.].xlsx|[^\\d.]")) |> as.numeric()
        
        message(glue("-- | {anio_db}"))
        
        read_excel(fzip[fnum], skip = 1) %>% 
          mutate(ANIO = anio_db) %>%
          mutate(VARIABLE = str_remove_all(fname,"_Excel"))
      } else {
        
        NULL
        
      }
      
    }
    
    #Delete files
    unlink(glue("{dir_name}/{fname}"), recursive = TRUE)
    
    db
}

message("Descarga desde 2021")

if (!dir.exists(glue("{dir_name}/pdf_reports"))){
  dir.create(glue("{dir_name}/pdf_reports"))
}

vars <- c("PREC","TMIN","TMAX","TMED")

#Esperar hasta febrero para descargar el año subsecuente
db2021 <- foreach (year = 2021:year(today() - days(35)), .combine = bind_rows) %do% {
  
  message(glue("Año: {year}"))
  
  foreach (j = 1:length(vars), .combine = bind_rows) %do% {
    
    var <- vars[j]
    
    message(glue("Variable: {var}"))
    
    url_file <- glue("https://smn.conagua.gob.mx/tools/DATA/Climatolog%C3%ADa/Pron%C3%B3stico%20",
                     "clim%C3%A1tico/Temperatura%20y%20Lluvia/{var}/{year}.pdf")
    
    #Descarga de reportes en pdf
    fname        <- glue("{dir_name}/pdf_reports/{var}_{year}.pdf")
    savename     <- glue("{dir_name}/pdf_reports/{var}_{year}.csv")
    download.file(url_file, fname, quiet = T)
    
    #Lectura con Python del excel
    #Area = top, left, bottom, right
    dftab           <- tabula$read_pdf(fname, lattice = TRUE, 
                                       pages = "1", area = c("52","33","520","700"))[[1]]
    dftab$Entidad   <- as.character(dftab$Entidad)
    colnames(dftab) <- toupper(colnames(dftab))
    
    dftab %>% 
      filter(ENTIDAD != "NaN") %>%
      mutate(ANIO = year) %>%
      mutate(VARIABLE = case_when(
        !!var == "PREC" ~ "Precipitacion",
        !!var == "TMIN" ~ "Temperatura_Minima",
        !!var == "TMAX" ~ "Temperatura_Maxima",
        !!var == "TMED" ~ "Temperatura_Promedio",
      ))
  }
}


clima_db <- clima_db %>% 
  filter(ANIO < 2021) %>%
  bind_rows(db2021) %>%
  mutate(ENTIDAD = toupper(ENTIDAD)) %>%
  pivot_longer(names_to = "MES", cols = c(ENE:DIC), values_to = "VALOR")

clima_db <- clima_db %>%
  mutate(MES_NUM = case_when(
    MES == "ENE" ~ "01",
    MES == "FEB" ~ "02",
    MES == "MAR" ~ "03",
    MES == "ABR" ~ "04",
    MES == "MAY" ~ "05",
    MES == "JUN" ~ "06",
    MES == "JUL" ~ "07",
    MES == "AGO" ~ "08",
    MES == "SEP" ~ "09",
    MES == "OCT" ~ "10",
    MES == "NOV" ~ "11",
    MES == "DIC" ~ "12"
  )) %>%
  mutate(FECHA_PROXY = ymd(paste0(ANIO,"/",MES_NUM,"/15")))

for (var in unique(clima_db$VARIABLE)){
  p1 <- clima_db %>%
    filter(VARIABLE == !!var & ENTIDAD != "NACIONAL") %>%
    ggplot() +
      geom_line(aes(x = FECHA_PROXY, y = VALOR, color = ENTIDAD)) +
      facet_wrap(~ENTIDAD, scales = "free_y", ncol = 4) +
      labs(
        x = "",
        y = toupper(str_replace_all(var, "_", " ")),
        title = glue("<br>",toupper(str_replace_all(var, "_"," "))),
        subtitle = "<span style='color: gray25'>[ Promedio mensual ]</span><br>",
        caption = paste("**Fuente:** CONAGUA <br>**Github:** RodrigoZepeda/DengueMX")
      ) +
      theme_classic() +
      theme(
        plot.subtitle    = element_markdown(size = 15, hjust = 0.5),
        plot.caption     = element_markdown(),
        plot.title       = element_markdown(size = 30, hjust = 0.5),
        legend.position  = "none",
        panel.background = element_rect(fill = "#f4f3e8"),
        plot.background  = element_rect(fill = "#f4f3e8"),
        strip.background = element_rect(fill = "gray25"),
        strip.text       = element_text(color = "white", face = "bold")
      ) +
    scale_color_manual("Entidad", values = paleta_color)
  ggsave(glue("{dir_name}/images/{var}.pdf"), p1, width = 15, height = 12)
  
  p2 <- clima_db %>%
    filter(VARIABLE == !!var & ENTIDAD == "NACIONAL") %>%
    ggplot() +
    geom_line(aes(x = FECHA_PROXY, y = VALOR, color = ENTIDAD)) +
    facet_wrap(~ENTIDAD, scales = "free_y", ncol = 4) +
    labs(
      x = "",
      y = toupper(str_replace_all(var, "_", " ")),
      title = glue("<br>",toupper(str_replace_all(var, "_"," "))),
      subtitle = "<span style='color: gray25'>[ Promedio mensual ]</span><br>",
      caption = paste("**Fuente:** CONAGUA <br>**Github:** RodrigoZepeda/DengueMX")
    ) +
    theme_classic() +
    theme(
      plot.subtitle    = element_markdown(size = 12, hjust = 0.5),
      plot.caption     = element_markdown(),
      plot.title       = element_markdown(size = 14, hjust = 0.5),
      legend.position  = "none",
      panel.background = element_rect(fill = "#f4f3e8"),
      plot.background  = element_rect(fill = "#f4f3e8"),
      strip.background = element_rect(fill = "gray25"),
      strip.text       = element_text(color = "white", face = "bold")
    ) +
    scale_color_manual("Entidad", values = paleta_color)
  ggsave(glue("{dir_name}/images/{var}_NACIONAL.pdf"), p2, width = 8, height = 4)
}

clima_db %>%
  write_rds(glue("{dir_name}/processed/Clima_info.rds")) %>%
  write_excel_csv(glue("{dir_name}/processed/Clima_info.csv"))
