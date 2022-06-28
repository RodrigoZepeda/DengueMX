rm(list = ls())
pacman::p_load(tidyverse, RCurl, glue, lubridate, foreach, readxl, ggtext)

dir_name <- "datos-clima"

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
      
      anio_db <- str_remove_all(fzip[fnum], 
                                glue("{dir_name}/{fname}/|[^\\d.].xlsx|[^\\d.]")) |> as.numeric()
      
      message(glue("-- | {anio_db}"))
      
      read_excel(fzip[fnum], skip = 1) %>% 
        mutate(ANIO = anio_db) %>%
        mutate(VARIABLE = str_remove_all(fname,"_Excel"))
      
    }
    
    db
}

clima_db <- clima_db %>% 
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
    filter(VARIABLE == !!var & ENTIDAD != "NACIONAL" & ANIO >= 2016) %>%
    ggplot() +
      geom_col(aes(x = FECHA_PROXY, y = VALOR, fill = ENTIDAD)) +
      facet_wrap(~ENTIDAD, scales = "free_y", ncol = 4) +
      labs(
        x = "",
        y = var,
        title = str_remove_all(var, "_")
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 30),
        legend.position = "none"
      )
  ggsave(glue("{dir_name}/images/{var}.pdf"), p1, width = 8, height = 12)
}

clima_db %>%
  write_rds(glue("{dir_name}/processed/Clima_info.rds")) %>%
  write_excel_csv(glue("{dir_name}/processed/Clima_info.csv"))
