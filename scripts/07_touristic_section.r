#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean touristic data by censal section
#         Data: Webscrapping INE, series experimentales,  Medición del número de viviendas turísticas en España y su capacidad
#         Serie origen: Viviendas turísticas, plazas y plazas por vivienda turística. Censos por Sección Censal

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)

name="Pedro"

if (name=="Pedro"){
  path <- paste0("C:/Users/user/Documents/mispapers/Housing/data/")
} else {
  path <- paste0("Plug_your_path")
}

# Get all files in folder
files <- list.files(paste0(path,"Series/raw/viv_turisticas"), full.names = TRUE)
files <- files[grepl("tabla5", files)]

# Open all files
data <- NA
for(i in files){
    print(i)
    dt <- read_xlsx(i, sheet = ifelse("Secciones" %in% excel_sheets(i), "Secciones", "Secciones censales"))
    names(dt) <- tolower(names(dt))
    names(dt)[1] <- "codigosec"
    names(dt)[10] <- "plazas por vivienda turistica"
    data <- rbind(data, dt)
}

# Preliminar Clean and Select variables
data <- data %>% 
  dplyr::select(codigosec, "vivienda turistica", "plazas", "porcentaje vivienda turistica",
                periodo, prov, mun) %>% na.omit() %>%
    mutate(year = as.numeric(substr(periodo, 1, 4)),
           month = as.numeric(substr(periodo, 6, 7)))  %>%
    mutate(mun = (substr(mun, 3, 5))) %>%
    dplyr::select(-periodo) %>%
    rename(vivienda = 'vivienda turistica',
           share = 'porcentaje vivienda turistica')

data$date <- dmy(paste("01", data$month, data$year))
data$viv_tot <- data$vivienda*100/data$share         # Get "total viviendas". Not sure this is relaible
