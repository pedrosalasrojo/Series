#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean Parque de Viviendas
#         Data: Estimación del Parque de Viviendas, Ministerio de Tranportes
#         Serie origen: https://www.transportes.gob.es/el-ministerio/informacion-estadistica/vivienda-y-actuaciones-urbanas/estadisticas/estimacion-del-parque-de-viviendas

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)

name="Pedro2"

if (name=="Pedro"){
  path <- paste0("C:/Users/user/Documents/mispapers/Housing/data/")
} else {
  path <- paste0("H:/")
}

# Open file for "total"
data <- readxl::read_xls(paste0(path, "Series/raw/pv/pv_ccaa_prov_total.xls"))
data <- data[-c(1:5), ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]
data <- data[-1, ]

data <- data %>%
    pivot_longer(cols = -prov_name, names_to = "year", values_to = "values") 

# Open file for "desglose"

data <- readxl::read_xls(paste0(path, "Series/raw/pv/pv_ccaa_prov_desglose.xls"))
data <- data[-c(1:5), ]
row1 <- c(NA, zoo::na.locf(as.numeric(data[1, ]), fromLast = FALSE))
data <- rbind(row1, data[-1, ])
year <- as.character(data[1, ])
type <- as.character(data[2, ])
type <- ifelse(type == "No", "No Principales", type)
yeartype <- paste0(year," ", type)
data <- rbind(yeartype, data[-1, ])
data <- data[-2, ]
data <- data[-2, ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]

data <- data %>%
    pivot_longer(cols = -prov_name, names_to = "yeartype", values_to = "values") %>%
        mutate(year = substr(yeartype, 1, 4),
               type = substr(yeartype, 6, nchar(yeartype))) %>%
        dplyr::select(-yeartype) 

