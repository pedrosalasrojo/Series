#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean Índice de Precios de Vivienda en Alquiler (IPVA). Ano base 2015
#         Data: Índice de Precios de Vivienda en Alquiler (IPVA), INE, Estadistica Experimental
#         Serie origen: https://www.ine.es/experimental/ipva/experimental_precios_vivienda_alquiler.htm

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)

name="Pedro2"

if (name=="Pedro"){
  path <- paste0("C:/Users/user/Documents/mispapers/Housing/data/")
} else {
  path <- paste0("H:/")
}

# Get all files in folder
files <- list.files(paste0(path,"Series/raw/ipva"), full.names = TRUE)

# Open all files for ccaa ----
files_ccaa <- files[grepl("ccaa", files)]

data <- NA
for(i in files_ccaa){
    print(i)
    dt <- read.csv(i, row.names = NULL, sep = ";")
    names(dt) <- c("totnac", "ccaa", "tipo", "dato", "periodo", "value")
    data <- na.omit(rbind(data, dt))
}

data <- data %>% dplyr::select(-totnac)

# Open all files for ccaa ----
files_pro <- files[grepl("pro", files)]

data <- NA
for(i in files_pro){
    print(i)
    dt <- read.csv(i, row.names = NULL, sep = ";")
    names(dt) <- c("totnac", "pro", "tipo", "dato", "periodo", "value")
    data <- na.omit(rbind(data, dt))
}

data <- data %>% dplyr::select(-totnac)

# Open Municipios

data <- read.csv(paste0("H:/Series/raw/ipva/ipva_muni.csv"), row.names = NULL, sep = ";")
names(data) <- c("munic", "tipo", "periodo", "value")

data$cpro <- as.numeric(substr(data$munic, 1, 2))
data$cmun <- as.numeric(substr(data$munic, 3, 5))
data$munic <- sub("^\\d{5}\\s*", "", data$munic)

# Open Districts

data <- read.csv(paste0("H:/Series/raw/ipva/ipva_dist.csv"), row.names = NULL, sep = ";")
names(data) <- c("totnac", "dist", "tipo", "periodo", "value")
data$cpro <- (substr(data$dist, 1, 2))
data$cmun <- (substr(data$dist, 3, 5))
data$dist <- (substr(data$dist, 6, 7))

data <- data %>% dplyr::select(-totnac)