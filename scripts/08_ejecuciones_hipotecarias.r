#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean Ejecuciones Hipotecarias
#         Data: Estadistica sobre Ejecuciones Hipotecarias
#         Serie origen: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176993&menu=ultiDatos&idp=1254735576757

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)

name="Pedro"

if (name=="Pedro"){
  path <- paste0("C:/Users/user/Documents/mispapers/Housing/data/")
} else {
  path <- paste0("-")
}

# Get all files in folder
files <- list.files(paste0(path,"Series/raw/eh"), full.names = TRUE)
files <- files[grepl("ccaa", files)]

# Open all files
data1 <- read.csv(files[1], sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data1) <- c("ccaa", "estado", "periodo", "total_estado")

data2 <- read.csv(files[2], sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data2) <- c("ccaa", "naturaleza", "periodo", "total_natur")

data3 <- read.csv(files[3], sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data3) <- c("ccaa", "titular", "periodo", "total_titular")

