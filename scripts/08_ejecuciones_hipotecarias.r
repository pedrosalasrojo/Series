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
  path <- paste0("Plug_your_path")
}

# Open all files. These are absolutely trivial
data1 <- read.csv(paste0(path,"Series/raw/eh/eh_ccaa_estado_vivienda.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data1) <- c("ccaa", "estado", "periodo", "total_estado")
data1$year <- substr(data1$periodo, 1, 4)
data1$term <- substr(data1$periodo, 6, 6)

data2 <- read.csv(paste0(path,"Series/raw/eh/eh_ccaa_naturaleza.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data2) <- c("ccaa", "naturaleza", "periodo", "total_natur")
data2$year <- substr(data2$periodo, 1, 4)
data2$term <- substr(data2$periodo, 6, 6)

data3 <- read.csv(paste0(path,"Series/raw/eh/eh_ccaa_titular.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data3) <- c("ccaa", "titular", "periodo", "total_titular")
data3$year <- substr(data3$periodo, 1, 4)
data3$term <- substr(data3$periodo, 6, 6)

data4 <- read.csv(paste0(path,"Series/raw/eh/eh_total.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data4) <- c("periodo", "total")
data4$year <- substr(data4$periodo, 1, 4)
data4$term <- substr(data4$periodo, 6, 6)
