#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean Estadística de Transmisiones de Derechos de la Propiedad
#         Data: Estadística de Transmisiones de Derechos de la Propiedad
#         Serie origen: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736171438&idp=1254735576606

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

# Open all files
data1 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_naturaleza.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data1) <- c("ccaa", "prov", "naturaleza", "periodo", "total_natur")

data2 <- read.csv(paste0(path,"Series/raw/etdp/etdp_personafisica.csv"), sep = ";", encoding = "latin1")
names(data2) <- c("tipo_transm", "tipo_titular", "periodo", "total")

data3 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_regimen.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data3) <- c("ccaa", "prov", "regimen", "periodo", "total_regimen")

data4 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_total.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data4) <- c("ccaa", "prov", "titulo_adquis", "periodo", "total_titul")

data5 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_titulo.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data5) <- c("ccaa", "prov", "titulo_adquis", "periodo", "total_titul")

data6 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_urbanas.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data6) <- c("ccaa", "prov", "titulo_adquis", "periodo", "total_titul")
