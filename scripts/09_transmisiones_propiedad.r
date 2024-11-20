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
  path <- paste0("Plug_your_path")
}

# Open all files. These are absolutely trivial
data1 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_naturaleza.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data1) <- c("ccaa", "prov", "naturaleza", "periodo", "total_natur")
data1$year <- substr(data1$periodo, 1, 4)
data1$month <- substr(data1$periodo, 6, 7)
data1$date <- dmy(paste("01", data1$month, data1$year))

data2 <- read.csv(paste0(path,"Series/raw/etdp/etdp_personafisica.csv"), sep = ";", encoding = "latin1")
names(data2) <- c("tipo_transm", "tipo_titular", "periodo", "total")
data2$year <- substr(data2$periodo, 1, 4)
data2$month <- substr(data2$periodo, 6, 7)
data2$date <- dmy(paste("01", data2$month, data2$year))

data3 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_regimen.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data3) <- c("ccaa", "prov", "regimen", "periodo", "total_regimen")
data3$year <- substr(data3$periodo, 1, 4)
data3$month <- substr(data3$periodo, 6, 7)
data3$date <- dmy(paste("01", data3$month, data3$year))

data4 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_total.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data4) <- c("ccaa", "prov", "titulo_adquis", "periodo", "total_titul")
data4$year <- substr(data4$periodo, 1, 4)
data4$month <- substr(data4$periodo, 6, 7)
data4$date <- dmy(paste("01", data4$month, data4$year))

data5 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_titulo.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data5) <- c("ccaa", "prov", "titulo_adquis", "periodo", "total_titul")
data5$year <- substr(data5$periodo, 1, 4)
data5$month <- substr(data5$periodo, 6, 7)
data5$date <- dmy(paste("01", data5$month, data5$year))

data6 <- read.csv(paste0(path,"Series/raw/etdp/etdp_ccaa_prov_urbanas.csv"), sep = ";", encoding = "latin1") %>%
            dplyr::select(-c(1))
names(data6) <- c("ccaa", "prov", "titulo_adquis", "periodo", "total_titul")
data6$year <- substr(data6$periodo, 1, 4)
data6$month <- substr(data6$periodo, 6, 7)
data6$date <- dmy(paste("01", data6$month, data6$year))
