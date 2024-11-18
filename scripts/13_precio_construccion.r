#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean Indices Precios de Construcción
#         Data: Estadísticos del Índice de Precios de Construcción, INE
#         Serie origen: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736154972&idp=1254735576757

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)

name="Pedro"

if (name=="Pedro"){
  path <- paste0("C:/Users/user/Documents/mispapers/Housing/data/")
} else {
  path <- paste0("H:/")
}

