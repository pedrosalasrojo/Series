#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Clean Estadisticas del Precio de Suelo
#         Data: Estadísticos del Precio del Suelo,  Ministerio de Tranportes
#         Serie origen: https://apps.fomento.gob.es/BoletinOnline2/?nivel=2&orden=36000000

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
files <- list.files(paste0(path,"Series/raw/pc"), full.names = TRUE)

# Open all files for "precios" ----
file_previos <- c("total", "big", "med1", "med2", "med3", "small")

alldata <- NA

for(filename in file_previos){

data <- readxl::read_xls(paste0(path,"Series/raw/pc/precios_ccaa_prov_",filename,".XLS"))
data <- data[-c(1:4), ]
row1 <- gsub("Año ", "", data[1, ])
data <- rbind(row1, data[-1, ])
row1 <- c(NA, zoo::na.locf(as.numeric(data[1, ]), fromLast = FALSE))
data <- rbind(row1, data[-1, ])
data <- data[-2, ]
year <- as.character(data[1, ])
trim <-as.character(data[2, ])
yeartrim <- paste0(year,"_", trim)
data <- rbind(yeartrim, data[-1, ])
data <- data[-2, ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]
data <- data[, -((ncol(data)-1):ncol(data))]

data <- data %>%
  mutate(across(-prov_name, as.character)) %>%
  pivot_longer(cols = -prov_name, names_to = "year", values_to = "Total") %>%
    mutate(Total = as.numeric(Total))

data$file <- filename
data$term <- str_extract(data$year, "(?<=_)[0-9]")
data$year <- str_extract(data$year, "^[0-9]{4}")

alldata <- na.omit(rbind(alldata, data))

}

# Open all files for transacciones ----
file_ps <- c("total", "big", "med1", "med2", "med3", "small", "fisica", "juridi")

alldata <- NA

for(filename in file_ps){

data <- readxl::read_xls(paste0(path,"Series/raw/pc/ps_ccaa_prov_",filename,".XLS"))
data <- data[, -((ncol(data)-1):ncol(data))]
data <- data[-c(1:5), ]
row1 <- gsub("Añon", "", data[1, ])
row1 <- gsub("Año ", "", row1)
data <- rbind(row1, data[-1, ])
row1 <- c(NA, zoo::na.locf(as.numeric(data[1, ]), fromLast = FALSE))
data <- rbind(row1, data[-1, ])
data <- data[-2, ]
year <- as.character(data[1, ])
trim <-as.character(data[2, ])
yeartrim <- paste0(year,"_", trim)
data <- rbind(yeartrim, data[-1, ])
data <- data[-2, ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]
data <- data[, -((ncol(data)-1):ncol(data))]

data <- data %>%
  mutate(across(-prov_name, as.character)) %>%
  pivot_longer(cols = -prov_name, names_to = "year", values_to = "Total") %>%
    mutate(Total = as.numeric(Total))

data$file <- filename
data$term <- str_extract(data$year, "(?<=_)[0-9]")
data$year <- str_extract(data$year, "^[0-9]{4}")

alldata <- na.omit(rbind(alldata, data))

}

# Open all files for "superficie" ----
file_superf <- c("fisica", "total", "juridi")

alldata <- NA

for(filename in file_superf){

data <- readxl::read_xls(paste0(path,"Series/raw/pc/superf_ccaa_prov_",filename,".XLS"))
data <- data[, -((ncol(data)-1):ncol(data))]
data <- data[-c(1:5), ]
row1 <- gsub("Añon", "", data[1, ])
row1 <- gsub("Año ", "", row1)
data <- rbind(row1, data[-1, ])
row1 <- c(NA, zoo::na.locf(as.numeric(data[1, ]), fromLast = FALSE))
data <- rbind(row1, data[-1, ])
data <- data[-2, ]
year <- as.character(data[1, ])
trim <-as.character(data[2, ])
yeartrim <- paste0(year,"_", trim)
data <- rbind(yeartrim, data[-1, ])
data <- data[-2, ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]
data <- data[, -((ncol(data)-1):ncol(data))]

data <- data %>%
  mutate(across(-prov_name, as.character)) %>%
  pivot_longer(cols = -prov_name, names_to = "year", values_to = "Total") %>%
    mutate(Total = as.numeric(Total))

data$file <- filename
data$term <- str_extract(data$year, "(?<=_)[0-9]")
data$year <- str_extract(data$year, "^[0-9]{4}")

alldata <- na.omit(rbind(alldata, data))

}


# Open all files for "valor de transacciones" ----
file_valor <- c("fisica", "total", "juridi")

alldata <- NA

for(filename in file_valor){

data <- readxl::read_xls(paste0(path,"Series/raw/pc/vs_ccaa_prov_",filename,".XLS"))
data <- data[, -((ncol(data)-1):ncol(data))]
data <- data[-c(1:5), ]
row1 <- gsub("Añon", "", data[1, ])
row1 <- gsub("Año ", "", row1)
data <- rbind(row1, data[-1, ])
row1 <- c(NA, zoo::na.locf(as.numeric(data[1, ]), fromLast = FALSE))
data <- rbind(row1, data[-1, ])
data <- data[-2, ]
year <- as.character(data[1, ])
trim <-as.character(data[2, ])
yeartrim <- paste0(year,"_", trim)
data <- rbind(yeartrim, data[-1, ])
data <- data[-2, ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]
data <- data[, -((ncol(data)-1):ncol(data))]

data <- data %>%
  mutate(across(-prov_name, as.character)) %>%
  pivot_longer(cols = -prov_name, names_to = "year", values_to = "Total") %>%
    mutate(Total = as.numeric(Total))

data$file <- filename
data$term <- str_extract(data$year, "(?<=_)[0-9]")
data$year <- str_extract(data$year, "^[0-9]{4}")

alldata <- na.omit(rbind(alldata, data))

}