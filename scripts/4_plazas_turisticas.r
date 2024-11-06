#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Plazas turísticas per capita
#         Data: Webscrapping INE, series experimentales,  Medición del número de viviendas turísticas en España y su capacidad
#         Serie origen: Viviendas turísticas, plazas y plazas por vivienda turística. Total nacional, comunidades autónomas y provincias

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)
library(plotly)
library(xts)
library(lubridate)
library(highcharter) 
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

name="Pedro"

if (name=="Pedro"){
  path <- paste0("C:/Users/user/Documents/mispapers/Housing/data/")
} else {
  path <- paste0("-")
}

# Data from INE (Total poblacion)

data <- read.csv(paste0(path,"/Series/raw/serie_poblacion_provincia.csv"), sep=";",
  fileEncoding = "latin1") %>%
  dplyr::select(Provincias, Periodo, Total) %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total))) 

data$Periodo <- dmy(data$Periodo)

pob <- data %>%
  group_by(Provincias) %>%
  complete(Periodo = seq.Date(min(Periodo), max(Periodo), by = "month")) %>%
  fill(Total, .direction = "downup") %>%
  mutate(Total = zoo::na.approx(Total, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(prov_code = sub(" .*", "", ifelse(is.na(Provincias), "", Provincias)),
     prov_name = sub("^[0-9]+ ", "", ifelse(is.na(Provincias), "", Provincias))) %>%
  dplyr::select(-Provincias) %>%
  dplyr::rename(pobtot = Total, date = Periodo)

# Data from INE (Plazas turisticas)

data <- read.csv(paste0(path,"/Series/raw/serie_viviendas_plazas_turisticas_prov_ccaa.csv"), sep=";",
  fileEncoding = "latin1") 
names(data) <- c("total", "ccaa", "prov", "type", "date", "value")

data$date <- ymd(paste0(substr(data$date, 1, 4), "-", substr(data$date, 6, 7), "-01"))

data <- data %>% 
   mutate(prov_code = sub(" .*", "", ifelse(is.na(prov), "", prov)),
   prov_name = sub("^[0-9]+ ", "", ifelse(is.na(prov), "", prov))) %>%
  filter(prov_code != "") %>%
   dplyr::select(-total, -ccaa, - prov, -prov_code) 

# Find mismatched names. They are all Autonomous Regions
mismatched_names <- setdiff(data$prov_name, pob$prov_name)
print(mismatched_names)

    # Merge both datasets 
data <- left_join(data, pob, by = c("prov_name", "date")) %>%
        na.omit() %>%
        filter(prov_name != "Ceuta" & prov_name != "Melilla")  %>%
  mutate(value = as.numeric(gsub("\\.", "", value)))

data$prov_name <- sub("^(.*) \\((.*)\\)$", "\\2 \\1", data$prov_name)

# Get ratio plazas / poblacion total
data$value = data$value / (data$pobtot/1000)

# Rename
data <- data %>%
  dplyr::rename(Provincia = prov_name,
                Fecha = date,
                Valor = value)

# Plot with ggplotly and hchart ----
# ggplotly(ggplot(data, aes(x = Fecha, y = Valor, color = Provincia)) +
#                 geom_line() +
#                 labs(title = "Flujo Rehabilitaciones", x = "Año",
#                  y = "Calificaciones por 1000 habitantes") +
#                 theme_minimal() +
#                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"))

# Plazas
dataplot <- data %>%
  filter(type == "Plazas")

hchart(dataplot, "line", 
                  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
                  hc_legend(enabled = FALSE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  hc_xAxis(title = list(text = "Mes - Año")) %>%
                  hc_yAxis(title = list(text = "Plazas turísticas por 1000 habitantes")) %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/plazas_turisticas_x1000habitantes.html"))

dataplot <- data %>%
  filter(type == "Viviendas turísticas")

hchart(dataplot, "line", 
                  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
                  hc_legend(enabled = TRUE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  hc_xAxis(title = list(text = "Mes - Año")) %>%
                  hc_yAxis(title = list(text = "Viviendas turísticas por 1000 habitantes")) %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/viviendas_turisticas_x1000habitantes.html"))

