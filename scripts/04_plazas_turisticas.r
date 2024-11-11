#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Plazas turísticas per capita
#         Data: Webscrapping INE, series experimentales,  Medición del número de viviendas turísticas en España y su capacidad
#         Serie origen: Viviendas turísticas, plazas y plazas por vivienda turística. Total nacional, comunidades autónomas y provincias

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(mapSpain)
library(readxl)
library(plotly)
library(htmlwidgets)
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

# Data from INE (Total poblacion)  Provincias -----

pob <- read.csv(paste0(path,"/Series/raw/other/serie_poblacion_provincia.csv"), 
  sep=";", fileEncoding = "latin1") %>%
  dplyr::select(Provincias, Periodo, Total) %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)),
         Periodo = dmy(Periodo)) %>%
  group_by(Provincias) %>%
  complete(Periodo = seq.Date(min(Periodo), max(Periodo), by = "month")) %>%
  fill(Total, .direction = "downup") %>%
  mutate(Total = zoo::na.approx(Total, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(prov_code = sub(" .*", "", ifelse(is.na(Provincias), "", Provincias)),
     prov_name = sub("^[0-9]+ ", "", ifelse(is.na(Provincias), "", Provincias))) %>%
  dplyr::select(-Provincias) %>%
  dplyr::rename(pobtot = Total, date = Periodo) %>%
  mutate(prov_name = recode(prov_name,
    "Araba/Alava" = "Araba/Álava",
    "Balears (Illes)" = "Balears, Illes",
    "Coruña (A)" = "Coruña, A",
    "Rioja (La)" = "Rioja, La",
    "Madrid (Comunidad de)" = "Madrid",
    "Murcia (Región de)" = "Murcia",
    "Navarra (Comunidad Foral de)" = "Navarra",
    "Asturias (Principado de )" = "Asturias",
    "Palmas (Las)" = "Palmas, Las"))
    
# Data from INE (Plazas turisticas), Provincias and CCAA ----

data <- read.csv2(paste0(path,"/Series/raw/viv_turisticas/viviendas_turisticas_ccaa_prov_total.csv"), 
  sep=";",  fileEncoding = "UTF-8") 
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

# Fix Ávila (otherwise it appears after Zaragoza)
data$Provincia <- gsub("^[Áá]", "A", data$Provincia)

# Plot with ggplotly and hchart 
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
                  hc_legend(enabled = TRUE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  hc_xAxis(title = list(text = "Mes - Año")) %>%
                  hc_yAxis(title = list(text = "Plazas turísticas por 1000 habitantes")) %>%
                  hc_title(text = "Plazas en viviendas turísticas por 1000 Habitantes") %>%
                  hc_subtitle(text = "Pedro Salas-Rojo | Datos: Instituto Nacional de Estadística") %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/plazas_turisticas_x1000habitantes.html"))

dataplot <- data %>%
  filter(type == "Viviendas turísticas")

hchart(dataplot, "line", 
                  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
                  hc_legend(enabled = TRUE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  hc_xAxis(title = list(text = "Mes - Año")) %>%
                  hc_yAxis(title = list(text = "Viviendas turísticas por 1000 habitantes")) %>%
                  hc_title(text = "Viviendas turísticas por 1000 Habitantes") %>%
                  hc_subtitle(text = "Pedro Salas-Rojo | Datos: Instituto Nacional de Estadística") %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/viviendas_turisticas_x1000habitantes.html"))

# Data from INE (Total poblacion) Municipalties -----

pob <- read_xlsx(paste0(path,"/Series/raw/other/pobmun/pobmun23.xlsx")) 
names(pob) <- c("cpro", "province", "cmun", "prov_name", "pobtot", "male", "female")
pob <- pob[-1,]
pob <- pob %>%
  dplyr::select(cpro, cmun, pobtot)

# Data from INE (Plazas turisticas), Municipalties ----
munic <- esp_get_munic()

data <- read.csv2(paste0(path,"/Series/raw/viv_turisticas/viviendas_turisticas_municipios_total.csv"), 
  sep=";",  fileEncoding = "UTF-8") 
names(data) <- c("total", "ccaa", "prov", "cmun", "type", "date", "value")
data$cpro <- substr(data$cmun, 1, 2)
data$muniname <- sub("^\\d{5} ", "", data$cmun)
data$prov_name <- sub("^(.*) \\((.*)\\)$", "\\2 \\1", sub("^\\d{5} ", "", data$prov))
data$cmun <- substr(data$cmun, 3, 5)
data$date <- ymd(paste0(substr(data$date, 1, 4), "-", substr(data$date, 6, 7), "-01"))

# Merge both datasets 
data <- left_join(data, pob, by = c("cpro", "cmun")) %>%
        na.omit() %>%
  mutate(value = as.numeric(gsub("\\.", "", value)))

data <- left_join(data, munic, by = c("cpro", "cmun")) 

# Get ratio plazas / poblacion total
data$value = data$value / (as.numeric(data$pobtot)/1000)

# Rename
data <- data %>%
  dplyr::rename(Provincia = prov_name,
                Fecha = date,
                Valor = value)

dataplot <- data %>%
  filter(type == "Plazas")     %>%
  filter(Fecha == "2024M08") %>%
  na.omit() 

p <-  ggplot(dataplot, aes(geometry = geometry)) +
    geom_sf(aes(fill = Valor), color = NA, linewidth = 0) +
    geom_sf(data = data, fill = NA, color = "black") +
    scale_fill_gradient(low = "darkgreen", 
                        high = "tomato",
                        na.value = "grey97",  # Set fill for NA values to white
                        name = "Share of Housing Rents (%)",  # Customize the legend title
                        labels = scales::comma) +
    labs(title = "Housing Rents by Municipalities with more than 5000 inhabitants (2021)") + 
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, color = "black"),  # Center and color the title
          text = element_text(color = "black"),
          legend.position = "bottom",
          legend.title = element_text(hjust = 0.5),  # Center the legend title
          legend.title.position = "top",
          legend.text = element_text(size = 8),  # Adjust legend text size
          legend.key.width = unit(30, "pt"))

saveWidget(ggplotly(p), file = paste0(path,"Series/plots/plazas_turisticas_munic_x1000habitantes.html"));