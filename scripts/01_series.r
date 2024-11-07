#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Series de Apartamentos Turísticos
#         Data: Encuesta de Ocupación en Apartamentos Turísticos (INE)
#         Serie origen: Encuesta de ocupación en apartamentos turísticos. Nacional, ccaa, provincias, zonas y puntos turísticos

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

# Get Data from INE (Total poblacion)

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
  dplyr::rename(pobtot = Total, date = Periodo)

# Data from INE (Plazas turisticas)

data <- read.delim(paste0(path,"Series/raw/viv_turisticas/eoat_serie_apartamentos_turisticos.csv"), 
  comment.char="#", fileEncoding = "latin1") %>%
  dplyr::select(Provincias, Periodo, Total) %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)))

# Clean data and fill missings in "Total" by doing a linear interpolation between the previous
# and the next non-missing value

data <- data %>%
  mutate(prov_code = sub(" .*", "", ifelse(is.na(Provincias), "", Provincias)),
         prov_name = sub("^[0-9]+ ", "", ifelse(is.na(Provincias), "", Provincias)),
         Year = as.numeric(sub("M.*", "", Periodo)),
         Month = as.numeric(sub(".*M", "", Periodo)),
         Total = as.numeric(Total)) %>%
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  group_by(prov_code) %>%
  mutate(Total = zoo::na.approx(Total, na.rm = FALSE)) %>%
  ungroup() 

# Merge both datasets 
data <- left_join(data, pob, by = c("prov_code", "prov_name", "date")) 

# Get ratio plazas / poblacion total
data$Total = data$Total / (data$pobtot/1000)

# Get total and time series
tsdata <- data %>%
    dplyr::select(prov_code, Total, date) %>%
    spread(key = prov_code, value = Total) %>%
    arrange(date)

decomp <- matrix(ncol = 6, nrow = 0, 
                 dimnames = list(NULL, c("x", "seasonal", "trend", 
                                         "random", "date", "prov_code")))

for (var in names(tsdata)[2:ncol(tsdata)]) {
  data_ts <- tsdata %>%
  dplyr::select(date, !!sym(var)) %>%
  na.omit()
  start_year <- as.numeric(format(min(data_ts$date), "%Y"))
  start_month <- as.numeric(format(min(data_ts$date), "%m"))
  data_ts <- ts(data_ts[[var]], start = c(start_year, start_month), frequency = 12)
  hp_result <- decompose(data_ts, type = "multiplicative")
  df <- as.data.frame(hp_result[c("x", "seasonal", "trend", "random")])
  df$date <- seq(from = as.Date(paste(start_year, start_month, "01", sep = "-")),
         by = "month", length.out = nrow(df))
  df$prov_code <- var
  names(df) <- names(decomp)
  decomp <- rbind(as.matrix(df), decomp)
}

decomp <- as.data.frame(decomp)
decomp$date <- as.Date(decomp$date)

data <- left_join(data, decomp, by = c("date", "prov_code"))

# Update values
data <- data %>%
  mutate(Provincia = as.factor(prov_name)) %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2022-12-31")) %>%
  mutate(Provincia = sub("([^,]+),\\s*(.*)", "\\2 \\1", Provincia)) %>%
  rename(Fecha = date, Valor = trend) %>%
  mutate(across(c(Total, prov_code, Year, Month, pobtot, 
                  x, seasonal, Valor, random), as.numeric))

# Fix Ávila (otherwise it appears after Zaragoza)
data$Provincia <- gsub("^[Áá]", "A", data$Provincia)

# Plot with ggplotly and hchart ----
# ggplotly(ggplot(decomp, aes(x = Fecha, y = Valor, color = Provincia)) +
#                 geom_line() +
#                 labs(title = "Trend by Date", x = "Date", y = "Trend") +
#                 theme_minimal() +
#                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"))

hchart(data, "line", 
  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
  hc_legend(enabled = TRUE) %>%
  hc_xAxis(title = list(text = "Mes - Año")) %>%
  hc_yAxis(title = list(text = "Apartamentos turísticos por 1000 Habitantes (EOAT)")) %>%
  hc_exporting(enabled = FALSE) %>%
  hc_title(text = "Apartamentos turísticos por 1000 Habitantes") %>%
  hc_subtitle(text = "Pedro Salas-Rojo | Datos: Encuesta de Ocupación en Apartamentos Turísticos (INE)") %>%
  htmlwidgets::saveWidget(paste0(path,"Series/plots/apartamentos_turisticos_x1000habitantes_EOAT.html"))
