#         Author: Pedro Salas Rojo
#         Date: 11/2023
#         Name of project: Series Apartamentos Turísticos

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

data <- read.delim(paste0(path,"Series/raw/serie_plazas_turisticas.csv"), 
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
data$Total = data$Total / data$pobtot

# Get only total and time series
tsdata <- data %>%
    dplyr::select(prov_code, Total, date) %>%
    spread(key = prov_code, value = Total) %>%
    arrange(date)

decomp <- data.frame(x = numeric(), seasonal = numeric(),
           trend = numeric(), random = numeric(),
           date = as.Date(character()),
           prov_code = character())

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
  decomp <- rbind(as.matrix(df), as.matrix(decomp))
}

decomp <- as.data.frame(decomp)
decomp <- decomp %>%
  mutate(across(-c(date, prov_code), as.numeric),
         Fecha = as.Date(date),
         Valor = round(trend, 5)) 

prov <- data %>%
  dplyr::select(prov_code, prov_name) %>%
  distinct()

decomp <- left_join(decomp, prov, by = "prov_code") %>%
  mutate(Provincia = as.factor(prov_name)) %>%
  filter(Fecha >= as.Date("2015-01-01"))

# Plot
 ggplotly(ggplot(decomp, aes(x = Fecha, y = Valor, color = Provincia)) +
                 geom_line() +
                 labs(title = "Trend by Date", x = "Date", y = "Trend") +
                 theme_minimal() +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"))

hchart(decomp, "line", 
                  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
                  hc_legend(enabled = FALSE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/plazas_per_capita.html"))
