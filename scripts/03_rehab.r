#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Nuevas Calificaciones de Vivienda Protegida
#         Data: Rehabilitación vivienda protegida, Ministerio de Transportes y Movilidad Sostenible (2024)
#         Serie origen: 2.6. Número de aprobaciones definitivas. Planes estatales y planes autonómicos

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
  mutate(prov_name = case_when(
    prov_name == "Araba/Álava" ~ "Araba/Alava",
    prov_name == "Asturias" ~ "Asturias (Principado de )",
    prov_name == "Balears, Illes" ~ "Balears (Illes)",
    prov_name == "Coruña, A" ~ "Coruña (A)",
    prov_name == "Navarra" ~ "Navarra (Comunidad Foral de)",
    prov_name == "Palmas, Las" ~ "Palmas (Las)",
    prov_name == "Rioja, La" ~ "Rioja (La)",
    prov_name == "Valencia/València" ~ "Valencia/València",
    prov_name == "Andalucía" ~ "Andalucía",
    prov_name == "Aragón" ~ "Aragón",
    prov_name == "Canarias" ~ "Canarias",
    prov_name == "Castilla y León" ~ "Castilla y León",
    prov_name == "Castilla-La Mancha" ~ "Castilla-La Mancha (1)",
    prov_name == "Cataluña" ~ "Cataluña",
    prov_name == "Comunitat Valenciana" ~ "Comunitat Valenciana",
    prov_name == "Extremadura" ~ "Extremadura",
    prov_name == "Galicia" ~ "Galicia",
    prov_name == "Madrid" ~ "Madrid (Comunidad de)",
    prov_name == "Murcia" ~ "Murcia (Región de)",
    prov_name == "País Vasco" ~ "País Vasco",
    prov_name == "TOTAL" ~ "TOTAL NACIONAL",
    TRUE ~ prov_name))

# Data from Ministerio
data <- read_excel(paste0(path,"Series/raw/flujos_calif_rehab/rehab_estat_auton.xls")) 
data <- data[-c(1:4), ]
row1 <- gsub(" por meses", "", data[1, ])
data <- rbind(row1, data[-1, ])
row1 <- c(NA, zoo::na.locf(as.numeric(data[1, ]), fromLast = FALSE))
data <- rbind(row1, data[-1, ])
year <- as.numeric(data[1, ])
month <-as.character(data[2, ])
yearmonth <- paste0(year, month)
data <- rbind(yearmonth, data[-1, ])
data <- data[-2, ]
colnames(data) <- data[1, ]
names(data)[1] <- "prov_name"
data <- data[-1, ]

data <- data %>%
  mutate(across(-prov_name, as.character)) %>%
  pivot_longer(cols = -prov_name, names_to = "date", values_to = "Total") %>%
    mutate(Total = as.numeric(Total)) 
    
data <- data[str_count(data$prov_name, '\\S+') <= 10, ]

months_spa <- c("Ene.", "Feb.", "Mar.", "Abr.", "May.", "Jun.", "Jul.", "Ago.", "Sep.", "Oct.", "Nov.", "Dic.")
months_eng <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
for (i in 1:length(months_spa)) {
  data$date <- gsub(months_spa[i], months_eng[i], data$date)
}
data$year <- as.numeric(substring(data$date, 1, 4))    
data$month <- substring(data$date, 5, 7)  
data$date <- mdy(paste(data$month, "01", data$year))

# Find mismatched names. They are all Autonomous Regions
mismatched_names <- setdiff(data$prov_name, pob$prov_name)
print(mismatched_names)

# Merge both datasets 
data <- left_join(data, pob, by = c("prov_name", "date")) %>%
        na.omit() %>%
        filter(prov_name != "Ceuta" & prov_name != "Melilla")

data$prov_name <- sub("^(.*) \\((.*)\\)$", "\\2 \\1", data$prov_name)
data$prov_name[data$prov_name == "Comunidad Foral de Navarra"] <- "Comunidad F. de Navarra"

# Get ratio new houses / poblacion total (in thousands)
data$Total_reference <- data$Total
data$Total = data$Total / (data$pobtot/1000)

# Rename
data <- data %>%
  dplyr::rename(Provincia = prov_name,
                Fecha = date,
                Valor = Total)

# Fix Ávila (otherwise it appears after Zaragoza)
data$Provincia <- gsub("^[Áá]", "A", data$Provincia)

# Plot with ggplotly and hchart ----
# ggplotly(ggplot(data, aes(x = Fecha, y = Valor, color = Provincia)) +
#                 geom_line() +
#                 labs(title = "Flujo Rehabilitaciones", x = "Año",
#                  y = "Calificaciones por 1000 habitantes") +
#                 theme_minimal() +
#                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"))

hchart(data, "line", 
                  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
                  hc_legend(enabled = TRUE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  hc_xAxis(title = list(text = "Año")) %>%
                  hc_yAxis(title = list(text = "Rehabilitación de vivienda protegida por 1000 habitantes")) %>%
                  hc_title(text = "Flujo de rehabilitaciones de vivienda protegida por 1000 Habitantes") %>%
                  hc_subtitle(text = "Pedro Salas-Rojo | Datos: Ministerio de Transportes y Movilidad Sostenible") %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/flujo_rehabilitacion_x1000habitantes.html"))

# Stock

data<-data %>%
  group_by(Provincia) %>%
  mutate(Stock = cumsum(Total_reference)/(pobtot/1000)) %>%
  ungroup()

hchart(data, "line", 
          hcaes(x = Fecha, y = Stock, group = Provincia)) %>%
          hc_legend(enabled = TRUE) %>%
          hc_exporting(enabled = FALSE) %>%
          hc_xAxis(title = list(text = "Año")) %>%
          hc_yAxis(title = list(text = "Rehabilitación por 1000 habitantes (Indexado a 0 = 01/01/2008)")) %>%
          hc_title(text = "Stock de rehabilitaciones de vivienda protegida por 1000 Habitantes") %>%
          hc_subtitle(text = "Pedro Salas-Rojo | Datos: Ministerio de Transportes y Movilidad Sostenible") %>%
          htmlwidgets::saveWidget(paste0(path,"Series/plots/stock_rehabilitaciones_x1000habitantes.html"))
