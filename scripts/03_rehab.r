#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Nuevas Calificaciones de Vivienda Protegida

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

data <- read.csv(paste0(path,"/Series/raw/serie_poblacion_provincia_anual.csv"), sep=";",
  fileEncoding = "latin1") %>%
  dplyr::select(Provincias, Periodo, Total) %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total))) 

pob <- data %>%
  group_by(Provincias) %>%
  fill(Total, .direction = "downup") %>%
  mutate(Total = zoo::na.approx(Total, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(prov_code = sub(" .*", "", ifelse(is.na(Provincias), "", Provincias)),
     prov_name = sub("^[0-9]+ ", "", ifelse(is.na(Provincias), "", Provincias))) %>%
  dplyr::select(-Provincias) %>%
  dplyr::rename(pobtot = Total, date = Periodo)

# Create a vector of modifications: names in pob to match data
pob <- pob %>%
  mutate(prov_name = case_when(
    prov_name == "Araba/Álava" ~ "Araba/Alava",
    prov_name == "Asturias" ~ "Asturias (Principado de )",
    prov_name == "Balears, Illes" ~ "Balears (Illes)",
    prov_name == "Coruña, A" ~ "Coruña (A)",
    prov_name == "Navarra" ~ "Navarra (Comunidad F. de)",
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

# Data from INE (Plazas turisticas)

data <- read_excel(paste0(path,"Series/raw/Rehabilitación_flujo.xlsx"), sheet = 2) 
data <- data[-c(1:4), ]
colnames(data) <- data[1, ]
data <- data[-1, ]
names(data)[1] <- "prov_name"

data <- data %>%
  mutate(across(-prov_name, as.character)) %>%
  pivot_longer(cols = -prov_name, names_to = "date", values_to = "Total") %>%
    mutate(Total = as.numeric(Total),
           date = as.numeric(date)) %>%
    na.omit()
    
# Find mismatched names. They are all Autonomous Regions
mismatched_names <- setdiff(data$prov_name, pob$prov_name)
print(mismatched_names)

# Merge both datasets 
data <- left_join(data, pob, by = c("prov_name", "date")) %>%
        na.omit() %>%
        filter(prov_name != "Ceuta" & prov_name != "Melilla")

data$prov_name <- sub("^(.*) \\((.*)\\)$", "\\2 \\1", data$prov_name)

# Get ratio new houses / poblacion total (in thousands)
data$Total = data$Total / (data$pobtot/1000)

# Rename
data <- data %>%
  dplyr::rename(Provincia = prov_name,
                Fecha = date,
                Valor = Total)

# Plot with ggplotly and hchart ----
# ggplotly(ggplot(data, aes(x = Fecha, y = Valor, color = Provincia)) +
#                 geom_line() +
#                 labs(title = "Flujo Rehabilitaciones", x = "Año",
#                  y = "Calificaciones por 1000 habitantes") +
#                 theme_minimal() +
#                 theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"))

hchart(data, "line", 
                  hcaes(x = Fecha, y = Valor, group = Provincia)) %>%
#                  hc_legend(enabled = FALSE) %>%
                  hc_exporting(enabled = FALSE) %>%
                  hc_xAxis(title = list(text = "Año")) %>%
                  hc_yAxis(title = list(text = "Rehabilitacion del parque residencial protegido, por 1000 habitantes")) %>%
                  htmlwidgets::saveWidget(paste0(path,"Series/plots/rehabilitacion_habitantes.html"))
