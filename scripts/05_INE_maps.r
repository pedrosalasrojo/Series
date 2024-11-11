#         Author: Pedro Salas Rojo
#         Date: 11/2024
#         Name of project: Explore Atlas Income by Pablo Guzman
#         Data: https://pablogguz.github.io/ineAtlas/index.html?q=Pablo%20Garc%C3%ADa%20Guzm%C3%A1n#author
#         Serie origen: https://pablogguz.github.io/ineAtlas/index.html?q=Pablo%20Garc%C3%ADa%20Guzm%C3%A1n#author

rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(ineAtlas)

# Get municipality-level income data and tract 
data <- get_atlas(category = "income", level = "tract", cache = TRUE, cache_dir = tempdir())
tract <- get_tract_geom(2022, cache = TRUE, cache_dir = tempdir())

# Merge both
data <- left_join(data, tract, by = c("tract_code", "year"))

# Plot map
ggplot(data, aes(geometry = geom)) +
  geom_sf(aes(fill = net_income_pc), color = NA, linewidth = 0) +
  scale_fill_gradient(low = "#36c8df",
                      high = "tomato",
                      na.value = "grey97",  # Set fill for NA values to white
                      name = "Share of Housing Rents (%)",  # Customize the legend title
                      labels = scales::comma) 
