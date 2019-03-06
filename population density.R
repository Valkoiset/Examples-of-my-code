library(eurostat)
library(dplyr)
library(ggplot2)
library(stringr)
library(rgdal)
library(sp)
library(sf)
library(RColorBrewer)

setwd("C:/Users/Oleksandr/Desktop/Spatial Econometrics/1 lesson/Home")

# check available datasets in eurostats
x <- search_eurostat("")

# download the map
geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "3",
                                   year = 2016, output_class = "spdf")

# Data from Eurostat
current_pop <- get_eurostat("demo_r_d3dens", time_format = "raw", stringsAsFactors = FALSE) %>%
  filter(substr(time, 1, 4) == "2017") %>% 
  dplyr::mutate(cat = cut_to_classes(values, manual = T, decimals = 2,
                                     manual_breaks = c(1, 50, 100, 2000, 13000)))

# Save data to home folder
saveRDS(current_pop, "demo_r_d3dens.rds")

# join data and map
geodata@data <- left_join(geodata@data, current_pop, by = "geo")

# plot
spplot(geodata, zcol = "cat", color.regions = c("dim grey", brewer.pal(n = 5, name = "PiYG")),
       col = "white", usePolypath = FALSE,
       main = "Population density by NUTS 3 region
       (The ratio between the annual average 
       population and the land area, persons per km2)",
       xlim = c(-22, 34), ylim = c(35, 70))

