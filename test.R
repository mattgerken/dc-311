# load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)
library(urbnthemes)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(janitor)
library(naniar)
library(scales)
library(ggrepel) 
library(stringr)
library(utils)
library(tidylog)
library(rrapply)
library(mdthemes)
library(dotenv)
library(tigris)
library(tidycensus)
library(sf)
library(stringi)
library(mapview)
library(gridExtra)
library(leaflet.extras2)
library(ggtext)
library(geojsonsf)
library(jsonlite)
library(gt)
library(gtExtras)

options(scipen = 100)

# get DC tracts 
dc_tracts <- tracts(
  state = "DC",
  year = 2022,
  progress_bar = FALSE
) %>%
  st_transform(crs = 6487)

# load data 
dc_311_2024 <- st_read("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/ServiceRequests/FeatureServer/16/query?outFields=*&where=1%3D1&f=geojson", 
                       quiet = TRUE) %>%
  st_transform(crs = 6487)

# 










