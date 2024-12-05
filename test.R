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

# get DC wards
dc_wards <- st_read("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/53/query?outFields=*&where=1%3D1&f=geojson", quiet = TRUE) %>%
  st_transform(crs = 6487) %>%
  select(WARD, NAME, GEOID, geometry)

# load data 
dc_311_2024 <- st_read("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/ServiceRequests/FeatureServer/16/query?outFields=*&where=1%3D1&f=geojson", 
                       quiet = TRUE) %>%
  select(SERVICECODEDESCRIPTION, ADDDATE, RESOLUTIONDATE, WARD, SERVICEORDERSTATUS) %>%
  filter(SERVICEORDERSTATUS == "Closed") %>%
  filter(!is.na(ADDDATE) & !is.na(RESOLUTIONDATE) & ADDDATE != 0 & RESOLUTIONDATE != 0) %>%
  mutate(ADDDATE = ADDDATE/1000,
         ADDDATE = as.POSIXct(ADDDATE, origin = "1970-01-01", tz = ),
         RESOLUTIONDATE = RESOLUTIONDATE/1000,
         RESOLUTIONDATE = as.POSIXct(RESOLUTIONDATE, origin = "1970-01-01", tz = ),
         month = month(ADDDATE, label = TRUE, abbr = TRUE),
         year = year(ADDDATE),
         response_time = difftime(RESOLUTIONDATE, ADDDATE, units = "days"),
         response_time = round(as.numeric(response_time), digits = 2)) %>%
  select(-ADDDATE, -RESOLUTIONDATE, -SERVICEORDERSTATUS) %>%
  st_transform(crs = 6487)

# census tract summ
tracts_2024 <- st_join(dc_311_2024, dc_tracts, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(GEOID, year, month) %>%
  summarize(
    total_calls = n(),
    total_response = sum(response_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(GEOID))

# ward month year summ
ward_2024 <- dc_311_2024 %>%
  st_drop_geometry() %>%
  # recategorize ward spelled out
  mutate(WARD = case_when(WARD == "Ward 1" ~ "1",
                          WARD == "Ward 2" ~ "2",
                          WARD == "Ward 3" ~ "3",
                          WARD == "Ward 4" ~ "4",
                          WARD == "Ward 5" ~ "5",
                          WARD == "Ward 6" ~ "6",
                          WARD == "Ward 7" ~ "7",
                          WARD == "Ward 8" ~ "8",
                          TRUE ~ WARD)) %>%
  group_by(WARD, year, month) %>%
  summarize(
    total_calls = n(),
    total_response = sum(response_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(WARD) & WARD != "Null")









