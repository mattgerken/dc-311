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
library(leaflet)

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
dc_311_2024 <- read.csv("C:/Users/matthew.gerken/Downloads/311_City_Service_Requests_in_2024.csv") %>%
  select(SERVICECODEDESCRIPTION, ADDDATE, RESOLUTIONDATE, WARD, SERVICEORDERSTATUS) %>%
  filter(SERVICEORDERSTATUS == "Closed") %>%
  filter(!is.na(ADDDATE) & !is.na(RESOLUTIONDATE) & ADDDATE != 0 & RESOLUTIONDATE != 0) %>%
  mutate(ADDDATE = ymd_hms(ADDDATE, tz = "UTC"),
         RESOLUTIONDATE = ymd_hms(RESOLUTIONDATE, tz = "UTC"),
         month = month(ADDDATE, label = TRUE, abbr = TRUE),
         year = year(ADDDATE),
         response_time = difftime(RESOLUTIONDATE, ADDDATE, units = "days"),
         response_time = round(as.numeric(response_time), digits = 2)) %>%
  select(-ADDDATE, -RESOLUTIONDATE, -SERVICEORDERSTATUS)

# load data for 2023
dc_311_2023 <- read.csv("C:/Users/matthew.gerken/Downloads/311_City_Service_Requests_in_2023.csv") %>%
  select(SERVICECODEDESCRIPTION, ADDDATE, RESOLUTIONDATE, WARD, SERVICEORDERSTATUS) %>%
  filter(SERVICEORDERSTATUS == "Closed") %>%
  filter(!is.na(ADDDATE) & !is.na(RESOLUTIONDATE) & ADDDATE != 0 & RESOLUTIONDATE != 0) %>%
  mutate(ADDDATE = ymd_hms(ADDDATE, tz = "UTC"),
         RESOLUTIONDATE = ymd_hms(RESOLUTIONDATE, tz = "UTC"),
         month = month(ADDDATE, label = TRUE, abbr = TRUE),
         year = year(ADDDATE),
         response_time = difftime(RESOLUTIONDATE, ADDDATE, units = "days"),
         response_time = round(as.numeric(response_time), digits = 2)) %>%
  select(-ADDDATE, -RESOLUTIONDATE, -SERVICEORDERSTATUS)


# combined spreadsheet
combined_311 <- dc_311_2023 %>%
  rbind(dc_311_2024) 

write.xlsx(combined_311, "C:/Data Strategy Team/311_combined.xlsx")


# census tract summ
tracts_2024 <- st_join(dc_311_2024, dc_tracts, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(GEOID, SERVICECODEDESCRIPTION, year, month) %>%
  summarize(
    total_calls = n(),
    avg_response = mean(response_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(GEOID))

tracts_2023 <- st_join(dc_311_2023, dc_tracts, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(GEOID, SERVICECODEDESCRIPTION, year, month) %>%
  summarize(
    total_calls = n(),
    avg_response = mean(response_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(GEOID))

tracts_all <- rbind(tracts_2023, tracts_2024)

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
  group_by(WARD, SERVICECODEDESCRIPTION, year, month) %>%
  summarize(
    total_calls = n(),
    avg_response = mean(response_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(WARD) & WARD != "Null")

ward_2023 <- dc_311_2023 %>%
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
  group_by(WARD, SERVICECODEDESCRIPTION, year, month) %>%
  summarize(
    total_calls = n(),
    avg_response = mean(response_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(WARD) & WARD != "Null")

ward_all <- rbind(ward_2023, ward_2024)


# map: average response times
set_urbn_defaults(style = "map")

map_data <- tracts_all %>%
  group_by(GEOID) %>%
  summarize(response_avg = weighted.mean(avg_response, total_calls),
            sum_total_calls = sum(total_calls)) %>%
  ungroup()

t1 <- dc_tracts %>%
  full_join(map_data, by = c("GEOID")) %>%
  ggplot() + 
  geom_sf(aes(fill = response_avg)) + 
  scale_fill_gradientn(
    labels = scales::label_comma(),
    name = NULL,
    #labels = c("Lower", "", "Middle", "",  "Higher"),
    na.value = "#d2d2d2"
  ) + 
  geom_sf(data = dc_wards, color = "white", fill = "transparent", linewidth = 1.5) +
  theme(legend.direction = "vertical", legend.box = "vertical",
        plot.caption = element_markdown(hjust = 0, size = 9),
        legend.title = element_text(face = "bold", size = 10)) +
  ggtitle("Average Response Time (in Days)") + 
  labs(caption = paste("**Source:**", "311 data (Open Data DC)"))

t1

# map: # of calls
t2 <- dc_tracts %>%
  full_join(map_data, by = c("GEOID")) %>%
  ggplot() + 
  geom_sf(aes(fill = sum_total_calls)) + 
  scale_fill_gradientn(
    labels = scales::label_comma(),
    name = NULL,
    #labels = c("Lower", "", "Middle", "",  "Higher"),
    na.value = "#d2d2d2"
  ) + 
  geom_sf(data = dc_wards, color = "white", fill = "transparent", linewidth = 1.5) +
  theme(legend.direction = "vertical", legend.box = "vertical",
        plot.caption = element_markdown(hjust = 0, size = 9),
        legend.title = element_text(face = "bold", size = 10)) +
  ggtitle("Total 311 Calls") + 
  labs(caption = paste("**Source:**", "311 data (Open Data DC)"))

t2

grid.arrange(t1, t2, nrow = 1)

# map alternative - interactive
m1<- dc_tracts %>%
  full_join(map_data, by = c("GEOID")) %>%
  mapview(zcol = "sum_total_calls", col.regions = palette_urbn_cyan, 
          layer.name = "Total 311 Calls", legend.pos = "bottomleft")

m2 <- dc_tracts %>%
  full_join(map_data, by = c("GEOID")) %>%
  mapview(zcol = "response_avg", col.regions = palette_urbn_magenta, 
          layer.name = "Average Response Time (in Days)", legend.pos = "topright")

m1| m2

# data table
table_data <- ward_2024 %>%
  group_by(WARD) %>%
  summarize(response_avg = weighted.mean(total_response, total_calls),
            test = sum(total_response * total_calls) / sum(total_calls),
            sum_total_calls = sum(total_calls)) %>%
  ungroup()

# math is not right. Need to take an average and then do a weighted average of averages.



# line charts, over time




# table data 
















