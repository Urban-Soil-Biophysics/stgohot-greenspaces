# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)

sf_use_s2(FALSE)

# Load raster from disk (no GEE needed here) ------------------------------
landsat_raster <- rast("data/geo-data/landsat_lst_deciles_jan2024.tif")

# Load and classify greenspaces -------------------------------------------
# filter(REGION == 13) first, matching STGOHOT order 
minvu <- sf::st_read("data/geo-data/parques_urbanos_minvu.geojson") %>%
  filter(REGION == 13) %>%
  mutate(
    greenspace_type = case_when(
      str_detect(NOMBRE, regex("parque", ignore_case = TRUE)) ~ "Urban Park",
      str_detect(NOMBRE, regex("plaza|plazoleta|jardin|jardines|santuario|estadio|deporte|deportivo|campo|piscina", ignore_case = TRUE)) ~ "Lawn Park",
      str_detect(NOMBRE, regex("bandejon|bandejón|rotonda|av\\.?|canal|estero|autopista", ignore_case = TRUE)) ~ "Roadside",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(greenspace_type) %>%
  mutate(
    park_id = row_number(),
    area    = SUPERFICIE,
    name    = NOMBRE
  ) %>%
  select(park_id, name, area, greenspace_type)

# Match CRS ---------------------------------------------------------------
minvu <- st_transform(minvu, crs(landsat_raster))

# Extract deciles ---------------------------------------------------------

landsat_extraction <- exact_extract(
  landsat_raster,
  minvu,
  include_cols  = c("park_id", "area", "greenspace_type"),
  coverage_area = TRUE
) %>%
  bind_rows() %>%
  rename(decile = value) %>% 
  filter(!is.na(decile)) %>%
  mutate(q_proportion = coverage_area / area) %>%
  filter(q_proportion >= 0.01)    

# Summarise: dominant decile label per park ------------------------------
# ✅ deciles_label in summarise() not mutate() — fixes per-row label bug
intercept_landsat_summary <- landsat_extraction %>%
  group_by(park_id, greenspace_type, area) %>%
  summarise(
    dominant_decile = as.integer(
      names(which.max(tapply(coverage_area, decile, sum)))
    ),
    deciles_label = paste0(
      "Q", sort(unique(as.integer(as.character(decile)))), collapse = "-"
    ),
    .groups = "drop"
  ) %>%
  left_join(
    st_drop_geometry(minvu) %>% select(park_id, name),
    by = "park_id"
  )

# Join geometry back for plotting ----------------------------------------
intercept_landsat_sf <- minvu %>%
  select(park_id) %>%
  left_join(intercept_landsat_summary, by = "park_id") %>%
  st_as_sf()