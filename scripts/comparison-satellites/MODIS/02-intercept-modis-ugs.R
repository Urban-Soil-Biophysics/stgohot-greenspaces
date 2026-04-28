# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)

sf_use_s2(FALSE)

# Load raster from disk (no GEE needed here) ------------------------------
modis_raster <- rast("data/geo-data/modis_lst_deciles_jan2024.tif")

# Load and classify greenspaces -------------------------------------------
# ✅ filter(REGION == 13) first, matching STGOHOT/Landsat order
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
minvu <- st_transform(minvu, crs(modis_raster))

# Extract deciles ---------------------------------------------------------
# ✅ bind_rows + include_cols — not imap_dfr with list index as park_id
# ✅ rename(decile = value) — exact_extract always outputs "value" column
modis_extraction <- exact_extract(
  modis_raster,
  minvu,
  include_cols  = c("park_id", "area", "greenspace_type"),
  coverage_area = TRUE
) %>%
  bind_rows() %>%
  rename(decile = value) %>%
  filter(!is.na(decile)) %>%
  mutate(q_proportion = coverage_area / area) %>%
  filter(q_proportion >= 0.01)    # ✅ consistent threshold with STGOHOT/Landsat

# Summarise: dominant decile label per park ------------------------------
# ✅ deciles_label in summarise() not mutate()
# ✅ as.character() before as.integer() to avoid factor level index bug
intercept_modis_summary <- modis_extraction %>%
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
# ✅ no first(geometry) — join from minvu sf object
intercept_modis_sf <- minvu %>%
  select(park_id) %>%
  left_join(intercept_modis_summary, by = "park_id") %>%
  st_as_sf()