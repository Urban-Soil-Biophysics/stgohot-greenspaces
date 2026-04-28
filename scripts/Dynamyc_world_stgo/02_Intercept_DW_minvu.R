# 1. Libraries ------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(rgee)

# 2. Settings & Folders ---------------------------------------------------
sf_use_s2(FALSE)
if (!dir.exists("data/stgo_dw")) dir.create("data/stgo_dw", recursive = TRUE)
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# 3. Process MINVU Vector Data (Zones) ------------------------------------
# This must be done first to create the GEE mask
minvu_raw <- st_read("data/geo-data/parques_urbanos_minvu.geojson")

minvu_ready <- minvu_raw %>%
  filter(REGION == 13) %>% 
  mutate(
    greenspace_type = case_when(
      str_detect(NOMBRE, regex("parque", ignore_case = TRUE)) ~ "Urban Park",
      str_detect(NOMBRE, regex("plaza|plazoleta|jardin|jardines|santuario|estadio|deporte|deportivo|campo|piscina", ignore_case = TRUE)) ~ "Lawn Park",
      str_detect(NOMBRE, regex("bandejon|bandejón|rotonda|av|av.|canal|estero|autopista", ignore_case = TRUE)) ~ "Roadside",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(greenspace_type) %>%
  mutate(
    park_id = row_number(),
    area_m2 = SUPERFICIE,
    name    = NOMBRE
  ) %>%
  select(park_id, name, area_m2, greenspace_type) %>%
  st_transform(4326)

# Save processed vector for Script 03
saveRDS(minvu_ready, "data/processed/minvu_santiago_cleaned.rds")

# 4. GEE Initialization ---------------------------------------------------
# ee_Initialize(user = "username", drive = TRUE)

# 5. ROI & Park Geometry for Masking --------------------------------------
stgo_bbox <- c(-70.85, -33.65, -70.45, -33.30) 

# Convert BBox to EE object
roi <- ee$Geometry$Rectangle(
  coords   = stgo_bbox,
  proj     = "EPSG:4326",
  geodesic = FALSE
)

# Convert all park polygons to a single EE Mask
parks_mask <- minvu_ready %>% 
  st_union() %>% 
  sf_as_ee()

# 6. Dynamic World Processing (10m Resolution) ---------------------------
target_date <- ee$Date("2024-01-20")

dw_col <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(roi)$
  filterDate(target_date$advance(-5, "day"), target_date$advance(5, "day"))

# 6.1. Woody Layer (Trees + Shrubs)
woody_ee <- dw_col$map(function(img) {
  return(img$select("trees")$add(img$select("shrub_and_scrub"))$rename("woody"))
})$median()$clip(parks_mask)

# 6.2. Herbaceous Layer (Grass)
grass_ee <- dw_col$select("grass")$median()$clip(parks_mask)

# 7. Export High-Res Rasters ----------------------------------------------
# Exporting at 10m scale (Sentinel-2 native resolution)
message("Downloading 10m Woody Raster (Parks only)...")
woody_10m <- ee_as_rast(
  image  = woody_ee,
  region = roi, # We use roi to keep the spatial extent consistent
  scale  = 10, 
  dsn    = "data/stgo_dw/dw_woody_parks_10m.tif"
)

message("Downloading 10m Grass Raster (Parks only)...")
grass_10m <- ee_as_rast(
  image  = grass_ee,
  region = roi,
  scale  = 10,
  dsn    = "data/stgo_dw/dw_grass_parks_10m.tif"
)

# 8. Visual Check ---------------------------------------------------------
plot(woody_10m, main = "High-Res Woody Probability (10m)")