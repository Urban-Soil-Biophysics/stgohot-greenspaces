# ── 02_Intercept_DW_minvu.R ───────────────────────────────────────────────────
# Goal: Process MINVU park polygons and download Dynamic World at 10m
# resolution clipped to park boundaries only (Sentinel-2 native resolution).
#
# Outputs:
#   data/processed/minvu_santiago_cleaned.rds  — classified park polygons
#   data/stgo_dw/dw_woody_parks_10m.tif        — woody probability at 10m
#   data/stgo_dw/dw_grass_parks_10m.tif        — grass probability at 10m
#
# Note: dplyr::filter() conflicts with the sf object from this geojson.
# Use base R subsetting [df$COL == val, ] for REGION filtering throughout.
# ─────────────────────────────────────────────────────────────────────────────

# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(rgee)
library(stars)

sf_use_s2(FALSE)

# Folders -----------------------------------------------------------------
dir.create("data/stgo_dw",   recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# MINVU park classification -----------------------------------------------
# Source: https://geoide.minvu.cl
# Downloaded via scripts/01_data-download/02_download-minvu-data.R
# Region 13 = Región Metropolitana de Santiago
# Note: dplyr::filter() causes coercion error on this sf object —
# using base R [df$REGION == 13, ] instead throughout this script.
minvu_raw <- st_read("data/geo-data/parques_urbanos_minvu.geojson")

minvu_ready <- minvu_raw[minvu_raw$REGION == 13, ] %>%
  dplyr::mutate(
    greenspace_type = dplyr::case_when(
      str_detect(
        NOMBRE,
        regex("parque", ignore_case = TRUE)
      ) ~ "Urban Park",
      str_detect(
        NOMBRE,
        regex(
          "plaza|plazoleta|jardin|jardines|santuario|estadio|deporte|deportivo|campo|piscina",
          ignore_case = TRUE
        )
      ) ~ "Lawn Park",
      str_detect(
        NOMBRE,
        regex(
          "bandejon|bandejón|rotonda|av\\.?|canal|estero|autopista",
          ignore_case = TRUE
        )
      ) ~ "Roadside",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(greenspace_type)) %>%
  dplyr::mutate(
    park_id = dplyr::row_number(),
    area_m2 = as.numeric(SUPERFICIE),
    name    = as.character(NOMBRE)
  ) %>%
  dplyr::select(park_id, name, area_m2, greenspace_type) %>%
  st_transform(4326)

message("Parks classified: ", nrow(minvu_ready))
print(dplyr::count(sf::st_drop_geometry(minvu_ready), greenspace_type))

# Save processed vector for downstream scripts ----------------------------
saveRDS(minvu_ready, "data/processed/minvu_santiago_cleaned.rds")
message("Saved: data/processed/minvu_santiago_cleaned.rds")

# ROI & park mask for GEE -------------------------------------------------
stgo_bbox <- c(-70.85, -33.65, -70.45, -33.30)

roi <- ee$Geometry$Rectangle(
  coords   = stgo_bbox,
  proj     = "EPSG:4326",
  geodesic = FALSE
)

# Union of all park polygons as GEE mask — clips raster to parks only
parks_mask <- minvu_ready %>%
  st_union() %>%
  sf_as_ee()

# Dynamic World 10m -------------------------------------------------------
# Date window: ±5 days around January 20th 2024
# Median composite across available images in the window
target_date <- ee$Date("2024-01-20")

dw_col <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(roi)$
  filterDate(target_date$advance(-5, "day"), target_date$advance(5, "day"))

# Woody = trees + shrub_and_scrub probability bands combined
woody_ee <- dw_col$map(function(img) {
  img$select("trees")$
    add(img$select("shrub_and_scrub"))$
    rename("woody")
})$median()$clip(parks_mask)

# Grass probability band
grass_ee <- dw_col$select("grass")$median()$clip(parks_mask)

# Download via Drive -------------------------------------------------------
# region = roi keeps spatial extent consistent even though data is
# masked to parks. scale = 10 is Sentinel-2 native resolution.
# ee_as_stars + rast() used consistently — dsn parameter does not exist
# in ee_as_rast and will cause an error if used.

message("Downloading 10m Woody Raster (Parks only)...")
woody_10m <- rast(ee_as_stars(
  image  = woody_ee,
  region = roi,
  scale  = 10,
  via    = "drive"
))
names(woody_10m) <- "woody"
terra::writeRaster(
  woody_10m,
  "data/stgo_dw/dw_woody_parks_10m.tif",
  overwrite = TRUE
)
message("Saved: data/stgo_dw/dw_woody_parks_10m.tif")

message("Downloading 10m Grass Raster (Parks only)...")
grass_10m <- rast(ee_as_stars(
  image  = grass_ee,
  region = roi,
  scale  = 10,
  via    = "drive"
))
names(grass_10m) <- "grass"
terra::writeRaster(
  grass_10m,
  "data/stgo_dw/dw_grass_parks_10m.tif",
  overwrite = TRUE
)
message("Saved: data/stgo_dw/dw_grass_parks_10m.tif")

message("Done.")
plot(woody_10m, main = "High-Res Woody Probability (10m)")
plot(grass_10m, main = "High-Res Grass Probability (10m)")
