# ── 03_zonal_statistics.R ─────────────────────────────────────────────────────
# Goal: Extract mean woody and grass probability per park polygon from 10m
# Dynamic World rasters. Compute vegetation structure metrics and categories.
# Input:
#   data/processed/minvu_santiago_cleaned.rds
#   data/stgo_dw/dw_woody_parks_10m.tif
#   data/stgo_dw/dw_grass_parks_10m.tif
# Output:
#   data/results/ugs_analysis_10m_utm.rds — one row per park with all metrics
# No GEE needed — reads from disk only.
# ─────────────────────────────────────────────────────────────────────────────

# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)

# Load data ---------------------------------------------------------------
minvu_data   <- readRDS("data/processed/minvu_santiago_cleaned.rds")
woody_raster <- rast("data/stgo_dw/dw_woody_parks_10m.tif")
grass_raster <- rast("data/stgo_dw/dw_grass_parks_10m.tif")

# Match CRS — reproject polygons to raster CRS (UTM 19S) ------------------
minvu_utm    <- st_transform(minvu_data, 32719)
woody_raster <- project(woody_raster, "EPSG:32719")
grass_raster <- project(grass_raster, "EPSG:32719")

# Zonal statistics --------------------------------------------------------
message("Extracting zonal statistics from 10m rasters...")

ugs_analysis <- minvu_utm %>%
  dplyr::mutate(
    mean_woody = exact_extract(woody_raster, ., "mean"),
    mean_grass = exact_extract(grass_raster, ., "mean")
  ) %>%
  dplyr::mutate(
    # Total vegetation index: sum of woody + grass probabilities
    total_veg_idx = mean_woody + mean_grass,

    # Woody ratio: proportion of woody within total vegetation signal
    # Small epsilon (0.0001) avoids division by zero for bare/impervious parks
    woody_ratio = mean_woody / (total_veg_idx + 0.0001),
    grass_ratio = mean_grass / (total_veg_idx + 0.0001),

    # Structural category based on woody dominance
    structure_cat = dplyr::case_when(
      woody_ratio > 0.6 ~ "High Woody Cover",
      woody_ratio < 0.4 ~ "High Herbaceous Cover",
      TRUE              ~ "Mixed/Balanced"
    )
  )

# Summary -----------------------------------------------------------------
message("Parks processed: ", nrow(ugs_analysis))
message("Structure breakdown:")
print(dplyr::count(st_drop_geometry(ugs_analysis), greenspace_type, structure_cat))

# Save --------------------------------------------------------------------
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)
saveRDS(ugs_analysis, "data/results/ugs_analysis_10m_utm.rds")
message("Saved: data/results/ugs_analysis_10m_utm.rds")
