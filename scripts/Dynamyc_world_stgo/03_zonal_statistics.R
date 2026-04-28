# 1. Libraries ------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)

# 2. Data Loading ---------------------------------------------------------
minvu_data <- readRDS("data/processed/minvu_santiago_cleaned.rds")
woody_raster <- rast("data/stgo_dw/dw_woody_parks_10m.tif")
grass_raster <- rast("data/stgo_dw/dw_grass_parks_10m.tif")

# 3. Spatial Transformation -----------------------------------------------
minvu_utm <- st_transform(minvu_data, 32719)

# 4. Zonal Statistics Extraction ------------------------------------------
message("Extracting zonal statistics from 10m rasters...")

extraction_results <- minvu_utm %>%
  mutate(
    mean_woody = exact_extract(woody_raster, ., "mean"),
    mean_grass = exact_extract(grass_raster, ., "mean")
  )

# 5. Metrics & Categorization ---------------------------------------------
ugs_analysis <- extraction_results %>%
  mutate(
    total_veg_idx = mean_woody + mean_grass,
    woody_ratio   = mean_woody / (total_veg_idx + 0.0001),
    grass_ratio   = mean_grass / (total_veg_idx + 0.0001),
    structure_cat = case_when(
      woody_ratio > 0.6  ~ "High Woody Cover",
      woody_ratio < 0.4  ~ "High Herbaceous Cover",
      TRUE               ~ "Mixed/Balanced"
    )
  )

# 6. Save Analytical Output -----------------------------------------------
if(!dir.exists("data/results")) dir.create("data/results", recursive = TRUE)
saveRDS(ugs_analysis, "data/results/ugs_analysis_10m_utm.rds")