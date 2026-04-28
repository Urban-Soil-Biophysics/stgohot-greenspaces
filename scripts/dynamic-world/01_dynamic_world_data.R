# ── 01_Dynamic_world_data.R ───────────────────────────────────────────────────
# Goal: Download Google Dynamic World probability rasters from GEE at 30m
# resolution over Santiago bounding box.
# Outputs:
#   data/geo-data/dw_woody_stgo_30m.tif  — trees + shrub_and_scrub probability
#   data/geo-data/dw_grass_stgo_30m.tif  — grass probability
# These are city-wide rasters used for visualization (hex maps).
# Park-level analysis uses 10m rasters from 02_Intercept_DW_minvu.R instead.
# ─────────────────────────────────────────────────────────────────────────────

# Libraries ---------------------------------------------------------------
library(rgee)
library(terra)
library(sf)
library(tidyverse)
library(stars)

# ROI — Santiago bounding box ---------------------------------------------
stgo_bbox <- c(-70.85, -33.65, -70.45, -33.30)
roi <- ee$Geometry$Rectangle(
  coords   = stgo_bbox,
  proj     = "EPSG:4326",
  geodesic = FALSE
)

# Dates — ±5 days around target -------------------------------------------
target_date <- as.Date("2024-01-20")
start_date  <- ee$Date(as.character(target_date - 5))
end_date    <- ee$Date(as.character(target_date + 5))

# Dynamic World collection ------------------------------------------------
dw_col <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(roi)$
  filterDate(start_date, end_date)

n_imgs <- dw_col$size()$getInfo()
message("Dynamic World images found: ", n_imgs)
if (n_imgs == 0) stop("No images found — widen the date window.")

# Woody: trees + shrub_and_scrub ------------------------------------------
# Median across available images in the date window
woody_ee <- dw_col$map(function(img) {
  img <- ee$Image(img)
  img$select("trees")$
    add(img$select("shrub_and_scrub"))$
    rename("woody")
})$median()$clip(roi)

# Grass -------------------------------------------------------------------
grass_ee <- dw_col$select("grass")$median()$clip(roi)

# Download via Drive -------------------------------------------------------
dir.create("data/geo-data", recursive = TRUE, showWarnings = FALSE)

message("Downloading woody raster (30m)...")
woody_rast <- rast(ee_as_stars(
  image  = woody_ee,
  region = roi,
  scale  = 30,
  via    = "drive"
))
names(woody_rast) <- "woody"
terra::writeRaster(
  woody_rast,
  "data/geo-data/dw_woody_stgo_30m.tif",
  overwrite = TRUE
)

message("Downloading grass raster (30m)...")
grass_rast <- rast(ee_as_stars(
  image  = grass_ee,
  region = roi,
  scale  = 30,
  via    = "drive"
))
names(grass_rast) <- "grass"
terra::writeRaster(
  grass_rast,
  "data/geo-data/dw_grass_stgo_30m.tif",
  overwrite = TRUE
)

message("Done.")
plot(woody_rast, main = "Woody probability: trees + shrubs (30m)")
plot(grass_rast, main = "Grass probability (30m)")
