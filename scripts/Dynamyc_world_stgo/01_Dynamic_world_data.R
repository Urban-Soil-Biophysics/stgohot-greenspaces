# 1. Libraries ------------------------------------------------------------
library(rgee)
library(terra)
library(sf)
library(tidyverse)

# 2. Inicialize GEE -------------------------------------------------------
rgee::ee_clean_user_credentials("agustincoddoudiaz")
rgee::ee_Initialize(
  user = "agustincoddoudiaz", 
  drive = TRUE
)
# 3. ROI (Region of Interest) ---------------------------------------------
# We use the BBOX from the study area
stgo_bbox <- c(-70.85, -33.65, -70.45, -33.30) 

roi <- ee$Geometry$Rectangle(
  coords   = stgo_bbox,
  proj     = "EPSG:4326",
  geodesic = FALSE
)

# 4. Dynamic World Data (2024) --------------------------------------------
target_date <- as.Date("2024-01-20")
start_date  <- ee$Date(as.character(target_date))$advance(-5, "day")
end_date    <- ee$Date(as.character(target_date))$advance(5, "day")

# Load collection and filter
dw_col <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")$
  filterBounds(roi)$
  filterDate(start_date, end_date)

# 4.1. Woody Cover Layer (Trees + Shrubs)
# We sum both probabilities to characterize woody structure
woody_ee <- dw_col$map(function(img) {
  return(img$select("trees")$add(img$select("shrub_and_scrub"))$rename("woody"))
})$median()$clip(roi)

# 4.2. Herbaceous Cover Layer (Grass)
grass_ee <- dw_col$select("grass")$median()$clip(roi)

# 5. Export to Local Raster (SpatRaster) ----------------------------------
# Scale = 30m for city-wide analysis (optimized for memory)
# Scale = 10m would be native resolution but much heavier

message("Downloading Woody Raster...")
woody_rast <- ee_as_rast(
  image  = woody_ee,
  region = roi,
  scale  = 30, 
  dsn    = "data/geo-data/dw_woody_stgo_30m.tif"
)

message("Downloading Grass Raster...")
grass_rast <- ee_as_rast(
  image  = grass_ee,
  region = roi,
  scale  = 30,
  dsn    = "data/geo-data/dw_grass_stgo_30m.tif"
)

# 6. Basic Validation Plot ------------------------------------------------
plot(woody_rast, main = "Woody Probability (Trees + Shrubs)")
plot(grass_rast, main = "Herbaceous Probability (Grass)")