# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(rgee)
library(stars)

# GEE init ----------------------------------------------------------------
#csource("scripts/comparison-satellites/00_gee_init.R") 

# ROI from StgoHOT footprint ----------------------------------------------
stgohot_raster    <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
stgohot_true      <- mask(!is.na(stgohot_raster), !is.na(stgohot_raster), maskvalues = 0)
stgohot_poly      <- as.polygons(stgohot_true, values = FALSE) %>%
  st_as_sf() %>%
  st_transform(4326)
stgohot_poly_gee  <- sf_as_ee(stgohot_poly)
stgohot_poly_geom <- stgohot_poly_gee$geometry()

# Date range --------------------------------------------------------------
start_date <- "2024-01-01"
end_date   <- "2024-01-31"

# Cloud mask function -----------------------------------------------------
maskL8sr <- function(image) {
  cloudShadowBitMask <- bitwShiftL(1, 4)
  cloudsBitMask      <- bitwShiftL(1, 3)
  qa   <- image$select("QA_PIXEL")
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  image$updateMask(mask)
}

# Landsat 8 collection ----------------------------------------------------
landsat_stgo_collection <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
  filterBounds(stgohot_poly_geom)$
  filterDate(start_date, end_date)$
  map(ee_utils_pyfunc(function(img) {
    maskL8sr(img)$clip(stgohot_poly_geom)
  }))

img_landsat_stgo <- landsat_stgo_collection$
  sort("CLOUD_COVER")$
  first()

# LST in Celsius ----------------------------------------------------------
lst_celcius_landsat <- img_landsat_stgo$
  select("ST_B10")$
  multiply(0.00341802)$
  add(149.0)$
  subtract(273.15)$
  rename("LST_Day_C")

# Deciles using global min/max --------------------------------------------
minmax_landsat <- lst_celcius_landsat$reduceRegion(
  reducer    = ee$Reducer$minMax(),
  geometry   = stgohot_poly_geom,
  scale      = 30,
  bestEffort = TRUE
)

mn_landsat <- ee$Number(minmax_landsat$get("LST_Day_C_min"))
mx_landsat <- ee$Number(minmax_landsat$get("LST_Day_C_max"))
bw_landsat <- mx_landsat$subtract(mn_landsat)$divide(10)

landsat_deciles_ee <- ee$Image(
  lst_celcius_landsat$expression(
    "floor((b(0) - mn) / bw) + 1",
    list(mn = mn_landsat, bw = bw_landsat)
  )
)$toInt()$clamp(1, 10)$rename("decile_landsat")   

# Download ----------------------------------------------------------------
landsat_stars <- ee_as_stars(
  image  = landsat_deciles_ee$clip(stgohot_poly_geom),
  region = stgohot_poly_geom,
  scale  = 30,
  crs    = "EPSG:4326",
  via    = "drive"
)

# Convert and save to disk so scripts 02-04 run independently -------------
landsat_raster <- rast(landsat_stars)
names(landsat_raster) <- "decile_landsat"
landsat_raster[landsat_raster == 0] <- NA

terra::writeRaster(
  landsat_raster,
  "data/geo-data/landsat_lst_deciles_jan2024.tif",
  overwrite = TRUE
)

# Check -------------------------------------------------------------------
plot(landsat_raster, col = hcl.colors(10, "viridis"),
     main = "Landsat LST deciles — January 2024")