# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(rgee)
library(stars)
library(viridis)

# ROI from StgoHOT footprint ----------------------------------------------
stgohot_raster    <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
stgohot_true      <- mask(!is.na(stgohot_raster), !is.na(stgohot_raster), maskvalues = 0)
stgohot_poly      <- as.polygons(stgohot_true, values = FALSE) %>%
  st_as_sf() %>%
  st_transform(4326)
stgohot_poly_gee  <- sf_as_ee(stgohot_poly)
stgohot_poly_geom <- stgohot_poly_gee$geometry()

# Date of analysis --------------------------------------------------------
stgohot_day <- ee$Date(sprintf("%s", as.Date("2024-01-20")))

# MODIS collection --------------------------------------------------------
modis_stgo <- ee$ImageCollection("MODIS/061/MOD11A1")$
  filterDate(stgohot_day, stgohot_day$advance(1, "day"))$
  filterBounds(stgohot_poly_gee)$
  map(ee_utils_pyfunc(function(img) {
    good <- img$select("QC_Day")$bitwiseAnd(3)$lte(1)
    img$updateMask(good)$clip(stgohot_poly_geom)
  }))

img_modis_stgo <- modis_stgo$mosaic()

# LST in Celsius ----------------------------------------------------------
modis_celcius <- img_modis_stgo$
  select("LST_Day_1km")$
  multiply(0.02)$
  subtract(273.15)$
  rename("LST_Day_C")

# Deciles using global min/max --------------------------------------------
minmax_modis <- modis_celcius$reduceRegion(
  reducer    = ee$Reducer$minMax(),
  geometry   = stgohot_poly_geom,
  scale      = 1000,
  maxPixels  = 1e13,
  tileScale  = 4,
  bestEffort = FALSE
)

mn_modis <- ee$Number(minmax_modis$get("LST_Day_C_min"))
mx_modis <- ee$Number(minmax_modis$get("LST_Day_C_max"))
bw_modis <- mx_modis$subtract(mn_modis)$divide(10)

modis_deciles_ee <- ee$Image(
  modis_celcius$expression(
    "floor((b(0) - mn) / bw) + 1",
    list(mn = mn_modis, bw = bw_modis)
  )
)$toInt()$clamp(1, 10)$rename("decile_modis")    # ✅ consistent naming

# Download ----------------------------------------------------------------
modis_stars <- ee_as_stars(
  image  = modis_deciles_ee$clip(stgohot_poly_geom),
  region = stgohot_poly_geom,
  scale  = 1000,
  crs    = "EPSG:4326",
  via    = "drive"
)

# Convert and save to disk so scripts 02-04 run independently -------------
modis_raster <- rast(modis_stars)
names(modis_raster) <- "decile_modis"
modis_raster[modis_raster == 0] <- NA

dir.create("data/geo-data", recursive = TRUE, showWarnings = FALSE)
terra::writeRaster(
  modis_raster,
  "data/geo-data/modis_lst_deciles_jan2024.tif",
  overwrite = TRUE
)

# Check -------------------------------------------------------------------
plot(modis_raster, col = viridis(10),
     main = "MODIS LST deciles — January 20th 2024")
