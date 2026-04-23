# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)
library(rgee)
library(reticulate)
library(stars)
library(ggspatial)
library(ggmap)
library(scales)
library(viridis)

# Goal --------------------------------------------------------------------

# to generate an object based on LST decile analysis from MODIS
# Day of analysis 20 - 01 - 2024
# Geometry based on stgohot

# Open GEE ----------------------------------------------------------------
# ee_Authenticate()  # Account GEE

ee_Initialize() # open GEE

# Define day of analysis --------------------------------------------------
# GEE objects do not process dates as R
# Define date a sprintf day

stgohot_day <- ee$Date(sprintf("%s", as.Date("2024-01-20")))

# Define clip -------------------------------------------------------------
stgohot_raster <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
crs(stgohot_raster) # EPSG:32719 - WGS 84 / UTM 19S

# Clip according StgoHOT --------------------------------------------------
# First load any Stgohot raster and create a mask/footprint as polygon
stgohot_true <-
  mask(!is.na(stgohot_raster), !is.na(stgohot_raster), maskvalues = 0)

# Get polygon
stgohot_poly <- as.polygons(stgohot_true, values = FALSE) %>%
  st_as_sf() %>%
  st_transform(4326)

# Transform to format ee and create geometry object
stgohot_poly_gee <- sf_as_ee(stgohot_poly)
stgohot_poly_geom <- stgohot_poly_gee$geometry()

# Get MODIS  --------------------------------------------------------------

modis_stgo <-
  ee$ImageCollection("MODIS/061/MOD11A1")$
    filterDate(stgohot_day, stgohot_day$advance(1, "day"))$
    filterBounds(stgohot_poly_gee)$
    map(ee_utils_pyfunc(function(img) {
    good <- img$select("QC_Day")$bitwiseAnd(3)$lte(1)
    img$updateMask(good)$clip(stgohot_poly_geom)
  }))

img_modis_stgo <- modis_stgo$mosaic() # set of tiles

# Check projection --------------------------------------------------------

img_modis_stgo$projection()$getInfo() # EPSG:4326
img_modis_stgo$select("LST_Day_1km")$projection()$getInfo() # EPSG:4326

# Calculate LST -----------------------------------------------------------
# Celcius

modis_celcius <- img_modis_stgo$
  select("LST_Day_1km")$multiply(0.02)$
  subtract(273.15)$
  rename("LST_Day_C")

# Get min and max for split deciles -------------------------------------

minmax_modis <- modis_celcius$reduceRegion(
  reducer = ee$Reducer$minMax(),
  geometry = stgohot_poly_geom,
  scale = 1000,
  maxPixels = 1e13, tileScale = 4, bestEffort = FALSE
)

# Extract min and max modis -----------------------------------------------

mn_modis <- ee$Number(minmax_modis$get("LST_Day_C_min")) # min C
mx_modis <- ee$Number(minmax_modis$get("LST_Day_C_max")) # max C
bw_modis <- mx_modis$subtract(mn_modis)$divide(10) # Bins weights (min - max)/10

# Create a image using deciles --------------------------------------------

modis_deciles <- ee$Image(
  modis_celcius$expression(
    "floor((b(0) - mn) / bw) + 1",
    list(mn = mn_modis, bw = bw_modis)
  )
)$toInt()$clamp(1, 10)$rename("LST_deciles_MODIS")

# Check CRS ---------------------------------------------------------------
modis_deciles$projection()$getInfo()

# EarthEngine Object to ggplot --------------------------------------------
# Final object start from ee
# Check if clipped and data from 1 to 10

modis_stars <- ee_as_stars(
  image  = modis_deciles$clip(stgohot_poly_geom),
  region = stgohot_poly_geom,
  scale  = 1000,
  crs    = "EPSG:4326",
  via    = "drive"
)

# Check results -----------------------------------------------------------

check <- rast(modis_stars)
check[check == 0] <- NA
plot(check, col = viridis(10))
