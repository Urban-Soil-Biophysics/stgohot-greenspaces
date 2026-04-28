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

# Goal --------------------------------------------------------------------
# to generate an object based on LST quintile analysis from LANDSAT
# Day of analysis around 20 - 01 - 2024

# Open GEE ----------------------------------------------------------------
# ee_Authenticate()  # Account GEE

# ee_Initialize()  # open GEE

# Define day of analysis --------------------------------------------------
# Landsat does not have daily coverage, so we search for the clearest image
# within a date range around your day of interest.
start_date <- "2024-01-01"
end_date   <- "2024-01-31"

# ROI (Region of Interest) ------------------------------------------------

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
stgohot_poly_gee  <- sf_as_ee(stgohot_poly)
stgohot_poly_geom <- stgohot_poly_gee$geometry()   

# Get Landsat 8 -----------------------------------------------------------

## Cloud mask function for Landsat 8
# This function uses the 'QA_PIXEL' band to identify and remove pixels
# that are classified as clouds, cloud shadows, or cirrus.
maskL8sr <- function(image) {
  # Bits 3 (Cloud) and 4 (Cloud Shadow) are the most important.
  cloudShadowBitMask <- bitwShiftL(1, 4)
  cloudsBitMask <- bitwShiftL(1, 3)
  
  # Select the quality assurance band.
  qa <- image$select('QA_PIXEL')
  
  # Both flags should be 0, indicating clear conditions.
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  
  # Apply the mask to the image and return it.
  return(image$updateMask(mask))
}

# Call to the Landsat 8 Image Collection
landsat_stgo_collection <-
  ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$
  filterBounds(stgohot_poly_geom)$
  filterDate(start_date, end_date)$
  map(ee_utils_pyfunc(function(img) {
    maskL8sr(img)$clip(stgohot_poly_geom)  
  }))

# Selection of the clearest image
# Instead of a mosaic, we sort by the cloud cover property and select the first one.
img_landsat_stgo <- landsat_stgo_collection$
  sort('CLOUD_COVER')$
  first()

# Calculate LST -----------------------------------------------------------
# The formula for Landsat 8 is different from MODIS.
# LST (Kelvin) = ST_B10 * 0.00341802 + 149.0
# Then we convert from Kelvin to Celsius (-273.15).

lst_celcius_landsat <- img_landsat_stgo$
  select("ST_B10")$       # The thermal temperature band is ST_B10
  multiply(0.00341802)$   # Scale factor
  add(149.0)$             # Add factor
  subtract(273.15)$       # Convert °K to °C
  rename("LST_Day_C")

# Get min and max for split quintiles -------------------------------------
minmax_landsat <- lst_celcius_landsat$reduceRegion(
  reducer    = ee$Reducer$minMax(),
  geometry   = stgohot_poly_geom,
  scale      = 30, 
  bestEffort = TRUE
)

mn_landsat <- ee$Number(minmax_landsat$get("LST_Day_C_min"))
mx_landsat <- ee$Number(minmax_landsat$get("LST_Day_C_max"))
bw_landsat <- mx_landsat$subtract(mn_landsat)$divide(10)

landstat_deciles <- ee$Image(
  lst_celcius_landsat$expression(
    "floor((b(0) - mn) / bw) + 1",
    list(mn = mn_landsat, bw = bw_landsat)
  )
)$toInt()$clamp(1, 10)$rename("LST_quintiles_LANDSAT")  # Final band name

# EarthEngine Object to R --------------------------------------------------
# Download the GEE object to your local R session.
# We change the final object name to avoid confusion with modis.

landsat_stars <- ee_as_stars(
  image  = landstat_deciles$clip(stgohot_poly_geom),
  region = stgohot_poly_geom,
  scale  = 30, 
  crs    = "EPSG:4326",
  via    = "drive" 
)


# Check -------------------------------------------------------------------

check <- rast(landsat_stars)
check[check == 0] <- NA  
plot(check, col = viridis(10))