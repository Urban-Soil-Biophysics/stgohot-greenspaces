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
# This remains the same
bbox <- c(-70.85, -33.65, -70.45, -33.30) 

roi <- ee$Geometry$Rectangle(
  coords   = bbox,
  proj     = "EPSG:4326",
  geodesic = FALSE
)

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
  filterBounds(roi)$
  filterDate(start_date, end_date)$
  map(ee_utils_pyfunc(maskL8sr)) # Apply the cloud masking function

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
minmax <- lst_celcius_landsat$reduceRegion(
  reducer    = ee$Reducer$minMax(),
  geometry   = roi,
  scale      = 30, 
  bestEffort = TRUE
)

mn <- ee$Number(minmax$get("LST_Day_C_min"))
mx <- ee$Number(minmax$get("LST_Day_C_max"))
bw <- mx$subtract(mn)$divide(5)

lst_celcius_quintiles_landsat <- ee$Image(
  lst_celcius_landsat$expression(
    "floor((b(0) - mn) / bw) + 1",
    list(mn = mn, bw = bw)
  )
)$toInt()$clamp(1, 5)$rename("LST_quintiles_LANDSAT")  # Final band name

# EarthEngine Object to R --------------------------------------------------
# Download the GEE object to your local R session.
# We change the final object name to avoid confusion with modis.

lst_quint_stars_landsat <- ee_as_stars(
  image  = lst_celcius_quintiles_landsat,
  region = roi,
  scale  = 30, 
  crs    = "EPSG:4326",
  via    = "drive" 
)

# Print the object to verify
print(lst_quint_stars_landsat)