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

# to generate an object based on LST quintile analysis from MODIS 
# Day of analysis 20 - 01 - 2024

# Open GEE ----------------------------------------------------------------
# ee_Authenticate()  # Account GEE

# ee_Initialize()  # open GEE

# Define day of analysis --------------------------------------------------
# GEE objects do not process dates as R
# Define date a sprintf day

stgohot_day_str <- sprintf("%s", as.Date("2024-01-20")) # string date
start_day_ee <- ee$Date(stgohot_day_str) # date ee format

# ROI ---------------------------------------------------------------------
# Region of interest is a rectangle of Santiago de Chile
# The bounding box must be a vector xmin, ymin, xmax, ymax
# -70.85, -33.65, -70.45, -33.30 # Santiago

bbox <- c(-70.85, -33.65, -70.45, -33.30) # double check with stgohot

roi <- ee$Geometry$Rectangle(
  coords   = bbox,
  proj     = "EPSG:4326",
  geodesic = FALSE
)

# Get MODIS  --------------------------------------------------------------

modis_stgo <-
  ee$ImageCollection("MODIS/061/MOD11A1")$ # modis
  filterDate(start_day_ee, start_day_ee$advance(1, "day"))$ # Day stgohot
  filterBounds(roi)$ # rectangle corresponding to Santiago
  map(ee_utils_pyfunc(function(img) {
    mask <- img$select("QC_Day")$bitwiseAnd(3)$lte(1)
    img$updateMask(mask)
  })) # funtion to select quality pixels 

img_modis_stgo <- modis_stgo$sort("system:time_start")$mosaic() # set of tiles

# Calculate LST -----------------------------------------------------------
# Celcius

lst_celcius <- img_modis_stgo$
  select("LST_Day_1km")$multiply(0.02)$
  subtract(273.15)$
  rename("LST_Day_C")

# Get min and max for split quintiles -------------------------------------

minmax <- lst_celcius$reduceRegion(
  reducer    = ee$Reducer$minMax(),
  geometry   = roi,
  scale      = 1000, # modis 1km
  bestEffort = TRUE
)

mn <- ee$Number(minmax$get("LST_Day_C_min")) # min C
mx <- ee$Number(minmax$get("LST_Day_C_max")) # max C
bw <- mx$subtract(mn)$divide(5) # Bins weights (min - max)/5

lst_celcius_quintiles <- ee$Image(
  lst_celcius$expression(
    "floor((b(0) - mn) / bw) + 1",
    list(mn = mn, bw = bw)
  )
)$toInt()$clamp(1, 5)$rename("LST_quintiles_MODIS")

# EarthEngine Object to ggplot --------------------------------------------

lst_quint_stars <- ee_as_stars(
  image  = lst_celcius_quintiles,
  region = roi,
  scale  = 1000,
  crs    = "EPSG:4326",
  via    = "drive"
)
