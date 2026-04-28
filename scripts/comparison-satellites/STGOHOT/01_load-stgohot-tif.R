# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)
library(stars)
library(ggspatial)
library(ggmap)
library(scales)

# Goal --------------------------------------------------------------------
# to generate an object based on air deciles analysis from Stgohot TIFs
# Analysis per hour and daily average using same range

# Define day of analysis --------------------------------------------------
# STGOHOT data correspond to 20th January 2024 at three times (am, af, pm)

# Load data ---------------------------------------------------------------
am_stgohot <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
af_stgohot <- rast("data/stgo-hot/santiago-chile_af_temp_c.tif")
pm_stgohot <- rast("data/stgo-hot/santiago-chile_pm_temp_c.tif")

# Create list of hourly rasters ------------------------------------------
stgohot_tifs <- list(
  AM = am_stgohot,
  AF = af_stgohot,
  PM = pm_stgohot
)

# Calculate global min, max, and decile width -----------------------------
# A common range is used for all hourly rasters and the daily mean raster.
mn_all <- min(map_dbl(stgohot_tifs, ~ global(.x, "min", na.rm = TRUE)[[1]]))
mx_all <- max(map_dbl(stgohot_tifs, ~ global(.x, "max", na.rm = TRUE)[[1]]))
bw_all <- (mx_all - mn_all) / 10 # bw = max - min 

# Function to calculate LST deciles ---------------------------------------
calc_stgohot_deciles <- function(r, mn, bw, layer_name = "deciles") {
  q <- floor((r - mn) / bw) + 1
  q <- clamp(q, 1, 10) %>%
    terra::as.int()
  names(q) <- layer_name
  q
}

# Calculate hourly LST deciles -------------------------------------------
stgohot_deciles <- map(
  stgohot_tifs,
  ~ calc_stgohot_deciles(.x, mn_all, bw_all)
)

# Calculate daily mean LST ------------------------------------------------
stgohot_stack <- rast(stgohot_tifs)

temp_mean <- mean(stgohot_stack, na.rm = TRUE)
names(temp_mean) <- "temp_c_mean"

# Calculate daily mean LST deciles using the same global range -------------
deciles_stgohot_tempmean <- calc_stgohot_deciles(
  temp_mean,
  mn_all,
  bw_all,
  layer_name = "decile_mean"
)