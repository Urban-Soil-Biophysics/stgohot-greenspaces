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
# to generate an object based on LST deciles analysis from Stgohot TIFs
# Analysis per hour and daily average

# Define day of analysis --------------------------------------------------
# STGOHOT data correspond to 20th January 2024 at three times (am, af, pm)

# Load data ---------------------------------------------------------------

am_stgohot <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
af_stgohot <- rast("data/stgo-hot/santiago-chile_af_temp_c.tif")
pm_stgohot <- rast("data/stgo-hot/santiago-chile_pm_temp_c.tif")

# Calculate an min and max global (all day) -------------------------------
# For fair comparison bewteen hours
stgohot_tifs <- list(
  am_stgohot,
  af_stgohot,
  pm_stgohot
)

# Min and max (global) ----------------------------------------------------

mn_all <- min(map_dbl(stgohot_tifs, ~ global(.x, "min", na.rm = TRUE)[[1]]))
mx_all <- max(map_dbl(stgohot_tifs, ~ global(.x, "max", na.rm = TRUE)[[1]]))
bw_all <- (mx_all - mn_all) / 10

# Calculate deciles -----------------------------------------------------

stgohot_deciles <- function(r, mn, bw) {
  q <- floor((r - mn) / bw) + 1
  q <- clamp(q, 1, 10)
  names(q) <- "LST_deciles"
  q
}
# Apply function and name each raster -------------------------------------

stgohot_deciles <- map(stgohot_tifs,
                                ~ stgohot_deciles(.x, mn_all, bw_all)) 

names(stgohot_deciles) <- c("AM","AF","PM")

# Average Celcius for daily comparison  -----------------------------------
# First a stack using the 3 times (AM/AF/PM)
# Calculate average celcius temperature
# New calculation of min, max and bw (from mean)

stgothot_stack <- rast(list(am_stgohot, af_stgohot, pm_stgohot))
temp_mean  <- mean(stgothot_stack, na.rm = TRUE)  
names(temp_mean) <- "temp_c_mean"

# New calculation of min and max ------------------------------------------

mn_mean_stgohot <- min(map_dbl(temp_mean, ~ global(.x, "min", na.rm = TRUE)[[1]]))
mx_mean_stgohot <- max(map_dbl(temp_mean, ~ global(.x, "max", na.rm = TRUE)[[1]]))
bw_mean_stgohot <- (mx_mean_stgohot - mn_mean_stgohot) / 10

# New object average temp -------------------------------------------------

deciles_stgohot_tempmean <- floor((temp_mean - mn_mean_stgohot) / bw_mean_stgohot) + 1
deciles_stgohot_tempmean <- clamp(deciles_stgohot_tempmean, 1, 10) %>%  terra::as.int()
names(deciles_stgohot_tempmean) <- "deciles"




