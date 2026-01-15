library(terra)
library(ggplot2)
library(tidyterra)

# Source of the raster
# The NDVI raster of Santiago was downloaded from GEE using the landsat collection (LANDSAT/LC08/C02/T1_L2)

# Load NDVI raster
ndvi_rast <- rast("data/geo-data/NDVI_Santiago_Jan2024_30m.tif")

# Define color palette (from GEE script)
gee_palette <- c('#ece7f2', '#a6bddb', '#2ca25f', '#006d2c')

# Generate plot
ndvi_map <- ggplot() +
  geom_spatraster(data = ndvi_rast) +
  scale_fill_gradientn(
    colors = gee_palette,
    name = "NDVI",
    limits = c(0, 0.8),
    na.value = "transparent"
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Vegetation Vigor (NDVI) - Urban Santiago",
    subtitle = "January 2024 | Landsat 8 & 9 (30m)",
    caption = "Project: stgo-hot"
  )

# Save to plots directory
if(!dir.exists("plots")) dir.create("plots")
ggsave("plots/map_ndvi_santiago.png", ndvi_map, width = 10, height = 8, dpi = 600)