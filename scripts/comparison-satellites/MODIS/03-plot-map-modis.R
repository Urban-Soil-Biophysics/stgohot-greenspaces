# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(ggspatial)
library(ggmap)
library(gganimate)

# Load raster from disk ---------------------------------------------------
modis_raster <- rast("data/geo-data/modis_lst_deciles_jan2024.tif")

# Map settings ------------------------------------------------------------
bbox_stgo <- c(left = -70.95, bottom = -33.70, right = -70.40, top = -33.23)

# Background map ----------------------------------------------------------
bg_stgo <- get_stadiamap(
  bbox    = bbox_stgo,
  zoom    = 12,
  maptype = "stamen_terrain"
)

# Raster to polygon for ggplot --------------------------------------------
modis_poly <- terra::as.polygons(
  modis_raster,
  values   = TRUE,
  na.rm    = TRUE,
  dissolve = TRUE
) %>%
  sf::st_as_sf() %>%
  st_make_valid() %>%
  mutate(
    decile = factor(decile_modis, levels = 1:10, labels = paste0("D", 1:10))
  )

# Plot LST decile raster map ----------------------------------------------
# ✅ assigned to variable for ggsave
modis_deciles_plot <- ggmap(bg_stgo) +
  geom_sf(
    data        = modis_poly,
    aes(fill    = decile),
    color       = NA,
    alpha       = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(
    option       = "magma",
    name         = "LST deciles",
    drop         = FALSE,
    na.translate = FALSE
  ) +
  coord_sf(expand = FALSE) +
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    style       = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = NULL, title = "MODIS LST deciles — January 20th 2024")

ggsave(
  "outputs/comparison-satellites/MODIS/modis-deciles.tiff",
  modis_deciles_plot,
  width = 6, height = 4, dpi = 300
)

# Order decile labels -----------------------------------------------------
labs_order_modis <- intercept_modis_summary %>%
  distinct(deciles_label) %>%
  mutate(
    nums = str_split(str_remove_all(deciles_label, "Q"), "-") %>%
      map(as.integer),
    key  = map_dbl(nums, mean),
    len  = map_int(nums, length)
  ) %>%
  arrange(key, len) %>%
  pull(deciles_label)

