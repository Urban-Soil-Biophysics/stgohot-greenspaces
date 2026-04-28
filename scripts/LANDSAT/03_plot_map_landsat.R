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
library(ggrepel)

# Start from LST deciles LANDSAT ------------------------------------------

landsat_raster <- rast(landsat_stars)
names(landsat_raster) <- "decile"

# Now as polygon terra (faster for ggplot) --------------------------------

landsat_poly <- terra::as.polygons(
  landsat_raster,
  values = TRUE,
  na.rm = TRUE,
  dissolve = TRUE
) %>% 
  sf::st_as_sf() %>%
  st_make_valid()

# Create factors Q1 to Q10 ------------------------------------------------

landsat_poly$decile <- factor(
  landsat_poly$decile, 
  levels = 1:10,
  labels = paste0("D", 1:10)
)

# Get a background map from ggmaps ----------------------------------------

bgd_stgo <- get_stadiamap(
  bbox = c(
    left = -70.95,
    bottom = -33.70,
    right = -70.40,
    top = -33.23
  ),
  zoom = 12,
  maptype = "stamen_terrain"
) # background of Santiago for plotting

# Map LANDSAT  --------------------------------------------------------------

landsat_deciles_plot <- ggmap(bg_stgo) +
  geom_sf(
    data = landsat_poly,
    aes(fill = decile),
    color = NA,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(option    = "magma",
                       name      = "LST deciles",
                       drop      = FALSE,
                       na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 12) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(x = NULL, y = NULL, title = "Landsat January 20th 2024")

# ggsave ------------------------------------------------------------------

ggsave(
  "outputs/LANDSAT/landsat-deciles.tiff",
  landsat_deciles_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# Ggplot polygons with categories -----------------------------------------
# First, got my analysis to sf
intercept_landsat_sf <-
  intercept_landsat %>%
  group_by(park_id, name, greenspace_type, area, deciles_label) %>%
  summarise(geometry = first(geometry),
            .groups = "drop") %>%
  st_as_sf()

# Then I set an order for deciles -----------------------------------------

labs_order_landsat <- intercept_landsat %>%
  distinct(deciles_label) %>%
  filter(!str_detect(deciles_label, "Q0")) %>% 
  mutate(
    nums = str_split(str_remove_all(deciles_label, "Q"), "-") %>% 
      map(as.integer),
    key  = map_dbl(nums, mean),     
    len  = map_int(nums, length)    
  ) %>%
  arrange(key, len) %>%
  pull(deciles_label)

# Map of UGS and deciles ------------------------------------------------

ggplot() + 
  geom_sf(
    data = intercept_landsat_sf %>%
      mutate(deciles_label = factor(deciles_label, levels = labs_order_landsat)),
    inherit.aes = FALSE,
    aes(fill = deciles_label),
    color = "darkgreen",
    alpha = 0.7
  ) +
  labs(x = "", y = "", fill = "deciles LST") +
  scale_fill_viridis_d(option    = "H",
                       na.translate = FALSE) +
  theme_minimal(base_size = 12) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(x = NULL, y = NULL, title = "Landsat January 20th 2024")


# gganimate ---------------------------------------------------------------

gif_landsat <- intercept_landsat_sf %>%
  mutate(deciles_label = factor(deciles_label, levels = labs_order_landsat)) %>% 
  mutate(state = deciles_label)

gif_landsat_plot <- ggplot() +
  geom_sf(data = gif_landsat, aes(fill = deciles_label),
          color = "darkgreen", alpha = 0.7, linewidth = 0.15,
          show.legend = TRUE) +
  scale_fill_viridis_d(option = "magma", na.translate = FALSE,
                       name = "Deciles") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  coord_sf() +
  labs(
    x = NULL, y = NULL,
    title = "LANDSAT"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right") +
  transition_states(state, wrap = FALSE, transition_length = 1, state_length = 1) +
  shadow_mark(past = TRUE, future = FALSE, exclude_layer = NULL, alpha = 1)

# save gif ----------------------------------------------------------------

animate(gif_landsat_plot, nframes = length(labs_order_landsat) * 8, fps = 8,
        width = 900, height = 650, res = 120)
anim_save("outputs/LANDSAT/landstat-animation-deciles.gif")

