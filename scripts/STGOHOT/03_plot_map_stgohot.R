# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(stars)
library(ggspatial)
library(ggmap)
library(scales)
library(ggrepel)
library(gganimate)

# Load stack --------------------------------------------------------------

stgohot_raster <- rast(stgohot_deciles)
names(stgohot_raster) <- c("AM", "AF", "PM")

# Reprojection (ggmap is 4326) --------------------------------------------

stgohot_raster <- project(stgohot_raster, "EPSG:4326")

# Create a long dataframe of rasters --------------------------------------

df_raster_stgohot <- as.data.frame(stgohot_raster,
                                   xy = TRUE,
                                   na.rm = TRUE,
                                   long = TRUE) %>%
  pivot_longer(cols = c("AM", "AF", "PM")) %>%
  mutate(decile = factor(value, levels = 1:10, labels = paste0("D", 1:10)))

# Get a background map from ggmaps ----------------------------------------

bg_stgo <- get_stadiamap(
  bbox = c(
    left = -70.95,
    bottom = -33.70,
    right = -70.40,
    top = -33.23
  ),
  zoom = 12,
  maptype = "stamen_terrain"
) # background of Santiago for plotting

# Map Stgohot deciles -----------------------------------------------------

stgohot_perhour_plot <-
  ggmap(bg_stgo) +
  geom_raster(
    data = df_raster_stgohot %>%
      mutate(name = fct_relevel(name, "AM", "AF", "PM")),
    aes(x = x, y = y, fill = decile),
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap( ~ name) +
  scale_fill_viridis_d(
    option = "magma",
    name = "LST deciles",
    drop = FALSE,
    na.translate = FALSE
  ) +
  coord_quickmap(
    xlim = c(-70.95, -70.40),
    ylim = c(-33.70,-33.23),
    expand = FALSE
  ) +
  theme_minimal(base_size = 12) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(x = NULL, y = NULL, title = "STGOHOT - 20 Jan 2024")


# ggsave ------------------------------------------------------------------

ggsave(
  "outputs/STGOHOT/stgohot-perhour-deciles.tiff",
  stgohot_perhour_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# Map average -------------------------------------------------------------

deciles_stgohot_tempmean <- project(deciles_stgohot_tempmean, "EPSG:4326")

bbox <- c(left = -70.95, bottom = -33.70, right = -70.40, top = -33.23)

df_stgohot_tempmean <-  as.data.frame(
  deciles_stgohot_tempmean,
  xy = TRUE,
  na.rm = TRUE,
  long = TRUE
) %>% mutate(deciles = factor(deciles, levels = 1:10, labels = paste0("D", 1:10)))

stgohot_mean_plot <-
  ggmap(bg_stgo) +
  geom_raster(
    data = df_stgohot_tempmean,
    aes(x = x, y = y, fill = deciles),
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(option = "magma", name = "LST deciles", drop = FALSE, na.translate = FALSE) +
  coord_sf(
    xlim = c(bbox["left"], bbox["right"]),
    ylim = c(bbox["bottom"], bbox["top"]),
    expand = FALSE,
    crs = st_crs(4326)          
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",      
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.0, "cm"),
    pad_x = grid::unit(0.3, "cm"),
    pad_y = grid::unit(0.3, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.2,           
    bar_cols = c("black","white"),
    unit_category = "metric",   
    height = grid::unit(0.25, "cm"),
    pad_x = grid::unit(0.3, "cm"),
    pad_y = grid::unit(0.3, "cm")
  ) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = NULL, title = "STGOHOT-LST — January 20th 2024 (mean)")

# ggsave ------------------------------------------------------------------

ggsave(
  "outputs/STGOHOT/stgohot-deciles.tiff",
  stgohot_mean_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# Ggplot polygons with categories -----------------------------------------
# First, got my analysis to sf
intercept_stgohot_sf <-
  intercept_stgohot %>%
  group_by(park_id, name, greenspace_type, area, deciles_label) %>%
  summarise(geometry = first(geometry),
            .groups = "drop") %>%
  st_as_sf()

# Then I set an order for deciles -----------------------------------------

labs_order_stgohot <- intercept_stgohot %>%
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
    data = intercept_stgohot_sf %>%
      mutate(deciles_label = factor(deciles_label, levels = labs_order_stgohot)),
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
  labs(x = NULL, y = NULL, title = "MODIS January 20th 2024")


# gganimate ---------------------------------------------------------------

# state new column for each decile
gif_stgohot <- intercept_stgohot_sf %>%
  mutate(deciles_label = factor(deciles_label, levels = labs_order_stgohot)) %>% 
  mutate(state = deciles_label)

gif_stgohot_plot <- ggplot() +
  geom_sf(data = gif_stgohot, aes(fill = deciles_label), alpha = 0.7, linewidth = 0.15,
          show.legend = TRUE) +
  scale_fill_viridis_d(option = "magma", na.translate = FALSE,
                       name = "Deciles") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  coord_sf() +
  labs(
    x = NULL, y = NULL,
    title = "STGOHOT"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right") +
  transition_states(state, wrap = FALSE, transition_length = 1, state_length = 1) +
  shadow_mark(past = TRUE, future = FALSE, exclude_layer = NULL, alpha = 1)

# save gif ----------------------------------------------------------------

animate(gif_stgohot_plot, nframes = length(labs_order_stgohot) * 8, fps = 8,
        width = 900, height = 650, res = 120)
anim_save("outputs/STGOHOT/stgohot-animation-deciles.gif")

