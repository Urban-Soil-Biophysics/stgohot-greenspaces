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

# Start from LST quintiles modis ------------------------------------------

modis_spatraster <- rast(lst_quint_stars)
names(modis_spatraster) <- "quintile"

# Now as polygon terra (faster for ggplot) --------------------------------

modis_terra <- terra::as.polygons(
  modis_spatraster,
  values = TRUE,
  na.rm = TRUE,
  dissolve = TRUE
) %>% 
sf::st_as_sf() %>%
  st_make_valid()

# Create factors Q1 to Q5 -------------------------------------------------

modis_terra$quintile <- factor(
  modis_terra$quintile,
  levels = 1:5,
  labels = c("Q1 (0–20%)","Q2 (20–40%)","Q3 (40–60%)","Q4 (60–80%)","Q5 (80–100%)")
)

# Get a background map from ggmaps ----------------------------------------

background_stgo <- get_stadiamap(
  bbox = c(
    left = -70.85,
    bottom = -33.65,
    right = -70.45,
    top = -33.30
  ),
  zoom = 12,
  maptype = "stamen_terrain"
) # background of Santiago for plotting

# Map MODIS  --------------------------------------------------------------

ggmap(background_stgo) +
  geom_sf(data = modis_terra,
          aes(fill = quintile),
          color = NA,
          alpha = 0.5,
          inherit.aes = FALSE) +
  scale_fill_viridis_d(
    option    = "magma",
    name      = "LST Quintiles",
    drop      = FALSE
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 12) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(x = NULL, y = NULL, title = "MODIS-LST January 20th 2024")

# Ggplot polygons with categories -----------------------------------------
# First, got my analysis to sf
intercept_analysis_sf <-
  intercept_analysis %>%
  group_by(park_id, name, greenspace_type, area, quintiles_label) %>%
  summarise(geometry = first(geometry),
            .groups = "drop") %>%
  st_as_sf()

# Then I set an order for quintiles ---------------------------------------

labs_order <- intercept_analysis %>%
  distinct(quintiles_label) %>%
  mutate(
    nums = str_split(str_remove_all(quintiles_label, "Q"), "-") |> 
      map(as.integer),
    key  = map_dbl(nums, mean),     
    len  = map_int(nums, length)    
  ) %>%
  arrange(key, len) %>%
  pull(quintiles_label)

# Map of UGS and quintiles ------------------------------------------------
# Get a white background map
background_stgo_clear <- get_stadiamap(
  bbox = c(
    left = -70.85,
    bottom = -33.65,
    right = -70.45,
    top = -33.30
  ),
  zoom = 12,
  maptype = "stamen_toner_background"
) # background of Santiago for plotting


# Map of quintiles --------------------------------------------------------

ggmap(background_stgo_clear) +
  geom_sf(
    data = intercept_analysis_sf %>% 
      mutate(quintiles_label = factor(quintiles_label, levels = labs_order)),
    inherit.aes = FALSE,
    aes(fill = quintiles_label),
    color = "darkgreen",
    alpha = 0.7
  ) +
  labs(x = "", y = "", fill = "Quintiles LST") +
  scale_fill_viridis_d(
    option    = "H"
  ) +
  theme_minimal(base_size = 12) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(x = NULL, y = NULL, title = "MODIS-LST January 20th 2024")

