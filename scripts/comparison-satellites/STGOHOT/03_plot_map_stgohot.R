# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(stars)
library(ggspatial)
library(ggmap)
library(scales)

# Map settings ------------------------------------------------------------
bbox_stgo <- c(
  left   = -70.95,
  bottom = -33.70,
  right  = -70.40,
  top    = -33.23
)

# Load hourly decile rasters ----------------------------------------------
stgohot_raster       <- rast(stgohot_deciles)
names(stgohot_raster) <- c("AM", "AF", "PM")
stgohot_raster_4326  <- project(stgohot_raster, "EPSG:4326")

# Convert hourly rasters to long dataframe --------------------------------
df_raster_stgohot <- as.data.frame(
  stgohot_raster_4326,
  xy    = TRUE,
  na.rm = TRUE
) %>%
  pivot_longer(
    cols      = c("AM", "AF", "PM"),
    names_to  = "time",
    values_to = "decile"
  ) %>%
  mutate(
    time   = factor(time, levels = c("AM", "AF", "PM")),
    decile = factor(decile, levels = 1:10, labels = paste0("D", 1:10))
  )

# Get background map ------------------------------------------------------
bg_stgo <- get_stadiamap(
  bbox    = bbox_stgo,
  zoom    = 12,
  maptype = "stamen_terrain"
)

# Plot hourly STGOHOT deciles ---------------------------------------------
stgohot_perhour_plot <- ggmap(bg_stgo) +
  geom_raster(
    data        = df_raster_stgohot,
    aes(x = x, y = y, fill = decile),
    alpha       = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ time) +
  scale_fill_viridis_d(
    option       = "magma",
    name         = "Temperature deciles",
    drop         = FALSE,
    na.translate = FALSE
  ) +
  coord_quickmap(
    xlim   = c(bbox_stgo["left"], bbox_stgo["right"]),
    ylim   = c(bbox_stgo["bottom"], bbox_stgo["top"]),
    expand = FALSE
  ) +
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    style       = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = NULL, title = "STGOHot — 20 January 2024")

ggsave(
  "outputs/comparison-satellites/STGOhot/stgohot-perhour-deciles.tiff",
  stgohot_perhour_plot,
  width = 6, height = 4, dpi = 300
)

# Plot daily mean STGOHOT deciles -----------------------------------------
deciles_stgohot_tempmean_4326 <- project(deciles_stgohot_tempmean, "EPSG:4326")

df_stgohot_tempmean <- as.data.frame(
  deciles_stgohot_tempmean_4326,
  xy    = TRUE,
  na.rm = TRUE
) %>%
  rename(decile = "decile_mean") %>%       # ✅ fixed layer name
  mutate(
    decile = factor(decile, levels = 1:10, labels = paste0("D", 1:10))
  )

stgohot_mean_plot <- ggmap(bg_stgo) +
  geom_raster(
    data        = df_stgohot_tempmean,
    aes(x = x, y = y, fill = decile),
    alpha       = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(
    option       = "magma",
    name         = "Temperature deciles",
    drop         = FALSE,
    na.translate = FALSE
  ) +
  coord_quickmap(
    xlim   = c(bbox_stgo["left"], bbox_stgo["right"]),
    ylim   = c(bbox_stgo["bottom"], bbox_stgo["top"]),
    expand = FALSE
  ) +
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    style       = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  theme_minimal(base_size = 12) +
  labs(x = NULL, y = NULL, title = "STGOHOT — 20 January 2024 (daily mean)")

ggsave(
  "outputs/comparison-satellites/STGOhot/stgohot-mean-deciles.tiff",
  stgohot_mean_plot,
  width = 6, height = 4, dpi = 300
)

# Order decile labels -----------------------------------------------------
labs_order_stgohot <- intercept_stgohot_summary %>%
  distinct(deciles_label) %>%
  mutate(
    nums = str_split(str_remove_all(deciles_label, "Q"), "-") %>%
      map(as.integer),
    key  = map_dbl(nums, mean),
    len  = map_int(nums, length)
  ) %>%
  arrange(key, len) %>%
  pull(deciles_label)

# Build sf objects --------------------------------------------------------
# Mean map
intercept_stgohot_mean_sf <- minvu %>%
  select(park_id) %>%
  left_join(
    intercept_stgohot_summary %>% filter(time == "Mean"),
    by = "park_id"
  ) %>%
  st_as_sf()

# Per-hour map
intercept_stgohot_perhour_sf <- minvu %>%
  select(park_id) %>%
  left_join(
    intercept_stgohot_summary %>%
      filter(time != "Mean") %>%
      mutate(time = factor(time, levels = c("AM", "AF", "PM"))),
    by = "park_id"
  ) %>%
  st_as_sf()

