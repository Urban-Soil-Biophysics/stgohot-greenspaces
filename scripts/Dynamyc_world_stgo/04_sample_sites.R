# 1. Libraries ------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)
library(ggmap)
library(viridis)
library(ggspatial) 
library(gridExtra)
library(scales)
library(hexbin)

# 2. Data Loading ---------------------------------------------------------
ugs_analysis <- readRDS("data/results/ugs_analysis_10m_utm.rds") %>% st_transform(4326)
woody_rast   <- rast("data/stgo_dw/dw_woody_parks_10m.tif")
grass_rast   <- rast("data/stgo_dw/dw_grass_parks_10m.tif")

stgo_hot_am  <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
stgo_hot_af  <- rast("data/stgo-hot/santiago-chile_af_temp_c.tif")
stgo_hot_pm  <- rast("data/stgo-hot/santiago-chile_pm_temp_c.tif")

# 3. Basemap Setup --------------------------------------------------------
stgo_bbox <- c(left = -70.80, bottom = -33.60, right = -70.50, top = -33.35)
basemap   <- get_stadiamap(bbox = stgo_bbox, zoom = 12, maptype = "stamen_toner_lite")

# 4. Biomass Hex-Mapping --------------------------------------------------
raster_to_df <- function(r, name) {
  as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
    setNames(c("x", "y", "value")) %>%
    mutate(type = name)
}

woody_df <- raster_to_df(woody_rast, "Woody")
grass_df <- raster_to_df(grass_rast, "Grass")

plot_hex_spatial <- function(df, title, palette) {
  ggmap(basemap) +
    stat_summary_hex(data = df, aes(x = x, y = y, z = value), 
                     fun = mean, bins = 60, alpha = 0.8, inherit.aes = FALSE) +
    scale_fill_viridis_c(option = palette, name = "Prob %", labels = percent) +
    coord_sf(crs = 4326) + 
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 16), legend.position = "right")
}

if(!dir.exists("plots")) dir.create("plots")
ggsave("plots/04_Hex_Woody.png", plot_hex_spatial(woody_df, "Santiago: Woody Density", "magma"), width = 10, height = 10)
ggsave("plots/04_Hex_Grass.png", plot_hex_spatial(grass_df, "Santiago: Grass Density", "plasma"), width = 10, height = 10)

# 5. Full Thermal Extraction & Selection ----------------------------------
priority_data <- ugs_analysis %>%
  mutate(
    temp_am_c = exact_extract(stgo_hot_am, ., "mean"),
    temp_af_c = exact_extract(stgo_hot_af, ., "mean"),
    temp_pm_c = exact_extract(stgo_hot_pm, ., "mean"),
    area_ha   = round(area_m2 / 10000, 2)
  ) %>%
  st_drop_geometry() %>%
  filter(area_m2 >= 2000)

top_10 <- priority_data %>% arrange(desc(woody_ratio)) %>% slice_head(n = 10) %>% mutate(Management = "Urban Forest (Cool)")
bottom_10 <- priority_data %>% arrange(woody_ratio) %>% slice_head(n = 10) %>% mutate(Management = "Open Grass (Hot)")

comparison_table <- bind_rows(top_10, bottom_10)

# 6. Final Table Generation -----------------------------------------------
table_output <- comparison_table %>%
  select(Name = name, `Area (ha)` = area_ha, `Woody (%)` = woody_ratio, `Grass (%)` = grass_ratio,
         `AM (C)` = temp_am_c, `AF (C)` = temp_af_c, `PM (C)` = temp_pm_c, Management) %>%
  mutate(across(contains("(%)"), ~percent(., accuracy = 1)),
         across(contains("(C)"), ~round(., 1)))

row_bg <- c(rep("#e7f3e7", 10), rep("#fde7e7", 10))
table_theme <- ttheme_default(
  core = list(bg_params = list(fill = row_bg, col = NA), fg_params = list(fontsize = 8)),
  colhead = list(bg_params = list(fill = "#333333"), fg_params = list(col = "white", fontface = "bold", fontsize = 9))
)

png("plots/04_Final_Sampling_Contrast_Table.png", height = 1200, width = 2400, res = 180)
grid.table(table_output, theme = table_theme)
dev.off()

write_csv(comparison_table, "data/results/FIELD_SAMPLING_FINAL_REPORT.csv")