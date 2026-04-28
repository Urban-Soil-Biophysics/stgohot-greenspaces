# ── 04_sample_sites.R ─────────────────────────────────────────────────────────
# Goal: Rank urban greenspaces for field soil sampling prioritization.
# Combines:
#   - Dynamic World vegetation structure (woody_ratio, grass_ratio)
#   - SantiagoHOT mean temperature decile (from intercept_stgohot_summary)
#   - Park condition (estado_2: BUEN/MAL) from areas_verdes geojson
#
# Rankings produced:
#   1. Global       — all parks + roadsides together
#   2. Urban Parks  — Urban Parks only
#   3. Lawn Parks   — Lawn Parks only
#   4. By condition — within Good / Poor condition groups
#
# No GEE needed — reads from disk only.
#
# Requires:
#   data/results/ugs_analysis_10m_utm.rds         (from 03_zonal_statistics.R)
#   data/results/intercept_stgohot_summary.rds    (from 02_intercept-stgohot-ugs.R)
#   data/geo-data/sup_areas_verdes_stgo.geojson
#   data/stgo-hot/santiago-chile_*_temp_c.tif
#   data/stgo_dw/dw_woody_parks_10m.tif
#   data/stgo_dw/dw_grass_parks_10m.tif
# ─────────────────────────────────────────────────────────────────────────────

# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)
library(ggmap)
library(viridis)
library(ggspatial)
library(scales)

sf_use_s2(FALSE)

# ── 1. Load Dynamic World analysis ───────────────────────────────────────────
# One row per park with woody_ratio, grass_ratio, structure_cat, geometry
ugs_analysis <- readRDS("data/results/ugs_analysis_10m_utm.rds") %>%
  st_transform(4326)

# ── 2. Load SantiagoHOT mean decile ──────────────────────────────────────────
# dominant_decile for time == "Mean":
#   - computed from pixel-wise mean of AM/AF/PM rasters
#   - classified using a common global range across all three hours
#   - consistent with Landsat/MODIS decile framework
# This is the thermal ranking variable — NOT raw temperature.
# time column stored as character to allow == comparison
intercept_stgohot_summary <- readRDS(
  "data/results/intercept_stgohot_summary.rds"
)

stgohot_mean_decile <- intercept_stgohot_summary %>%
  dplyr::filter(as.character(time) == "Mean") %>%
  dplyr::select(park_id, stgohot_decile_mean = dominant_decile)

# ── 3. Load and join park condition ──────────────────────────────────────────
# estado_2: BUEN = Good condition / MAL = Poor condition
# Join strategy: spatial intersection, largest overlap wins (no shared ID)
areas_verdes <- st_read("data/geo-data/sup_areas_verdes_stgo.geojson") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  dplyr::select(comuna, estado_2, clase) %>%
  dplyr::mutate(
    condition = dplyr::case_when(
      estado_2 == "BUEN" ~ "Good condition",
      estado_2 == "MAL"  ~ "Poor condition",
      TRUE               ~ NA_character_
    )
  )

ugs_with_condition <- ugs_analysis %>%
  st_join(
    areas_verdes %>% dplyr::select(condition, clase, comuna),
    join    = st_intersects,
    left    = TRUE,
    largest = TRUE
  )

message(
  "Parks with condition assigned: ",
  sum(!is.na(ugs_with_condition$condition)),
  " / ", nrow(ugs_with_condition)
)
print(dplyr::count(sf::st_drop_geometry(ugs_with_condition), condition))

# ── 4. Extract SantiagoHOT raw temperatures per park ─────────────────────────
# Raw degrees C at AM (6-7h), AF (15-16h), PM (19-20h) — January 20th 2024
# Kept for display in Leaflet map and contrast tables only.
# NOT used for ranking — stgohot_decile_mean drives ranking.
stgo_hot_am <- rast("data/stgo-hot/santiago-chile_am_temp_c.tif")
stgo_hot_af <- rast("data/stgo-hot/santiago-chile_af_temp_c.tif")
stgo_hot_pm <- rast("data/stgo-hot/santiago-chile_pm_temp_c.tif")

priority_data <- ugs_with_condition %>%
  dplyr::mutate(
    temp_am_c = exactextractr::exact_extract(stgo_hot_am, ., "mean"),
    temp_af_c = exactextractr::exact_extract(stgo_hot_af, ., "mean"),
    temp_pm_c = exactextractr::exact_extract(stgo_hot_pm, ., "mean"),
    area_ha   = round(area_m2 / 10000, 2)
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(stgohot_mean_decile, by = "park_id")

# ── 5. Ranking helper function ────────────────────────────────────────────────
# Called identically for all ranking subsets.
# Deciles computed WITHIN the subset so rankings are internally consistent:
#   Global:       decile 10 = top 10% of all parks
#   Urban Parks:  decile 10 = top 10% of Urban Parks only
#   By condition: decile 10 = top 10% within Good or Poor separately
#
# Ranking variables:
#   woody_decile   ntile(woody_ratio, 10)          DW vegetation structure
#   temp_decile    ntile(stgohot_decile_mean, 10)  StgoHOT thermal exposure
#   length_decile  ntile(area_m2, 10)              size proxy Roadsides only
#   priority_score mean of applicable deciles
compute_ranking <- function(data) {
  data %>%
    dplyr::mutate(
      woody_decile  = dplyr::ntile(woody_ratio,         10),
      temp_decile   = dplyr::ntile(stgohot_decile_mean, 10),
      length_decile = dplyr::case_when(
        greenspace_type == "Roadside" ~ dplyr::ntile(area_m2, 10),
        TRUE                          ~ NA_integer_
      ),
      priority_score = dplyr::case_when(
        greenspace_type == "Roadside" ~
          (woody_decile + temp_decile + length_decile) / 3,
        TRUE ~
          (woody_decile + temp_decile) / 2
      )
    ) %>%
    dplyr::mutate(
      sampling_priority = dplyr::case_when(
        # Parks and Lawn Parks: vegetation structure + heat
        greenspace_type != "Roadside" & woody_decile >= 8 & temp_decile >= 8 ~
          "P1 — Forest + Hot",
        greenspace_type != "Roadside" & woody_decile <= 3 & temp_decile >= 8 ~
          "P2 — Grass + Hot",
        greenspace_type != "Roadside" & woody_decile >= 8 & temp_decile <= 3 ~
          "P3 — Forest + Cool",
        greenspace_type != "Roadside" & woody_decile <= 3 & temp_decile <= 3 ~
          "P4 — Grass + Cool",
        # Roadsides: heat + length
        greenspace_type == "Roadside" & temp_decile >= 8 & length_decile >= 8 ~
          "P1 — Hot + Long",
        greenspace_type == "Roadside" & temp_decile >= 8 & length_decile <= 3 ~
          "P2 — Hot + Short",
        greenspace_type == "Roadside" & temp_decile <= 3 & length_decile >= 8 ~
          "P3 — Cool + Long",
        greenspace_type == "Roadside" & temp_decile <= 3 & length_decile <= 3 ~
          "P4 — Cool + Short",
        TRUE ~ "P5 — Intermediate"
      )
    ) %>%
    dplyr::arrange(greenspace_type, dplyr::desc(priority_score))
}

# ── 6. Rankings ───────────────────────────────────────────────────────────────

# 6.1 Global — all greenspace types together
# Area filter >= 2000 m2 applied to parks/lawn parks only
# Roadsides excluded from area filter — linear features
parks_all <- priority_data %>%
  dplyr::filter(
    greenspace_type %in% c("Urban Park", "Lawn Park"),
    area_m2 >= 2000
  )
roadsides <- priority_data %>%
  dplyr::filter(greenspace_type == "Roadside")

ranking_global <- dplyr::bind_rows(parks_all, roadsides) %>%
  compute_ranking()

message("--- Global ranking ---")
print(dplyr::count(ranking_global, greenspace_type, sampling_priority))

# 6.2 Urban Parks only
ranking_urban_parks <- priority_data %>%
  dplyr::filter(greenspace_type == "Urban Park", area_m2 >= 2000) %>%
  compute_ranking()

message("--- Urban Parks: ", nrow(ranking_urban_parks), " parks ---")
print(dplyr::count(ranking_urban_parks, sampling_priority))

# 6.3 Lawn Parks only
ranking_lawn_parks <- priority_data %>%
  dplyr::filter(greenspace_type == "Lawn Park", area_m2 >= 2000) %>%
  compute_ranking()

message("--- Lawn Parks: ", nrow(ranking_lawn_parks), " parks ---")
print(dplyr::count(ranking_lawn_parks, sampling_priority))

# 6.4 By condition (Good / Poor) — parks + lawn parks only
# Roadsides excluded: condition data unreliable for linear features
ranking_by_condition <- priority_data %>%
  dplyr::filter(
    greenspace_type %in% c("Urban Park", "Lawn Park"),
    area_m2 >= 2000,
    !is.na(condition)
  ) %>%
  dplyr::group_by(condition) %>%
  dplyr::group_modify(~ compute_ranking(.x)) %>%
  dplyr::ungroup()

message("--- Condition-based ranking ---")
print(dplyr::count(ranking_by_condition, condition, sampling_priority))

# ── 7. Summary tables ─────────────────────────────────────────────────────────

summary_global <- ranking_global %>%
  dplyr::group_by(greenspace_type, sampling_priority) %>%
  dplyr::summarise(
    n            = dplyr::n(),
    mean_area_ha = round(mean(area_ha),                           2),
    mean_woody   = round(mean(woody_ratio),                       3),
    mean_temp_pm = round(mean(temp_pm_c,          na.rm = TRUE),  1),
    mean_decile  = round(mean(stgohot_decile_mean, na.rm = TRUE), 1),
    pct_good     = round(mean(condition == "Good condition",
                              na.rm = TRUE) * 100,                1),
    pct_poor     = round(mean(condition == "Poor condition",
                              na.rm = TRUE) * 100,                1),
    .groups      = "drop"
  )

summary_condition <- ranking_by_condition %>%
  dplyr::group_by(condition, greenspace_type, sampling_priority) %>%
  dplyr::summarise(
    n            = dplyr::n(),
    mean_woody   = round(mean(woody_ratio),                       3),
    mean_temp_pm = round(mean(temp_pm_c,          na.rm = TRUE),  1),
    mean_decile  = round(mean(stgohot_decile_mean, na.rm = TRUE), 1),
    .groups      = "drop"
  )

# ── 8. Hex density maps ───────────────────────────────────────────────────────

library(ggplot2)  
library(hexbin)

woody_rast <- rast("data/stgo_dw/dw_woody_parks_10m.tif")
grass_rast <- rast("data/stgo_dw/dw_grass_parks_10m.tif")

stgo_bbox <- c(left = -70.80, bottom = -33.60, right = -70.50, top = -33.35)
basemap   <- get_stadiamap(
  bbox    = stgo_bbox,
  zoom    = 12,
  maptype = "stamen_toner_lite"
)

raster_to_df <- function(r, name) {
  as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
    setNames(c("x", "y", "value")) %>%
    dplyr::mutate(type = name)
}

plot_hex_spatial <- function(df, title, palette) {
  ggmap(basemap) +
    ggplot2::stat_summary_hex(
      data        = df,
      aes(x = x, y = y, z = value),
      fun         = mean,
      bins        = 60,
      alpha       = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_viridis_c(
      option = palette,
      name   = "Probability",
      labels = scales::percent
    ) +
    coord_sf(crs = 4326) +
    annotation_scale(location = "bl", width_hint = 0.2) +
    annotation_north_arrow(
      location = "tr",
      style    = north_arrow_fancy_orienteering()
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 16),
      legend.position = "right"
    )
}

dir.create("plots", showWarnings = FALSE)

ggplot2::ggsave(
  "plots/04_Hex_Woody.png",
  plot_hex_spatial(
    raster_to_df(woody_rast, "Woody"),
    "Santiago: Woody Density (DW 10m)", "magma"
  ),
  width = 10, height = 10, dpi = 300
)

ggplot2::ggsave(
  "plots/04_Hex_Grass.png",
  plot_hex_spatial(
    raster_to_df(grass_rast, "Grass"),
    "Santiago: Grass Density (DW 10m)", "plasma"
  ),
  width = 10, height = 10, dpi = 300
)

# ── 9. Save all outputs ───────────────────────────────────────────────────────
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)

write_csv(ranking_global,       "data/results/ranking_global.csv")
write_csv(ranking_urban_parks,  "data/results/ranking_urban_parks.csv")
write_csv(ranking_lawn_parks,   "data/results/ranking_lawn_parks.csv")
write_csv(ranking_by_condition, "data/results/ranking_by_condition.csv")
write_csv(summary_global,       "data/results/summary_global.csv")
write_csv(summary_condition,    "data/results/summary_condition.csv")

# Master unified layer — all columns for Leaflet/QMD dashboard
unified_layer <- ranking_global %>%
  dplyr::select(
    park_id,
    name,
    greenspace_type,
    comuna,
    area_ha,
    area_m2,
    # Vegetation structure (Dynamic World 10m)
    mean_woody,
    mean_grass,
    woody_ratio,
    grass_ratio,
    structure_cat,
    # Thermal display (SantiagoHOT raw degrees C)
    temp_am_c,
    temp_af_c,
    temp_pm_c,
    # Thermal ranking (SantiagoHOT mean decile)
    stgohot_decile_mean,
    # Park condition (areas_verdes estado_2)
    condition,
    clase,
    # Ranking outputs
    woody_decile,
    temp_decile,
    priority_score,
    sampling_priority
  )

write_csv(unified_layer, "data/results/unified_park_ranking.csv")
saveRDS(unified_layer,   "data/results/unified_park_ranking.rds")

message("Done. Outputs saved to data/results/")
message("Unified layer: ", nrow(unified_layer), " parks")
print(dplyr::count(unified_layer, greenspace_type, sampling_priority))
