# ── 02_intercept-stgohot-ugs.R ────────────────────────────────────────────────
# Goal: Extract SantiagoHOT temperature deciles per park polygon for
# AM, AF, PM, and daily mean time periods.
# Input:
#   stgohot_deciles          — list of hourly decile rasters (from 01_load-stgohot-tif.R)
#   deciles_stgohot_tempmean — daily mean decile raster (from 01_load-stgohot-tif.R)
#   data/geo-data/parques_urbanos_minvu.geojson
# Output:
#   data/results/intercept_stgohot_summary.rds — dominant decile per park per time
# Requires 01_load-stgohot-tif.R to have been run first in the same session.
# ─────────────────────────────────────────────────────────────────────────────

# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)

sf_use_s2(FALSE)

# Load and classify greenspaces -------------------------------------------
# Note: dplyr::filter() causes coercion error on this sf object —
# using base R [df$REGION == 13, ] instead
minvu_raw <- sf::st_read("data/geo-data/parques_urbanos_minvu.geojson")

minvu <- minvu_raw[minvu_raw$REGION == 13, ] %>%
  dplyr::mutate(
    greenspace_type = dplyr::case_when(
      str_detect(NOMBRE, regex("parque", ignore_case = TRUE)) ~ "Urban Park",
      str_detect(NOMBRE, regex("plaza|plazoleta|jardin|jardines|santuario|estadio|deporte|deportivo|campo|piscina", ignore_case = TRUE)) ~ "Lawn Park",
      str_detect(NOMBRE, regex("bandejon|bandejón|rotonda|av\\.?|canal|estero|autopista", ignore_case = TRUE)) ~ "Roadside",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(greenspace_type)) %>%
  dplyr::mutate(
    park_id = dplyr::row_number(),
    area    = as.numeric(SUPERFICIE),
    name    = as.character(NOMBRE)
  ) %>%
  dplyr::select(park_id, name, area, greenspace_type)

# Match CRS ---------------------------------------------------------------
minvu <- st_transform(minvu, crs(deciles_stgohot_tempmean))

# Helper: extract deciles from one raster ---------------------------------
# exact_extract always outputs raster values as "value" column regardless
# of the raster layer name — rename(decile = value) is always correct
extract_deciles <- function(decile_raster, polygons, time_label) {
  exactextractr::exact_extract(
    decile_raster,
    polygons,
    include_cols  = c("park_id", "area", "greenspace_type"),
    coverage_area = TRUE
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(decile = value) %>%
    dplyr::filter(!is.na(decile)) %>%
    dplyr::mutate(
      time         = time_label,
      q_proportion = coverage_area / area
    ) %>%
    dplyr::filter(q_proportion >= 0.01)
}

# Extract per hour + mean -------------------------------------------------
extraction_am   <- extract_deciles(stgohot_deciles$AM,       minvu, "AM")
extraction_af   <- extract_deciles(stgohot_deciles$AF,       minvu, "AF")
extraction_pm   <- extract_deciles(stgohot_deciles$PM,       minvu, "PM")
extraction_mean <- extract_deciles(deciles_stgohot_tempmean, minvu, "Mean")

intercept_stgohot_long <- dplyr::bind_rows(
  extraction_am,
  extraction_af,
  extraction_pm,
  extraction_mean
) %>%
  dplyr::mutate(
    # Keep time as character — avoids factor comparison errors downstream
    time = as.character(time)
  )

# Summarise: dominant decile label per park per time ----------------------
# dominant_decile: the decile class with the largest coverage area
# deciles_label:   all decile classes present (e.g. "Q5-Q6-Q7")
# as.character() before as.integer() avoids factor level index bug
intercept_stgohot_summary <- intercept_stgohot_long %>%
  dplyr::group_by(park_id, greenspace_type, area, time) %>%
  dplyr::summarise(
    dominant_decile = as.integer(
      names(which.max(tapply(coverage_area, decile, sum)))
    ),
    deciles_label = paste0(
      "Q", sort(unique(as.integer(as.character(decile)))), collapse = "-"
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    sf::st_drop_geometry(minvu) %>% dplyr::select(park_id, name),
    by = "park_id"
  ) %>%
  # Force time to character so == comparisons work in downstream scripts
  dplyr::mutate(time = as.character(time))

# Join geometry back for plotting -----------------------------------------
# Do NOT use first(geometry) — geometry does not exist in exact_extract output
# Join from minvu sf object instead
intercept_stgohot_sf <- minvu %>%
  dplyr::select(park_id) %>%
  dplyr::left_join(intercept_stgohot_summary, by = "park_id") %>%
  sf::st_as_sf()

message("Summary rows: ", nrow(intercept_stgohot_summary))
print(dplyr::count(intercept_stgohot_summary, time, greenspace_type))

# Save --------------------------------------------------------------------
# Required by scripts/ranking/04_sample_sites.R for stgohot_decile_mean
# time column stored as character to avoid factor comparison errors
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)
saveRDS(intercept_stgohot_summary, "data/results/intercept_stgohot_summary.rds")
message("Saved: data/results/intercept_stgohot_summary.rds")
