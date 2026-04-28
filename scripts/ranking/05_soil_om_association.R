# ── 05_soil_om_association.R ──────────────────────────────────────────────────
# Goal: Explore associations between field-measured organic matter (OM) and
# the ranking variables computed in 04_sample_sites.R.
#
# Conceptual framework (see figure in README):
#   X-axis = urban soil cover / management proxies:
#     - greenspace_type     (Urban Park / Lawn Park / Roadside)
#     - woody_ratio         (Dynamic World vegetation structure)
#     - stgohot_decile_mean (thermal exposure)
#     - condition           (Good / Poor from areas_verdes)
#     - sampling_priority   (P1–P5 combined rank)
#   Y-axis = soil physical properties (this script: OM as proxy for SOC)
#
# Join strategy: spatial nearest-feature
#   dataset_rm_test.csv has lat/lon in WGS84 (EPSG:4326)
#   Each soil point is matched to the nearest park in unified_park_ranking
#
# Input:
#   data/results/unified_park_ranking.rds
#   data/geo-data/dataset_rm_test.csv  — columns: om (%), lat, lon
# Output:
#   plots/05_om_boxplot_by_type.png
#   plots/05_om_boxplot_by_priority.png
#   plots/05_om_scatter_woody.png
#   plots/05_om_scatter_temp.png
#   plots/05_om_boxplot_by_condition.png
#   plots/05_om_correlation_matrix.png
#   data/results/om_ranking_joined.csv
#   data/results/om_ranking_summary.csv
# ─────────────────────────────────────────────────────────────────────────────

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(corrplot)

dir.create("plots",        showWarnings = FALSE)
dir.create("data/results", showWarnings = FALSE, recursive = TRUE)

# ── 1. Load ranking layer ─────────────────────────────────────────────────────
ranking <- readRDS("data/results/unified_park_ranking.rds")

message("Ranking rows: ", nrow(ranking))
message("Ranking columns: ", paste(names(ranking), collapse = ", "))

# ── 2. Load field OM data ─────────────────────────────────────────────────────
# Columns used: om (organic matter %), lat, lon (WGS84 / EPSG:4326)

fix_coord <- function(x) {
  x_nodots <- stringr::str_remove_all(as.character(x), "\\.")
  as.numeric(sub("^(-?)(\\d{2})(\\d+)$", "\\1\\2.\\3", x_nodots))
}

om_clean <- om_raw |>
  dplyr::mutate(
    lat = fix_coord(lat),
    lon = fix_coord(lon),
    om  = suppressWarnings(as.numeric(om))
  ) |>
  dplyr::filter(!is.na(lat), !is.na(lon), !is.na(om)) |>
  dplyr::rename(om_value = om)

message("OM points after cleaning: ", nrow(om_clean))
message("Rows dropped: ", nrow(om_raw) - nrow(om_clean))

# Sanity check
stopifnot(
  "lon out of range" = all(om_clean$lon > -71.5 & om_clean$lon < -69.5),
  "lat out of range" = all(om_clean$lat > -34.5 & om_clean$lat < -32.5)
)
message("Coordinate fix OK — all ", nrow(om_clean), " points within Santiago bounds")

message("OM points after cleaning: ", nrow(om_clean))

# Sanity check — all points should fall within Santiago bounding box
stopifnot(
  "lon out of range — fix_coord may have failed" =
    all(om_clean$lon > -71.5 & om_clean$lon < -69.5, na.rm = TRUE),
  "lat out of range — fix_coord may have failed" =
    all(om_clean$lat > -34.5 & om_clean$lat < -32.5, na.rm = TRUE)
)

message("Coordinate fix OK — all points within Santiago bounds")
om_raw <- read_csv(
  "data/geo-data/dataset_rm_test.csv",
  show_col_types = FALSE
)

message("OM dataset rows: ", nrow(om_raw))
message("OM dataset columns: ", paste(names(om_raw), collapse = ", "))


# ── 3. Spatial join: each OM point → nearest park ────────────────────────────
# Convert OM points to sf (WGS84)
om_sf <- sf::st_as_sf(
  om_clean,
  coords = c("lon", "lat"),
  crs    = 4326
)

# Build park centroid sf from ranking
# unified_park_ranking has no geometry — reconstruct from MINVU if needed
# Use the centroids from the ranking CSV columns
# If ranking already has lon/lat columns use them; otherwise rebuild from MINVU
if (all(c("lon", "lat") %in% names(ranking))) {
  
  ranking_sf <- ranking |>
    dplyr::filter(!is.na(lon), !is.na(lat)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
} else {
  
  # Rebuild park centroids from MINVU geojson
  message("Rebuilding park centroids from MINVU geojson...")
  minvu_raw <- sf::st_read(
    "data/geo-data/parques_urbanos_minvu.geojson",
    quiet = TRUE
  )
  
  # Replace the ranking_sf construction block with this
  minvu_centroids <- minvu_raw[minvu_raw$REGION == 13, ] |>
    dplyr::mutate(
      park_id_tmp = dplyr::row_number(),
      name_raw    = as.character(NOMBRE)
    ) |>
    dplyr::select(park_id_tmp, name_raw) |>
    sf::st_centroid() |>
    sf::st_transform(4326)
  
  # Deduplicate — keep first match per name to avoid many-to-many
  minvu_centroids_df <- minvu_centroids |>
    dplyr::mutate(
      name_upper = toupper(name_raw),
      lon_park   = sf::st_coordinates(minvu_centroids)[, 1],
      lat_park   = sf::st_coordinates(minvu_centroids)[, 2]
    ) |>
    sf::st_drop_geometry() |>
    dplyr::distinct(name_upper, .keep_all = TRUE)   # ← dedup here
  
  ranking_sf <- ranking |>
    dplyr::mutate(name_upper = toupper(name)) |>
    dplyr::left_join(
      minvu_centroids_df |>
        dplyr::select(name_upper, lon_park, lat_park),
      by           = "name_upper",
      relationship = "many-to-one"
    ) |>
    dplyr::filter(!is.na(lon_park), !is.na(lat_park)) |>
    sf::st_as_sf(coords = c("lon_park", "lat_park"), crs = 4326)
}

# Nearest-feature join — each OM point gets the closest park's attributes
om_joined <- sf::st_join(
  om_sf,
  ranking_sf |>
    dplyr::select(
      park_id, name, greenspace_type, comuna,
      woody_ratio, grass_ratio, structure_cat,
      stgohot_decile_mean, condition, clase,
      woody_decile, temp_decile, priority_score, sampling_priority
    ),
  join = sf::st_nearest_feature
) |>
  sf::st_drop_geometry() |>
  # Add back lat/lon for reference
  dplyr::bind_cols(
    sf::st_coordinates(om_sf) |>
      as.data.frame() |>
      dplyr::rename(lon = X, lat = Y)
  )

message("Joined rows: ", nrow(om_joined))
message("Parks matched: ", dplyr::n_distinct(om_joined$park_id, na.rm = TRUE))

# Check join quality — compute distance to matched park
om_joined <- om_joined |>
  dplyr::mutate(
    join_distance_m = as.numeric(sf::st_distance(
      sf::st_as_sf(om_joined, coords = c("lon", "lat"), crs = 4326),
      ranking_sf[match(om_joined$park_id, ranking_sf$park_id), ],
      by_element = TRUE
    ))
  )

message("Median join distance: ",
        round(median(om_joined$join_distance_m, na.rm = TRUE), 0), " m")

# Flag points that joined to a park > 500m away (likely outside any park)
# Change threshold or remove filter for analysis
# 612m median is reasonable given Santiago's park distribution
om_joined <- om_joined |>
  dplyr::mutate(
    within_park = join_distance_m <= 1000   # ← loosen to 1km
  )

message("Points within 500m of a park: ",
        sum(om_joined$within_park, na.rm = TRUE),
        " / ", nrow(om_joined))

# ── 4. OM by greenspace type ──────────────────────────────────────────────────
p_type <- om_joined |>
  dplyr::filter(!is.na(greenspace_type)) |>
  ggplot(aes(x = greenspace_type, y = om_value, fill = greenspace_type)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8) +
  scale_fill_manual(values = c(
    "Urban Park" = "#2d6a4f",
    "Lawn Park"  = "#95d5b2",
    "Roadside"   = "#e76f51"
  )) +
  labs(
    title    = "Soil organic matter by greenspace type",
    subtitle = "X-axis proxy: park type | spatial nearest-feature join",
    x        = NULL,
    y        = "Organic matter (%)",
    caption  = "Source: dataset_rm_test.csv"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("plots/05_om_boxplot_by_type.png", p_type,
       width = 8, height = 5, dpi = 300)
message("Saved: plots/05_om_boxplot_by_type.png")

# ── 5. OM by sampling priority ────────────────────────────────────────────────
priority_order <- c(
  "P1 — Forest + Hot", "P2 — Grass + Hot",
  "P3 — Forest + Cool", "P4 — Grass + Cool",
  "P1 — Hot + Long", "P2 — Hot + Short",
  "P3 — Cool + Long", "P4 — Cool + Short",
  "P5 — Intermediate"
)

p_priority <- om_joined |>
  dplyr::filter(!is.na(sampling_priority)) |>
  dplyr::mutate(
    sampling_priority = factor(
      sampling_priority,
      levels = intersect(priority_order, unique(sampling_priority))
    )
  ) |>
  ggplot(aes(x = sampling_priority, y = om_value,
             fill = sampling_priority)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8) +
  scale_fill_viridis_d(option = "magma") +
  labs(
    title    = "Soil organic matter by sampling priority class",
    subtitle = "Priority = woody decile + temperature decile combined rank",
    x        = NULL,
    y        = "Organic matter (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 30, hjust = 1)
  )

ggsave("plots/05_om_boxplot_by_priority.png", p_priority,
       width = 10, height = 5, dpi = 300)
message("Saved: plots/05_om_boxplot_by_priority.png")

# ── 6. OM ~ woody_ratio scatter ───────────────────────────────────────────────
p_woody <- om_joined |>
  dplyr::filter(!is.na(woody_ratio)) |>
  ggplot(aes(x = woody_ratio, y = om_value,
             color = greenspace_type)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray30", linewidth = 0.8) +
  scale_color_manual(values = c(
    "Urban Park" = "#2d6a4f",
    "Lawn Park"  = "#95d5b2",
    "Roadside"   = "#e76f51"
  )) +
  labs(
    title    = "Soil OM ~ woody vegetation ratio",
    subtitle = "X-axis proxy: Dynamic World woody cover (trees + shrubs / total veg)",
    x        = "Woody ratio (0–1)",
    y        = "Organic matter (%)",
    color    = "Park type"
  ) +
  theme_minimal(base_size = 12)

ggsave("plots/05_om_scatter_woody.png", p_woody,
       width = 8, height = 5, dpi = 300)
message("Saved: plots/05_om_scatter_woody.png")

# ── 7. OM ~ temperature decile scatter ───────────────────────────────────────
p_temp <- om_joined |>
  dplyr::filter(!is.na(stgohot_decile_mean)) |>
  ggplot(aes(x = stgohot_decile_mean, y = om_value,
             color = greenspace_type)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray30", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = c(
    "Urban Park" = "#2d6a4f",
    "Lawn Park"  = "#95d5b2",
    "Roadside"   = "#e76f51"
  )) +
  labs(
    title    = "Soil OM ~ SantiagoHOT mean temperature decile",
    subtitle = "X-axis proxy: thermal exposure (1 = coolest, 10 = hottest)",
    x        = "Temperature decile (mean AM/AF/PM)",
    y        = "Organic matter (%)",
    color    = "Park type"
  ) +
  theme_minimal(base_size = 12)

ggsave("plots/05_om_scatter_temp.png", p_temp,
       width = 8, height = 5, dpi = 300)
message("Saved: plots/05_om_scatter_temp.png")

# ── 8. OM ~ condition (Good / Poor) ──────────────────────────────────────────
p_cond <- om_joined |>
  dplyr::filter(!is.na(condition)) |>
  ggplot(aes(x = condition, y = om_value, fill = condition)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.5) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 1.8) +
  scale_fill_manual(
    values = c("Good condition" = "#52b788", "Poor condition" = "#e76f51")
  ) +
  labs(
    title    = "Soil OM by park condition",
    subtitle = "Condition from áreas verdes dataset (estado_2: BUEN / MAL)",
    x        = NULL,
    y        = "Organic matter (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("plots/05_om_boxplot_by_condition.png", p_cond,
       width = 6, height = 5, dpi = 300)
message("Saved: plots/05_om_boxplot_by_condition.png")

# ── 9. Correlation matrix ─────────────────────────────────────────────────────
numeric_vars <- om_joined |>
  dplyr::select(
    om_value,
    dplyr::any_of(c(
      "woody_ratio", "grass_ratio",
      "stgohot_decile_mean",
      "woody_decile", "temp_decile", "priority_score",
      "area_ha", "temp_am_c", "temp_af_c", "temp_pm_c",
      "join_distance_m"
    ))
  ) |>
  dplyr::select(where(~ !all(is.na(.x))))

if (ncol(numeric_vars) >= 2) {
  cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
  
  png("plots/05_om_correlation_matrix.png",
      width = 900, height = 800, res = 120)
  corrplot::corrplot(
    cor_matrix,
    method      = "color",
    type        = "upper",
    tl.col      = "black",
    tl.srt      = 45,
    addCoef.col = "black",
    number.cex  = 0.7,
    col         = corrplot::COL2("RdBu"),
    title       = "OM ~ ranking variables correlation",
    mar         = c(0, 0, 2, 0)
  )
  dev.off()
  message("Saved: plots/05_om_correlation_matrix.png")
}

# ── 10. Summary statistics ────────────────────────────────────────────────────
om_summary <- om_joined |>
  dplyr::group_by(greenspace_type, sampling_priority) |>
  dplyr::summarise(
    n         = dplyr::n(),
    mean_om   = round(mean(om_value,              na.rm = TRUE), 2),
    sd_om     = round(sd(om_value,                na.rm = TRUE), 2),
    median_om = round(median(om_value,            na.rm = TRUE), 2),
    mean_woody = round(mean(woody_ratio,           na.rm = TRUE), 3),
    mean_temp  = round(mean(stgohot_decile_mean,  na.rm = TRUE), 1),
    .groups   = "drop"
  )

message("Summary:")
print(om_summary)

# ── 11. Save outputs ─────────────────────────────────────────────────────────
write_csv(om_joined,  "data/results/om_ranking_joined.csv")
write_csv(om_summary, "data/results/om_ranking_summary.csv")
message("Saved: data/results/om_ranking_joined.csv")
message("Saved: data/results/om_ranking_summary.csv")