# Libraries ---------------------------------------------------------------
library(tidyverse)

# Build count table -------------------------------------------------------
# ✅ uses intercept_modis_summary (no geometry) — not broken intercept_modis_sf
# ✅ count n not pct — consistent with STGOHOT/Landsat heatmap style
heatmap_data_modis <- intercept_modis_summary %>%
  count(greenspace_type, deciles_label) %>%
  mutate(
    deciles_label   = factor(deciles_label, levels = labs_order_modis),
    greenspace_type = factor(
      greenspace_type,
      levels = c("Urban Park", "Roadside", "Lawn Park")
    )
  )

# Plot --------------------------------------------------------------------
heatmap_modis_plot <- heatmap_data_modis %>%
  ggplot(aes(x = deciles_label, y = greenspace_type, fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(
    option    = "magma",       # ✅ consistent with STGOHOT/Landsat
    name      = NULL,
    na.value  = "black",
    direction = 1
  ) +
  scale_x_discrete(drop = TRUE) +
  scale_y_discrete(drop = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(
      color = "white", angle = 45, hjust = 1, size = 8
    ),
    axis.text.y      = element_text(color = "white", size = 9),
    axis.title       = element_blank(),
    legend.text      = element_text(color = "white"),
    legend.position  = "right"
  )

ggsave(
  "outputs/comparison-satellites/MODIS/modis-decile-heatmap.png",
  heatmap_modis_plot,
  width  = 10,
  height = 4,      # single panel — no time dimension
  dpi    = 300,
  bg     = "black"
)