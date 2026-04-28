# Libraries ---------------------------------------------------------------
library(tidyverse)

# Build count table -------------------------------------------------------

heatmap_data_landsat <- intercept_landsat_summary %>%
  count(greenspace_type, deciles_label) %>%
  mutate(
    deciles_label   = factor(deciles_label, levels = labs_order_landsat),
    greenspace_type = factor(
      greenspace_type,
      levels = c("Urban Park", "Roadside", "Lawn Park")
    )
  )

# Plot --------------------------------------------------------------------
heatmap_landsat_plot <- heatmap_data_landsat %>%
  ggplot(aes(x = deciles_label, y = greenspace_type, fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(
    option    = "magma",   # ✅ consistent with STGOHOT
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
  "outputs/comparison-satellites/LANDSAT/landsat-decile-heatmap.png",
  heatmap_landsat_plot,
  width  = 10,
  height = 4,     
  dpi    = 300,
  bg     = "black"
)