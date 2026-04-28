# Libraries ---------------------------------------------------------------
library(tidyverse)

# Build count table -------------------------------------------------------
heatmap_data <- intercept_stgohot_summary %>%
  filter(time != "Mean") %>%
  count(time, greenspace_type, deciles_label) %>%
  mutate(time = factor(time, levels = c("AM", "AF", "PM")))

# Order decile labels by mean decile value (same logic as script 03) ------
labs_order <- heatmap_data %>%
  distinct(deciles_label) %>%
  mutate(
    nums = str_split(str_remove_all(deciles_label, "Q"), "-") %>%
      map(as.integer),
    key  = map_dbl(nums, mean),
    len  = map_int(nums, length)
  ) %>%
  arrange(key, len) %>%
  pull(deciles_label)

# Order greenspace types --------------------------------------------------
type_order <- c("Urban Park", "Roadside", "Lawn Park")

# Plot --------------------------------------------------------------------
heatmap_plot <- heatmap_data %>%
  mutate(
    deciles_label   = factor(deciles_label, levels = labs_order),
    greenspace_type = factor(greenspace_type, levels = type_order)
  ) %>%
  ggplot(aes(x = deciles_label, y = greenspace_type, fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(
    ~ time,
    ncol   = 1,
    scales = "free_x"    # each hour has different decile label combinations
  ) +
  scale_fill_viridis_c(
    option       = "magma",
    name         = NULL,
    na.value     = "black",
    direction    = 1
  ) +
  scale_x_discrete(drop = TRUE) +
  scale_y_discrete(drop = FALSE) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid       = element_blank(),
    strip.text       = element_blank(),           # no facet label — time shown via spacing
    axis.text.x      = element_text(
      color = "white", angle = 45, hjust = 1, size = 8
    ),
    axis.text.y      = element_text(color = "white", size = 9),
    axis.title       = element_blank(),
    legend.text      = element_text(color = "white"),
    legend.position  = "right",
    panel.spacing    = unit(1.2, "lines")
  )

ggsave(
  "outputs/comparison-satellites/STGOhot/stgohot-decile-heatmap.png",
  heatmap_plot,
  width  = 10,
  height = 10,
  dpi    = 300,
  bg     = "black"
)

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggalluvial)

# Prepare alluvial data ---------------------------------------------------
# One row per park showing decile movement AM → AF → PM
alluvial_data <- intercept_stgohot_summary %>%
  filter(time != "Mean") %>%
  select(park_id, greenspace_type, time, dominant_decile) %>%
  pivot_wider(
    names_from  = time,
    values_from = dominant_decile
  ) %>%
  drop_na(AM, AF, PM) %>%        # only parks with all three time points
  mutate(
    AM = factor(paste0("D", AM), levels = paste0("D", 1:10)),
    AF = factor(paste0("D", AF), levels = paste0("D", 1:10)),
    PM = factor(paste0("D", PM), levels = paste0("D", 1:10))
  )

# Count combinations for alluvial weight ----------------------------------
alluvial_counts <- alluvial_data %>%
  count(greenspace_type, AM, AF, PM, name = "n")

# Plot: all greenspace types combined -------------------------------------
alluvial_plot_all <- ggplot(
  alluvial_counts,
  aes(axis1 = AM, axis2 = AF, axis3 = PM, y = n)
) +
  geom_alluvium(
    aes(fill = AM),
    width     = 1/12,
    alpha     = 0.7,
    knot.pos  = 0.4
  ) +
  geom_stratum(
    width     = 1/8,
    fill      = "grey30",
    color     = "white",
    linewidth = 0.3
  ) +
  geom_text(
    stat  = "stratum",
    aes(label = after_stat(stratum)),
    size  = 3,
    color = "white"
  ) +
  scale_x_discrete(
    limits = c("AM", "AF", "PM"),
    expand = c(0.1, 0.1)
  ) +
  scale_fill_viridis_d(
    option       = "magma",
    name         = "AM decile",
    drop         = FALSE,
    na.translate = FALSE
  ) +
  labs(
    title    = "STGOHOT — decile transitions AM → AF → PM",
    subtitle = "Flow width proportional to number of parks",
    x        = NULL,
    y        = "Number of parks"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid       = element_blank(),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white"),
    plot.title       = element_text(color = "white", face = "bold"),
    plot.subtitle    = element_text(color = "white"),
    legend.text      = element_text(color = "white"),
    legend.title     = element_text(color = "white")
  )

ggsave(
  "outputs/comparison-satellites/STGOhot/stgohot-alluvial-all.png",
  alluvial_plot_all,
  width  = 10,
  height = 7,
  dpi    = 300,
  bg     = "black"
)

# Plot: faceted by greenspace type ----------------------------------------
alluvial_plot_facet <- ggplot(
  alluvial_counts,
  aes(axis1 = AM, axis2 = AF, axis3 = PM, y = n)
) +
  geom_alluvium(
    aes(fill = AM),
    width    = 1/12,
    alpha    = 0.7,
    knot.pos = 0.4
  ) +
  geom_stratum(
    width     = 1/8,
    fill      = "grey30",
    color     = "white",
    linewidth = 0.3
  ) +
  geom_text(
    stat  = "stratum",
    aes(label = after_stat(stratum)),
    size  = 2.5,
    color = "white"
  ) +
  scale_x_discrete(
    limits = c("AM", "AF", "PM"),
    expand = c(0.1, 0.1)
  ) +
  scale_fill_viridis_d(
    option       = "magma",
    name         = "AM decile",
    drop         = FALSE,
    na.translate = FALSE
  ) +
  facet_wrap(
    ~ greenspace_type,
    ncol   = 1,
    scales = "free_y"    # roadsides likely have fewer parks than urban parks
  ) +
  labs(
    title    = "STGOHOT — decile transitions by greenspace type",
    subtitle = "AM → AF → PM | Flow width proportional to number of parks",
    x        = NULL,
    y        = "Number of parks"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid       = element_blank(),
    strip.text       = element_text(color = "white", face = "bold"),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white"),
    plot.title       = element_text(color = "white", face = "bold"),
    plot.subtitle    = element_text(color = "white"),
    legend.text      = element_text(color = "white"),
    legend.title     = element_text(color = "white")
  )

ggsave(
  "outputs/comparison-satellites/STGOhot/stgohot-alluvial-by-type.png",
  alluvial_plot_facet,
  width  = 10,
  height = 12,
  dpi    = 300,
  bg     = "black"
)