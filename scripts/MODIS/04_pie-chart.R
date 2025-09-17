
# Calculate proportion of each quintile in parks --------------------------

analysis_pie <- intercept_analysis_sf %>%
  sf::st_drop_geometry() %>%
  count(greenspace_type, quintiles_label, name = "n") %>%
  group_by(greenspace_type) %>%
  mutate(pct = n / sum(n),
         lbl = percent(pct, accuracy = 1)) %>%
  ungroup()

# Pie chart ---------------------------------------------------------------

ggplot(
  analysis_pie,
  aes(
    x = "",
    y = pct,
    fill = factor(quintiles_label, levels = labs_order)
  )) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    facet_wrap(vars(greenspace_type)) +
    scale_fill_viridis_d(
      option = "magma",
      begin = 0.2,
      end = 0.9,
      name = "LST quintiles",
      drop = FALSE
    ) +
    geom_text(
      aes(label = ifelse(pct >= 0.05, lbl, "")),
      # show labels ≥5%
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    labs(x = NULL, y = NULL, title = "Urban greenspace MODIS LST quintile") +
    theme_void(base_size = 12) +
    theme(legend.position = "right"
    )