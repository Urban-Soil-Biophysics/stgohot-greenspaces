# Calculate proportion of each decile in parks --------------------------

stgohot_proportion <- intercept_stgohot_sf %>%
  filter(!str_detect(deciles_label, "Q0")) %>%
  mutate(deciles_label = factor(deciles_label, levels = labs_order_stgohot)) %>%
  sf::st_drop_geometry() %>%
  count(greenspace_type, deciles_label, name = "n") %>%
  group_by(greenspace_type) %>%
  mutate(
    pct = n / sum(n),
    lbl = percent(pct, accuracy = 1)
  ) %>%
  ungroup()

# Proportion plot ---------------------------------------------------------

stgohot_proportion_plot <-
  ggplot(
    stgohot_proportion,
    aes(x = deciles_label, y = greenspace_type, fill = pct)
  ) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    option = "inferno",
    begin = 0.2,
    end = 0.8,
    labels = percent,
    name = "%\ngreenspace"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "STGOHOT"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# save plot ---------------------------------------------------------------

ggsave(
  "outputs/STGOHOT/stgohot-proportion.tiff",
  stgohot_proportion_plot,
  width = 8,
  height = 3,
  dpi = 300
)
