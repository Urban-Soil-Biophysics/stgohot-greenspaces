
# Calculate proportion of each decile in parks --------------------------

landsat_proportion <- intercept_landsat_sf %>%
  filter(!str_detect(deciles_label, "Q0")) %>% 
  mutate(deciles_label = factor(deciles_label, levels = labs_order_landsat)) %>% 
  sf::st_drop_geometry() %>%
  count(greenspace_type, deciles_label, name = "n") %>%
  group_by(greenspace_type) %>%
  mutate(pct = n / sum(n),
         lbl = percent(pct, accuracy = 1)) %>%
  ungroup()

# Proportion plot ---------------------------------------------------------

ggplot(landsat_proportion, aes(x = deciles_label, y = greenspace_type, fill = pct)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "inferno", begin = 0.2, end = 0.8,  labels = percent, name = "%\ngreenspace") +
  labs(x = NULL, y = NULL,
       title = "LANDSAT") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# save plot ---------------------------------------------------------------

ggsave("outputs/LANDSAT/landsat-proportion.tiff", width = 8, height = 3, dpi = 300)