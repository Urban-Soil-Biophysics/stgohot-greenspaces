# Libraries ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)
library(stars)
library(ggspatial)
library(ggmap)
library(scales)

# Goal --------------------------------------------------------------------

# Using the LST decile analysis, check how is the distribution of
# LST in urban parks, lawn parks and roadsides greenspaces
# Data from greenspaces https://ide.minvu.cl/datasets/MINVU::catastro-de-parques-urbanos/about

# Load data ---------------------------------------------------------------

sf_use_s2(FALSE) # planar geometry sf

# Using text analysis to classify urban greenspaces (UGS)
# Metropolitan region (REGION == 13) = 754 (UGS)
# Some of the do not have official classification
# For comparison, UGS not classified are dropped

minvu <- sf::st_read("data/geo-data/parques_urbanos_minvu.geojson") %>% 
  mutate(
    greenspace_type = case_when(
      str_detect(NOMBRE, regex("parque", ignore_case = TRUE)) ~ "Urban Park",
      str_detect(NOMBRE, regex("plaza|plazoleta|jardin|jardines|santuario|estadio|deporte|deportivo|campo|piscina", ignore_case = TRUE)) ~ "Lawn Park",
      str_detect(NOMBRE, regex("bandejon|bandejón|rotonda|av|av.|canal|estero|autopista", ignore_case = TRUE)) ~ "Roadside",
      TRUE ~ NA_character_
    )
  ) %>% filter(REGION == 13) %>% 
  drop_na(greenspace_type) %>% 
  mutate(park_id = row_number(),
         area = SUPERFICIE,
         name = NOMBRE) %>% # id for double check
  select(park_id, name, area, greenspace_type) 

minvu <- st_transform(minvu, crs(deciles_stgohot_tempmean))

# -------------------------------------------------------------------------
# Use the intercept funcion exact_extract over each time (AM-AF-PM)
stgohot_extraction <- imap(
  deciles_stgohot_tempmean$deciles,
  ~ exact_extract(
    .x,
    minvu,
    include_cols = c("park_id","area","greenspace_type"),
    coverage_area = TRUE
  )
)
# -------------------------------------------------------------------------
# using mean temperature

intercept_stgohot <- imap_dfr(stgohot_extraction$deciles, ~ tibble(
  park_id = .y,
  decile = .x[["value"]],
  area_cover_q = .x[["coverage_area"]])) %>%
  filter(!is.na(decile)) %>%
  left_join(minvu) %>%
  mutate(q_proportion = area_cover_q / area) %>%
  group_by(park_id) %>%
  filter(q_proportion >= 0.01) %>%
  mutate(deciles_label = paste(paste0("Q", sort(
    unique(as.integer(decile))
  )),
  collapse = "-")) %>%
  ungroup()
