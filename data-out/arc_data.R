# Write summary files for ArcGIS dashboard
library(tidyverse)

MAX_YEAR <- 2023

cleanups_tidy <- 
  read_csv(here::here("data/cleanups_tidy.csv")) %>% 
  filter(year <= MAX_YEAR)

cleanups_tidy %>% 
  summarize(
    num_events = n(),
    num_years = n_distinct(year),
    num_locations = n_distinct(location),
    dist_crews = n_distinct(crew_name),
    across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
  ) %>% 
  mutate(
    total_items = 
      sum(
        plastic_bags, paper_bags, plastic_bottles, glass_bottles, 
        plastic_bottle_caps, metal_cans_caps_tabs, plastic_cups_lids,
        paper_cups_lids, foam_cups_lids, plastic_takeout_plates,
        paper_takeout_plates, foam_takeout_plates, utensils, straws,
        food_waste, food_wrappers, plastic_pcs, paper_pcs, foam_pcs,
        metal_pcs, glass_pcs, sanitary_items, covid_items, syringes,
        cigarette_butts, tobacco, fireworks, dog_waste_bag, dog_waste_pile,
        human_waste, wildlife_waste_trash, sled_pcs, misc
      )
  ) %>% 
  select(-year) %>% 
  write_csv(here::here("data-out/cleanups_summarized.csv"))

cleanups_tidy %>% 
  summarize(
    across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
  ) %>% 
  select(-c(year:area_acres)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "count"
  ) %>% 
  mutate(
    category = str_replace_all(category, "_", " ") %>% str_to_sentence(),
    category = str_replace_all(category, "pcs", "pieces"),
    category = str_replace_all(category, "cups lids", "cups/lids"),
    category = str_replace_all(category, "takeout plates", "takeout/plates"),
    category =
      case_match(
        category,
        "Metal cans caps tabs" ~ "Metal cans/caps/tabs",
        "Dog waste pile"       ~ "Dog waste (pile)",
        "Dog waste bag"        ~ "Dog waste (bagged)",
        "Misc"                 ~ "Miscellaneous",
        .default = category
      )
  ) %>% 
  write_csv(here::here("data-out/cleanups_all_items.csv"))

cleanups_total <-
  cleanups_tidy %>% 
  group_by(year) %>% 
  summarize(
    cleanup = "Total",
    num_events = n(),
    num_crews = n_distinct(crew_name),
    weight_lbs = sum(weight_lbs, na.rm = TRUE)
  ) %>% 
  ungroup()

cleanups_tidy %>% 
  group_by(year, cleanup) %>% 
  summarize(
    num_events = n(),
    num_crews = n_distinct(crew_name),
    weight_lbs = sum(weight_lbs, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  bind_rows(cleanups_total) %>% 
  write_csv(here::here("data-out/cleanups_group_year.csv"))

cleanups_tidy %>% 
  filter(year <= MAX_YEAR) %>% 
  write_csv(here::here("data-out/cleanups_tidy.csv"))
