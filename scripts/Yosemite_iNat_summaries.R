library(tidyverse)
library(stringr)

Yosemite_needsID <- read_csv("Yosemite_needsID.csv")
Yosemite_RG <- read_csv("Yosemite_ResearchGrade.csv")
park_species_list <- read_csv("NPSpecies_FullList_YOSE_20240216_051634.csv") %>%
  janitor::clean_names()
vouchers_only <- read_csv("herbarium_only_species_yosemite_v2.csv")
inat_only <- read_csv("inat_only_species_yosemite.csv")

needsID_summary <- Yosemite_needsID %>%
  select(scientific_name, observed_on, positional_accuracy, coordinates_obscured, taxon_family_name, taxon_genus_name) %>%
  group_by(scientific_name) %>%
  mutate(
    count_needs_id = n(),
    positional_accuracy = min(positional_accuracy, na.rm = TRUE),
    first_observed_on = min(observed_on, na.rm = TRUE),
    last_observed_on = max(observed_on, na.rm = TRUE),
    coordinates_obscured = all(coordinates_obscured, na.rm = TRUE),
    ) %>%
  ungroup() %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  mutate(species_or_better = ifelse(stringr::str_detect(scientific_name, " "), TRUE, FALSE)) %>%
  filter(species_or_better == TRUE) %>%
  select(-observed_on)

research_grade_summary <- Yosemite_RG %>%
  select(scientific_name, observed_on, positional_accuracy, license, coordinates_obscured, taxon_family_name, taxon_genus_name) %>%
  group_by(scientific_name) %>%
  mutate(
    count_RG = n(),
    positional_accuracy = min(positional_accuracy, na.rm = TRUE),
    first_observed_on = min(observed_on, na.rm = TRUE),
    last_observed_on = max(observed_on, na.rm = TRUE),
    coordinates_obscured = all(coordinates_obscured, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  filter(stringr::str_detect(scientific_name, " ")) %>%
  select(-observed_on, -license)

research_grade_summary_no_subsp <- Yosemite_RG %>%
  select(scientific_name, observed_on, positional_accuracy, license, coordinates_obscured, taxon_family_name, taxon_genus_name) %>%
  filter(stringr::str_detect(scientific_name, " ")) %>%
  mutate(scientific_name = stringr::word(scientific_name, start = 1, end = 2)) %>%
  group_by(scientific_name) %>%
  mutate(
    count_RG = n(),
    positional_accuracy = min(positional_accuracy, na.rm = TRUE),
    first_observed_on = min(observed_on, na.rm = TRUE),
    last_observed_on = max(observed_on, na.rm = TRUE),
    coordinates_obscured = all(coordinates_obscured, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(-observed_on, -license)

only_in_needs <- needsID_summary %>%
  select(scientific_name, coordinates_obscured, taxon_family_name, count_needs_id, positional_accuracy) %>%
  anti_join(research_grade_summary %>% select(scientific_name)) %>%
  write_csv("taxa_only_in_needsID.csv")

research_grade_observers_diversity <- Yosemite_RG %>%
  select(scientific_name, user_login) %>%
  distinct() %>%
  group_by(user_login) %>%
  mutate(
    RG_diversity_by_observer = n()
  ) %>%
  ungroup() %>%
  select(-scientific_name) %>%
  distinct()

research_grade_observers_counts <- Yosemite_RG %>%
  select(scientific_name, user_login) %>%
  group_by(user_login) %>%
  mutate(
    RG_counts_by_observer = n()
  ) %>%
  ungroup() %>%
  select(-scientific_name) %>%
  distinct()

needsID_observers_diversity <- Yosemite_needsID %>%
  select(scientific_name, user_login) %>%
  distinct() %>%
  group_by(user_login) %>%
  mutate(
    needsID_diversity_by_observer = n()
  ) %>%
  ungroup() %>%
  select(-scientific_name) %>%
  distinct()

park_species_list_through_TNRS <- 
  TNRS::TNRS(park_species_list$scientific_name[1:10], sources = "wfo")

park_species_list_through_TNRS %>% write_csv("YOSE_parks_list_updated_taxonomy.csv")

Yosemite_Park_list_wfo_aligned <- park_species_list_through_TNRS %>%
  select(Name_submitted, Accepted_name, Name_matched, Name_matched_url, Accepted_name_url, Name_matched_accepted_family) %>%
  mutate(scientific_name = ifelse(!is.na(Accepted_name), Accepted_name, Name_matched))

all_lists <- Yosemite_Park_list_wfo_aligned %>%
  mutate(in_park = "on_park_list") %>%
  select(scientific_name, in_park, family = Name_matched_accepted_family) %>%
  mutate(
    scientific_name = stringr::word(scientific_name, start = 1, end = 2),
    scientific_name = APCalign::standardise_names(scientific_name),
    scientific_name = APCalign::strip_names_2(scientific_name)
    ) %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  full_join(by = c("scientific_name"), 
            needsID_summary %>% 
              select(scientific_name, count_needs_id, taxon_family_name) %>%
              mutate(
                scientific_name = stringr::word(scientific_name, start = 1, end = 2),
                scientific_name = APCalign::standardise_names(scientific_name),
                scientific_name = APCalign::strip_names_2(scientific_name)
                ) %>%
              distinct(scientific_name, .keep_all = TRUE)
            ) %>%
  full_join(by = c("scientific_name"),
            research_grade_summary_no_subsp %>%
              select(scientific_name, count_RG, family_name2 = taxon_family_name) %>%
              mutate(
                scientific_name = stringr::word(scientific_name, start = 1, end = 2),
                scientific_name = APCalign::standardise_names(scientific_name),
                scientific_name = APCalign::strip_names_2(scientific_name)
              ) %>%
              distinct(scientific_name, .keep_all = TRUE)
            ) %>%
  mutate(
    family = ifelse(is.na(family), taxon_family_name, family),
    family = ifelse(is.na(family), family_name2, family),
    scientific_name = stringr::str_to_sentence(scientific_name)
    ) %>%
  select(scientific_name, in_park, count_RG, count_needs_id, family) %>%
  arrange(family, scientific_name) %>%
  full_join(by = c("scientific_name"),
            vouchers_only %>%
              select(scientific_name, family_v = family) %>%
              mutate(voucher = "has_voucher")) %>%
  mutate(family = ifelse(is.na(family), family_v, family)) %>%
  select(-family_v) %>%
  mutate(
    as_voucher2 = ifelse(!is.na(count_RG), "has_voucher", NA),
    iNat = ifelse(!is.na(count_RG)|!is.na(count_needs_id), "in_iNat", NA),
    as_voucher2 = ifelse(scientific_name %in% inat_only$inat_only_species, NA, as_voucher2),
    voucher = ifelse(!is.na(as_voucher2), as_voucher2, voucher)
    ) %>%
  select(-as_voucher2) %>%
  select(scientific_name, in_park, voucher, iNat, everything()) %>%
  mutate(
    park_only = ifelse(in_park == "on_park_list" & is.na(count_RG) & is.na(count_needs_id) & is.na(voucher), 1, 0),
    iNat_only = ifelse(is.na(in_park) & is.na(voucher), 1, 0),
    voucher_only = ifelse(voucher == "has_voucher" & is.na(count_RG) & is.na(count_needs_id) & is.na(in_park), 1, 0),
    park_and_voucher_only = ifelse(in_park == "on_park_list" & voucher == "has_voucher" & is.na(count_RG) & is.na(count_needs_id), 1, 0),
    park_and_voucher_only = ifelse(is.na(park_and_voucher_only), 0, park_and_voucher_only),
    park_or_voucher = ifelse(in_park == "on_park_list" | voucher == "has_voucher", 1, 0),
    park_or_voucher = ifelse(is.na(park_or_voucher), 0, park_or_voucher)
  ) %>% write_csv("merged_lists.csv", na = "")

read_csv("herbarium_only_species_yosemite_v3.csv") %>%
  left_join(all_lists %>% select(scientific_name, count_RG, count_needs_id)) %>%
  write_csv("herbarium_only_species_yosemite_v3.csv", na="")


###############

# 2024-03-08

library(tidyverse)
merged_updated <- read_csv("merged_lists.csv")
iNat_only <-read_csv("inat_only_species_yosemite.csv")
voucher_only <-read_csv("herbarium_only_species_yosemite_v4.csv")

merged_updated %>%
  full_join(iNat_only %>% select(-family), by = "scientific_name") %>%
  full_join(voucher_only %>% select(-family, -unsure, -broader, -RG, -needsID), by = "scientific_name") %>%
  left_join(voucher_only %>% select(family, broader) %>% distinct(family, .keep_all = TRUE), by = "family") %>%
  mutate(iNat_needs_ID_only = ifelse(iNat_only == 1 & is.na(count_RG), 1, 0)) %>%
  write_csv("Yosemite_all_notes.csv", na = "")

###############

# 2024-05-07

library(tidyverse)
read_csv("Yosemite_plants_summary_2024.05.06.csv") -> summary
read_csv("Yosemite_all_notes_20240424.csv") -> all_notes

summary %>%
  left_join(all_notes %>% select(scientific_name, count_RG, count_needs_id)) %>%
  write_csv("Yosemite_all_notes_2024.05.07.csv", na = "")

