library(tidyverse)
library(readr)
library(here)

# Load the SAHIE RDS data set
sahie_cleaned <- readRDS(here::here("dataset", "sahie_cleaned.rds"))

# Load in CDI data
cdi_cleaned <- readRDS(here::here("dataset", "dataclean.rds"))
cdi_cleaned <- cdi_cleaned %>%
  rename(Year = "YearStart")

# Cleaning CDI a little more
cdi_race <- cdi_cleaned %>%
  filter(StratificationCategory1 == "Race/Ethnicity") %>%
  mutate(Race = case_when(
    StratificationID1 == "OVR" ~ "OVR",
    StratificationID1 == "WHT" ~ "WHT",
    StratificationID1 == "BLK" ~ "BLK",
    StratificationID1 == "HIS" ~ "HIS",
    StratificationID1 == "AIAN" ~ "AIAN",
    StratificationID1 == "ASN" ~ "ASN",
    StratificationID1 == "HAPI" ~ "HAPI",
    StratificationID1 == "API" ~ "HAPI",
    StratificationID1 == "MRC" ~ "MRC",
    TRUE ~ NA_character_
  )) |>
  select(-StratificationID1, -StratificationCategory1)

cdi_2022 = cdi_race |>
  filter(Year == 2022) |>
  filter(!(LocationDesc %in% c("Virgin Islands", "Puerto Rico", "Guam", "United States")))

# Merge
merged_data <- cdi_2022 %>%
  left_join(sahie_cleaned, by = c("Year", "LocationDesc", "Race"))

# Save
saveRDS(merged_data, "merged_data_2022.rds")
