library(tidyverse)
library(readr)
library(here)

# Load dataset
sahie_data <- read_csv(here::here("dataset-ignore", "sahie_2022.csv"), skip = 84)

# Select columns
sahie_clean <- sahie_data %>%
  # Keep only state-level estimates (county_name is NA)
  filter(is.na(county_name)) %>%
  
  # Select and rename relevant columns
  select(
    Year = "year",
    Age = "agecat",
    Race = "racecat",
    Sex = "sexcat",
    Income = "iprcat",
    NIPR,
    NUI,
    NIC,
    PCTUI,
    PCTIC,
    LocationDesc = "state_name"
  ) %>%
  
  mutate(Age = case_when(
    Age == 0 ~ "< 65",
    Age == 1 ~ "18-64",
    Age == 2 ~ "40-64",
    Age == 3 ~ "50-64",
    Age == 4 ~ "< 18",
    Age == 5 ~ "21-64",   # Asian alone
    TRUE ~ NA_character_
  )) |>
  
  # Recode race/ethnicity values
  mutate(Race = case_when(
    Race == 0 ~ "OVR",   # All races
    Race == 1 ~ "WHT",   # White alone, not Hispanic or Latino
    Race == 2 ~ "BLK",   # Black or African American alone
    Race == 3 ~ "HIS",   # Hispanic or Latino (any race)
    Race == 4 ~ "AIAN",  # American Indian and Alaska Native alone
    Race == 5 ~ "ASN",   # Asian alone
    Race == 6 ~ "HAPI",  # Native Hawaiian and Other Pacific Islander alone
    Race == 7 ~ "MRC",   # Two or More Races
    TRUE ~ NA_character_           # Catch any unexpected values
  )) |>
  
  mutate(Sex = case_when(
    Sex == 0 ~ "Both",
    Sex == 1 ~ "Male",
    Sex == 2 ~ "Female",
    TRUE ~ NA_character_
  )) |>
  
  mutate(Income = case_when(
    Income == 0 ~ "All",
    Income == 1 ~ "< 200% poverty",
    Income == 2 ~ "< 250% poverty",
    Income == 3 ~ "< 135% poverty",
    Income == 4 ~ "< 400% poverty",
    Income == 5 ~ "138-400% poverty",
    TRUE ~ NA_character_
  ))

# Converting columns to numeric
sahie_clean <- sahie_clean %>%
  mutate(
    NIPR = as.numeric(gsub(",", "", NIPR)),
    NUI = as.numeric(gsub(",", "", NUI)),
    NIC = as.numeric(gsub(",", "", NIC)),
    PCTUI = as.numeric(gsub("%", "", PCTUI)),
    PCTIC = as.numeric(gsub("%", "", PCTIC))
  )

saveRDS(sahie_clean, file = here::here("dataset", "sahie_cleaned.rds"))
