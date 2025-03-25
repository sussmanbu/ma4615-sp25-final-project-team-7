library(dplyr)
library(tidyverse)
library(here)

# Load the dataset
data <- read_csv(here::here("dataset", "U.S._Chronic_Disease_Indicators (1).csv"))

# Selecting key variables, cleaning NA values, and filtering out outliers
data_selected <- data %>%
  select(YearStart, LocationDesc, Topic, Question, DataValueType, DataValue, StratificationID1, StratificationCategory1) %>%
  filter(DataValue < 1000000) %>%
  na.omit()

saveRDS(data_selected, file = here::here("dataset", "dataclean.rds"))
