library(dplyr)

# Load the dataset
data <- read.csv("~/ma4615-sp25-final-project-team-7/dataset/U.S._Chronic_Disease_Indicators (1).csv")

# Selecting key variables and clean the value have NA
data_selected <- data %>%
  select(YearStart, LocationDesc, Topic, Question,DataValueType,DataValue, StratificationCategory1,StratificationCategoryID1, StratificationID1) %>%
  na.omit() 

saveRDS(data_selected, "~/ma4615-sp25-final-project-team-7/dataset/dataclean.rds")

