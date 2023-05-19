### EJ tool: Calculating percentile ranking for cumulative impacts air indicators

# Working directory: "X:\Agency_Files\Data_Services\DAU\Analyses\EJ\EJ Score - Metrics for Scoring\EJ Prioritization Tool Version 1\Data Files - Organized\MNRISK\cumulative_impacts_air_indicators_2017.csv"

# Load packages
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)

# Remove scientific notation
options(scipen = 999)

### Import data

# Score per census tract for hazardous  
ci <- read.csv("cumulative_impacts_air_indicators_2017.csv")

### Clean up and check data for correct number of tracts per indicators

# How many tracts are there for each indicator (in the stressor column)
ci.summary <- ci %>%
  group_by(stressor) %>%
  summarize(n_tracts = n_distinct(tract))

# 1502 for PM2.5 diesel, 1502 for proximity to traffic, 3 for the other two stressors
# For now, will rank the two with the correct number of tracts, then come back to the third one we need


### Data manipulation

# Let's keep all rows where stressor is 'PM2.5 Diesel' or 'proxmity to traffic'
# Keep only tract, value and stressor

test <- ci %>%
  filter(stressor == 'PM2.5 Diesel'|stressor == 'proximity to traffic') %>%
  select("tract", "value", "stressor")

### Percentile ranking 

# Group by stressor, then rank based on value

air <- test %>%
  group_by(stressor) %>%
  mutate(percentile_rank = percent_rank(value)*100)

# Pivot wider to get a column for each stressor value and for each percentile ranking

air_wider <- air %>%
  pivot_wider(
    names_from = stressor,
    values_from = c(value, percentile_rank)
  )

