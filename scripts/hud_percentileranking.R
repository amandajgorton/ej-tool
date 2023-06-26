### EJ tool: Calculating percentile ranking for HUD data

# Working directory for data: X:\Agency_Files\Data_Services\DAU\Analyses\EJ\EJ Score - Metrics for Scoring\EJ Prioritization Tool Version 1\Data Files - Organized\HUD

# Load packages
library(janitor)
library(dplyr)
library(tidyverse)

### Import data

hud <- read.csv("HUD_table8_costburden.csv")

### Filter for MN, clean names and drop extraneous columns

# According to the CalEnvironScreen, we want variable T8_est_102 (and the associated margin of error)
# Renter occupied, greater than 50% but less than or equal to 80% of HAMFI (area median family income)

test <- hud %>%
  clean_names %>%
  filter(st == 27) %>%
  #mutate_at('geoid', as.numeric) %>%
  select("geoid", "t8_est102", "t8_moe102")
 
# Need to chop off the first few characters of the geoid