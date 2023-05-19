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



