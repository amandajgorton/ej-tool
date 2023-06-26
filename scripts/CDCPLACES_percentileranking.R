### EJ tool: Calculating percentile ranking for CDC PLACES Data: Asthma Rates + Heart Disease

# Working directory: "X:\Agency_Files\Data_Services\DAU\Analyses\EJ\EJ Score - Metrics for Scoring\EJ Prioritization Tool Version 1\Data Files - Organized\PLACES data\PLACESData_CDC.csv"

# Where are these data from?
# https://www.cdc.gov/places/index.html

# Load packages
library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)

# Need these packages for assigning the 2010 values to 2020 tracts
library(tigris)
library(sf)
library(leaflet) #Might be able to omit this one

### Import data

# PLACES data for MN at a census tract level (I previously subset it just to MN to make it smaller)
MN <- read.csv("CDCPlaces_MN.csv")

### Filtering, cleaning and parsing data

# Clean up column names, rename 'locationid' to geoid, convert to numeric, and keep relevant columns

MN_2010 <- MN %>%
  clean_names %>%
  rename(geoid_2010 = location_id) %>%
  mutate_at(c('geoid_2010','data_value'), as.numeric) %>%
  select('county_name', 'measure', 'data_value', 'geoid_2010')

# Filter for asthma and heart disease only 

asthma <- MN_2010 %>% 
  filter(measure == 'Current asthma among adults aged >=18 years')

heartdisease <- MN_2010 %>% 
  filter(measure == "Coronary heart disease among adults aged >=18 years")

### Assign 2010 tracts to 2020 tracts


### Calculate percentile ranking for asthma and heart disease

asthma_percentiles <- asthma %>%
  rename(geoid = LocationID) %>%
  mutate(percentile_rank = percent_rank(Data_Value))

heartdisease_percentiles <- heartdisease %>%
  rename(geoid = LocationID) %>%
  mutate(percentile_rank = percent_rank(Data_Value))


# Ranked percentiles for percentage of adults with asthma, by tract
write.csv(asthma_percentiles, "asthma_percentiles.csv")

# Ranked percentile for percentage of adults with heart disease, by tract
write.csv(heartdisease_percentiles, "heartdisease_percentiles.csv")




