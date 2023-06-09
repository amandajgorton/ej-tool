### EJ tool: Calculating percentile ranking for hazardous waste enforcement actions

# Working directory for data: "X:\Agency_Files\Data_Services\DAU\Analyses\EJ\EJ Score - Metrics for Scoring\EJ Prioritization Tool Version 1\Data Files - Organized\Hazardous waste\hazardouswaste_2020censustracts.csv"

# Load packages
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)
library(readxl)

### Import data

# Score per census tract for hazardous waste enforcement and non-compliance inspections
hw.enf <- read.csv("hazardouswaste_2020censustracts.csv")
hw.insp <- read_excel("HW Insp Noncompliance Points by Census Tract.xlsx", sheet = "HW Insp Noncomp R")

### Clean up data, name and select relevant columns

## Enforcement
# Which columns need to be kept?
# GEOID, TRACTCE, Halfed_Point_based_on_Median

# Drop unnecessary columns and rename
hw.enf_short<- hw.enf %>% 
  rename(enf_points = Halfed_Points_based_on_Median) %>%
  select(c(GEOID, enf_points))

hw.enf_short$GEOID <- as.numeric(hw.enf_short$GEOID)

## Inspections
# Drop unnecessary columns and rename

hw.insp_short <- hw.insp %>%
  rename(insp_points = "Hazardous Waste Inspection Noncompliance Points",
         GEOID = "Census Tract") %>%
  select(c(GEOID, insp_points))

hw.insp_short$GEOID <- as.numeric(hw.insp_short$GEOID)

### Combine the inspection and enforcement points

# Add all the dataframes to a list
hw.list <- list(hw.enf_short, hw.insp_short)

# Merge by geoid - looks like there are some tracts without any points
hw <- hw.list %>%
  reduce(full_join, by = 'GEOID')

# Sum points across columns
hw_finalpoints <- hw %>%
  mutate(hw_points = rowSums(across(c(enf_points, insp_points)), na.rm=TRUE))


### Calculate percentile ranking

hw_percentiles <- hw_finalpoints %>%
  mutate(percentile_rank = percent_rank(hw_points)*100)


# Ranked percentiles for hazardous waste, by tract
write.csv(hw_percentiles, "hazardouswaste_percentiles.csv")
