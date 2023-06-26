### EJ tool: Calculating percentile ranking for solid waste data

# Working directory for data: "X:\Agency_Files\Data_Services\DAU\Analyses\EJ\EJ Score - Metrics for Scoring\EJ Prioritization Tool Version 1\Data Files - Organized\solid waste "

# Load packages
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)
library(readxl)

### Import data

# Points per census tract for each type of solid waste

compost <- read_excel("Solid Waste Compost Final Points for EJ Score Tool.xlsx", sheet = "Final Compost Points")
gas <- read_excel("Solid Waste Gas Capture Final Points for EJ Score Tool.xlsx", sheet = "Final Gas Capture Points")
leachate <- read_excel("Solid Waste Leachate Final Points for EJ Score Tool.xlsx", sheet = "Final Leachate Points")
msw <- read_excel("Solid Waste MSW Demo Industrial Combustor Waste Final Points for EJ Score Tool.xlsx", sheet = "MSW Demo Ind Comb Final Points")
land <- read_excel("Solid Waste Land Disposal Final Points for EJ Score Tool.xlsx", sheet = "SW Land Disposal EJ Points")
transfer <- read_excel("Solid Waste Transfer Areas Final Points for EJ Score Tool.xlsx", sheet = "Transfer Areas EJ Points")
wte <- read_excel("Solid Waste WTE Final Points for EJ Score Tool.xlsx", sheet = "WTE Points for EJ Score")
tire <- read_excel("Solid Waste Tire Facilities Final Points for EJ Score Tool.xlsx", sheet = "Tire Points EJ Scoring")
insp <- read_excel("Solid Waste Inspection Noncompliance Final Points for EJ Score Tool.xlsx", sheet = "SW Insp Non Points EJ Score")
enf <- read_excel("Solid Waste Enforcement Actions Final Points for EJ Score Tool.xlsx", sheet = "SW Enfor Action Points EJ")


### Clean up dataframes

# 1. Clean up column names to remove spaces, upper case
# 2. Rename points column and census tract column
# 3. Convert GEOID to a numeric variables
# 4. Drop extraneous columns

compost <- compost %>%
  clean_names %>%
  rename(compost_points = overall_compost_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "compost_points")

gas <- gas %>%
  clean_names %>%
  rename(gascapture_points = overall_gas_capture_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "gascapture_points")

leachate <- leachate %>%
  clean_names %>%
  rename(leachate_points = overall_leachate_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "leachate_points")

msw <- msw %>%
  clean_names %>%
  rename(msw_points = overall_msw_demo_ind_comb_waste_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "msw_points")

land <- land %>%
  clean_names %>%
  rename(landdisposal_points = total_land_disposal_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "landdisposal_points")

transfer <- transfer %>%
  clean_names %>%
  rename(transfer_points = total_transfer_area_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "transfer_points")

wte <- wte %>%
  clean_names %>%
  rename(wte_points = overall_wte_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "wte_points")

tire <- tire %>%
  clean_names %>%
  rename(tire_points = overall_tire_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "tire_points")

insp <- insp %>%
  clean_names %>%
  rename(insp_points = sw_insp_non_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "insp_points")

enf <- enf %>%
  clean_names %>%
  rename(enf_points = sw_enforcement_action_points, geoid = census_tract) %>%
  mutate_at('geoid', as.numeric) %>%
  select("geoid", "enf_points")


### Calculate percentiles

# 1. Merge across data types and summarize total points per tract 
# 2. Calculate percentile ranking

# Add all the dataframes to a list
sw_list <- list(compost, enf, gas, insp, land, leachate, msw, tire, transfer, wte)

# Merge by geoid
sw <- sw_list %>%
  reduce(full_join, by = 'geoid')

# Sum points across columns
sw_finalpoints <- sw %>%
  mutate(sw_points = rowSums(across(c(compost_points, enf_points, gascapture_points, insp_points, landdisposal_points, msw_points, tire_points, transfer_points, wte_points)), na.rm=TRUE))

# Calculate percentiles

test <- sw_finalpoints %>%
  mutate(percentile_rank = percent_rank(sw_points)*100) %>%
  mutate(across('percentile_rank', round, 2))

### Export data

# Ranked percentiles for each indicator, with one row per tract
write.csv(sw_ranked, "solid_waste_percentiles.csv")




