### Estimating the area percentage contributions for 2010 tracts to each 2020 tract ###
### Created by Dorian Kvale, April 2023 ###


library(tidyverse)
library(tigris)
library(sf)
library(leaflet)

# Using tigris get 2010 and 2020 census tracts
tracts_10 <- tracts("MN", cb = FALSE, year = 2010)
tracts_20 <- tracts("MN", cb = FALSE, year = 2020)

# Return the area of each tract
tracts_20$start_area <- st_area(tracts_20)

# Assign fake health value to 2010 tracts
tracts_10$asthma_rate <- rnorm(n    = nrow(tracts_10), 
                               mean = 0.28, 
                               sd   = 0.07)

range(tracts_10$asthma_rate)

# Create a plot of asthma rate per tract
plot(tracts_10[,"asthma_rate"])


# Create 2010 tract ID column
#tracts_10 <- tracts_10 %>% mutate(GEOID10 = substr(GEO_ID, 10, 20))
tracts_10 <- tracts_10 %>% rename(GEOID_2010 = GEOID10)

# The internet says to add this to make the world flat
sf_use_s2(FALSE)

# Perform intersection to get the overlapping areas
## Takes ~2 mins
overlap_all <- st_intersection(tracts_10[, c("asthma_rate", "GEOID_2010")], 
                               tracts_20)

# Check the area of overlapping pieces, then drop any < 30 meters^2 to eliminate the border segments that slightly overlapped 
overlap_all$area <- st_area(overlap_all)

overlap <- overlap_all %>% 
           filter(area > units::as_units(30, "m^2"))

# Drop LINES/POINTS
overlap <- subset(overlap, 
                  !st_geometry_type(overlap) %in% c("LINESTRING","MULTILINESTRING", "POINT")) 


# View the 2020 tracts with multiple asthma values from 2010
multi_overlap <- overlap %>%
                 group_by(GEOID) %>%
                 mutate(n = n_distinct(asthma_rate)) %>%
                 filter(n > 1)

# Leaflet plot for ground-truthing - explore how an individual 2020 tract is being split

## Legend colors
pal <- colorNumeric(palette = "YlGnBu", 
                    domain = overlap$asthma_rate)

# Inspect a single 2020 Tract split in two by 2010 tracts
## GEOID: 27131070504
leaflet(overlap %>% 
           st_transform(4326) %>% 
           filter(GEOID %in% 27131070504) %>% st_cast("POLYGON")) %>%
    addTiles() %>% #Add map tiles --> map displayed by seamlessly joining a bunch images or vectors
    #addPolygons(data = tracts_10 %>% st_transform(4326), weight = 3, color = "gray") %>%
    addPolygons(color = ~pal(asthma_rate), 
                weight = 3,
                fillColor = ~pal(asthma_rate),
                opacity = 0.9,
                fillOpacity = 0.5,
                popup = ~paste("<b>2020 Tract:</b>", GEOID, "<br>2010 asthma rate:", as.character(round(asthma_rate,2))),
                label = ~paste(GEOID, " - ", as.character(round(asthma_rate,2))))


# Create a ref table with the 2010 area weights
tract_2020_wts <- overlap %>%
          st_set_geometry(NULL) %>%
          group_by(GEOID) %>%
          mutate(total_area = sum(area),
                 area_weight_frx = as.numeric(area) / as.numeric(total_area),
                 total_frx_chk = sum(area_weight_frx)) %>%
          select(GEOID, GEOID_2010, area_weight_frx, total_frx_chk)

# Drop fractions less than 0.05% and then normalize back to 1
tract_2020_wts <- filter(tract_2020_wts, 
                         area_weight_frx > 0.05/100)

tract_2020_wts <- tract_2020_wts  %>%
                  group_by(GEOID) %>% 
                  mutate(total_frx = sum(area_weight_frx),
                         area_weight_frx = area_weight_frx / total_frx,
                         total_frx_chk = sum(area_weight_frx))

tract_2020_wts <- select(tract_2020_wts, -total_frx, total_frx_chk)

# SAVE
write_csv(tract_2020_wts %>% rename(GEOID_2020 = GEOID), "Tract_area_weights_to_extract_2010_values_to_2020.csv")

### Example of extrapolating 2010 to 2020 tracts 

# Join asthma rates based on 2010 GEOIDs
test <- tract_2020_wts %>%
  left_join(tracts_10 %>% select(asthma_rate, GEOID_2010),
            by = c("GEOID_2010" = "GEOID_2010"))

# Calculate the weighted avg to get 2020 value
avg_2020_asthma <- test %>% 
  group_by(GEOID) %>%
  summarize(wt_avg_asthma_rate = weighted.mean(asthma_rate, area_weight_frx))
