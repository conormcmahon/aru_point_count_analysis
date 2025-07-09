

library(tidyverse)

allowed_species <- read_csv("D:/birdrec/scripts/birdnet_postprocessing/allowed_species.csv")

# Load all detections, filter species, merge into one dataframe, delete extras
goleta_detections <- read_csv("G:/Bioacoustics/Goleta_2025/detections_compiled/all_detections.csv") %>% filter(common_name %in% allowed_species$species)
scr_detections <- read_csv("G:/Bioacoustics/SCR_2025/detections_compiled/all_detections.csv") %>% filter(common_name %in% allowed_species$species)
all_detections <- rbind(scr_detections, goleta_detections)
rm(scr_detections, goleta_detections)
other_detections <- read_csv("G:/Bioacoustics/Goleta_Various/detections_compiled/all_detections.csv") %>% filter(common_name %in% allowed_species$species)
all_detections <- rbind(all_detections, other_detections)
rm(other_detections)

# Get number of days recorded at each site
num_days <- all_detections %>%
  filter(year == 2025, month >= 5) %>%
  group_by(site) %>%
  summarize(num_days = length(unique(doy)))
all_detections <- merge(all_detections,
                        num_days,
                        by="site")

detection_summary <- all_detections %>%
  filter(common_name == "Yellow Warbler", 
         year == 2025, month >= 5) %>%
  group_by(site) %>%
  summarize(count_norm = n()/mean(num_days, na.rm=TRUE)) %>%
  arrange(site)

detection_summary %>% View("Detection Summary")