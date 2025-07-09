
library(Distance)
library(tidyverse)

#target_species <- "Dark-eyed Junco"
target_species <- "Orange-crowned Warbler"
#target_species <- unique(point_counts$species)

#target_species <- c("Yellow Warbler", "Orange-crowned Warbler", "Common Yellowthroat", "Wilson's Warbler", "Yellow-breasted Chat", "Wrentit", "Bushtit")
#target_species <- c("Acorn Woodpecker", "Downy Woodpecker", "Hairy Woodpecker", "Nuttall's Woodpecker")
#target_species <- c("Western Flycatcher", "Ash-throated Flycatcher", "Western Kingbird", "Cassin's Kingbird")
#target_species <- c("Western Flycatcher", "Ash-throated Flycatcher", "Western Kingbird", "Cassin's Kingbird", "Cliff Swallow", "Tree Swallow", "Violet-green Swallow", "Barn Swallow", "Northern Rough-winged Swallow")
#target_species <- c("Song Sparrow", "Spotted Towhee", "California Towhee", "Savannah Sparrow", "House Finch", "Lesser Goldfinch", "American Goldfinch", "Purple Finch")
#target_species <- c("Song Sparrow", "Spotted Towhee", "California Towhee", "Savannah Sparrow", "Dark-eyed Junco")
#target_species <- c("Anna's Hummingbird", "Allen's Hummingbird", "Costa's Hummingbird")
#target_species <- c("Mourning Dove", "Eurasian-collared Dove", "Common Ground-Dove", "Band-tailed Pigeon")
#target_species <- "Yellow Warbler"

# Parulidae
#target_species <- c("Yellow Warbler", "Orange-crowned Warbler", "Common Yellowthroat", "Wilson's Warbler")

# Cavity-nesters
#target_species <- c("White-breasted Nuthatch", "Western Bluebird", "Chestnut-backed Chickadee", "Oak Titmouse", "Violet-green Swallow", "Tree Swallow", "Ash-throated Flycatcher", "Downy Woodpecker", "Hairy Woodpecker", "Acorn Woodpecker", "Nuttall's Woodpecker", "Northern Flicker")
# Cup-nesters
#target_species <- c("Yellow Warbler", "Orange-crowned Warbler", "Common Yellowthroat", "Wilson's Warbler", "Yellow-breasted Chat", "Wrentit", "House Finch", "Lesser Goldfinch", "Song Sparrow")


allowable_behavior_code <- "S"

allowed_sites <- c("Atascadero Creek",
                   "Cuyama",
                   "Elings",
                   "Ellwood",
                   "NCOS",
                   "San Jose Creek",
                   "Santa Clara",
                   "Sedgwick Ranch"
                   )


min_date <- as.Date("2025-05-01")
max_date <- as.Date("2025-07-31")

truncation_threshold <- 80


point_counts <- read_csv("aru_point_counts.csv") %>%
  janitor::clean_names()
 
# point_counts %>% group_by(species) %>% 
#    summarize(count = n(), 
#              mean_dist = mean(distance, na.rm=TRUE)) %>% 
#    arrange(-count) %>% 
#    View("Data Summary by Species") 

point_counts <- point_counts %>%
  mutate(Region.Label = paste(site, station),
         Sample.Label = paste(site, station, date, time_start),
         time_frac = hour(point_counts$time_start) + minute(point_counts$time_start)/60,
         hour = hour(time_start)+minute(time_start)/60,
         date = lubridate::mdy(point_counts$date)) %>%
  mutate(doy = lubridate::yday(date),
         allowed = grepl(allowable_behavior_code, behavior)) %>%
  mutate(lubridate::month(date) >= 5) 

object_data <- point_counts %>% 
  filter(species %in% target_species,
         allowed,
         date >= min_date,
         date <= max_date,
         site %in% allowed_sites) %>% 
  mutate(Area=pi*truncation_threshold^2) %>% 
  mutate(object = 1:n())
region_data <- point_counts %>% 
  group_by(Region.Label) %>% 
  summarize(Area = truncation_threshold^2 * pi)
sample_data <- point_counts %>% 
  mutate(Area=pi*truncation_threshold^2) %>% 
  dplyr::select(Sample.Label,Region.Label) %>% 
  group_by(Region.Label, Sample.Label) %>% 
  summarize(Region.Label=Region.Label[[1]], 
            Effort=1)
new_mod <- ds(object_data, 
              truncation = truncation_threshold, 
              obs_table = object_data %>% 
                dplyr::select(object, Region.Label, Sample.Label), 
              sample_table = sample_data, 
              region_table = region_data,
              transect = "point",
              formula = ~time_frac + doy)
summary(new_mod)

distance_df <- new_mod[[2]][[1]]$D %>%
  janitor::clean_names()
site_list <- str_split(distance_df$label, pattern=" ")
site_list <- lapply(site_list,
                    function(str_list){
                      return(paste(str_list[1:(length(str_list)-1)],
                                   collapse=" "))
                    }) %>%
  unlist()
distance_df <- distance_df %>%
  mutate(site = site_list)

setMin <- function(x, min){
  x[x <= min] <- min
  return(x)
}

ggplot(distance_df) + 
  geom_errorbar(aes(x=label, y=estimate*10^4, ymin=setMin((estimate-se)*10^4, 0), ymax=(estimate+se)*10^4, col=site)) + 
  geom_point(aes(x=label, y=estimate*10^4, col=site)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) + 
  xlab("Survey Location") + 
  ylab("Density (birds/hectare)") + 
  scale_y_continuous(expand=c(0,0.1))


summary(new_mod)[1]