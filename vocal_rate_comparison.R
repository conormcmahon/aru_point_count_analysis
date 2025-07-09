
library(tidyverse)

#detections <- read_csv("G:/Bioacoustics/Goleta_2024/detections_compiled/all_detections.csv")
#effort <- read_csv("G:/Bioacoustics/Goleta_2024/effort/survey_time.csv")

yewa <- detections %>% filter(common_name == "Orange-crowned Warbler", confidence >= 0.0) 

yewa_summary <- yewa %>%
  # filter(site %in% paste("SCR-0", c(13,14,22,11,20,21,32,33,18,31), sep="")) %>%
  mutate(week = floor(doy/14)) %>%
  group_by(site, week) %>% 
  summarize(total_calls = n()) %>%
  group_by(site) %>% 
  mutate(normalized_calls = total_calls / mean(total_calls))

yewa_effort_summary <- merge(yewa_summary, 
                             effort %>% 
                               mutate(week = floor(doy/14)) %>% 
                               group_by(site, week) %>% 
                               summarize(total_hours = sum(length) / 3600), 
                             by = c("site","week"),
                             all = TRUE) %>%
  mutate(time_normalized_calls = total_calls / total_hours) %>%
  replace_na(list("total_calls" = 0, "time_normalized_calls" = 0)) %>% 
  group_by(site) %>% 
  mutate(both_normalized_calls = time_normalized_calls / mean(time_normalized_calls)) 

ggplot(yewa_effort_summary %>% filter(total_hours >= 15)) +
  geom_smooth(aes(x=as.Date("2023-01-01") + week*14, y=time_normalized_calls), se=FALSE) + 
  geom_point(aes(x=as.Date("2023-01-01") + week*14, y=time_normalized_calls), se=FALSE) + 
  facet_wrap(~site) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw()


ggplot(yewa_effort_summary %>% filter(total_hours >= 15)) +
  geom_smooth(aes(x=as.Date("2023-01-01") + week*14, y=both_normalized_calls)) + 
  geom_point(aes(x=as.Date("2023-01-01") + week*14, y=both_normalized_calls)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme_bw()