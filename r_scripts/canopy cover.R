library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)

canopy <- read_csv("data/raw_data/CanopyCover.dataentry2021.csv")

summary(canopy)

canopy <- canopy %>% 
  group_by(plot) %>% 
  summarise(canopy_cover = mean(percent_cover_closed))
