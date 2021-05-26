# script processing MOT tree data 2021 ####

library(readr)
library(dplyr)

trees <- read_csv("data/raw_data/MOTtrees2021.csv")

summary(trees)

unique(trees$species)
unique(trees$canopy)
unique(trees$location)
sort(unique(trees$dbh))
n_distinct(trees$species)
# ^correct any errors in species name/location/canopy/dbh

basal_area_in <- trees %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_in = (dbh)*0.393701)

stem_dbh <- basal_area_in %>% 
  group_by(plot, species, functional_group) %>% 
  summarise(avg_stand_diam = mean(dbh_in))

basal_area_stem <- basal_area_in %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(ba = ((dbh_in)^2)*0.005454139)

basal_area_plot <- basal_area_stem %>% 
  group_by(plot, species, functional_group) %>% 
  summarise(ba_tot = sum(ba)/0.25)

basal_area_plot_2 <- basal_area_plot %>% 
  group_by(plot) %>% 
  summarise(ba_all = sum(ba_tot))


basal_area <- basal_area %>% 
  group_by(plot, species) %>%
  summarise(ba_sp_in = sum(ba_in))


test <- trees %>% 
  group_by(plot, functional_group) %>% 
    summarise(avg_dbh = mean(dbh),
              sum_dbh = sum(dbh))
f1 <- filter(trees, plot==1)
filter(f1, species=="PITA")
