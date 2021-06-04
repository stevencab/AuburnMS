# script processing MOT tree data 2021 ####

library(readr)
library(dplyr)
library(ggplot2)

trees <- read_csv("data/raw_data/MOTtrees2021csv.csv")

summary(trees)

unique(trees$species)
unique(trees$canopy)
unique(trees$location)
sort(unique(trees$dbh))
n_distinct(trees$species)
# ^correct any errors in species name/location/canopy/dbh

percent_trees <- trees %>% 
  group_by(plot, functional_group) %>% 
  summarise(total_count = n_distinct(stem_id))

dbh_cm_in <- trees %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701)

basal_area_m2_ft2 <- dbh_cm_in %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144))

basal_area_m2_ft2_plot <- basal_area_m2_ft2 %>% 
  group_by(plot, functional_group) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)
####### plot BA total
plot_basal_area <- basal_area_m2_ft2 %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

###### checking percent pine BA per plot
pine_sp <- trees %>% 
  filter(species %in% c("PITA","PIEC"))

pine_dbh_cm_in <- pine_sp %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701)

pine_basal_area_m2_ft2 <- pine_dbh_cm_in %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144))

pine_basal_area_m2_ft2_plot <- pine_basal_area_m2_ft2 %>% 
  group_by(plot) %>% 
  summarise(pine_ba_m2ha = sum(ba_m2)/0.1011714,
            pine_ba_ft2a = sum(ba_ft2)/0.25)

ba_x_pine <- merge(plot_basal_area, pine_basal_area_m2_ft2_plot)

ba_x_pine_plot <- ba_x_pine %>%
  group_by(plot) %>% 
  summarise(pct_pine_bam2ha = (pine_ba_m2ha/ba_m2ha)*100,
            pct_pint_baft2a = (pine_ba_ft2a/ba_ft2a)*100)

ggplot(trees, aes(dbh, color = functional_group, fill = functional_group)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80),
                     limits = c(0,55)) +
  ggtitle("All Individuals DBH by Functional Group") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(trees, aes(plot, color = functional_group, fill = functional_group)), +
  geom_histogram(binwidth = 1, center = 1) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11))

plot_1 <- filter(trees, plot==1)

ggplot(plot_1, aes(dbh, color = functional_group, fill = functional_group)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80),
                     limits = c(0,55)) +
  ggtitle("Plot 1") +
  theme(plot.title = element_text(hjust = 0.5))



stem_dbh <- basal_area_in %>% 
  group_by(plot, species, functional_group) %>% 
  summarise(avg_stand_diam = mean(dbh_in))

basal_area_stem <- dbh_cm_in %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(ba = ((dbh_in)^2)*0.005454139)

basal_area_plot <- basal_area_stem %>% 
  group_by(plot, species, functional_group) %>% 
  summarise(ba_tot = sum(ba)/0.25)

basal_area_plot_2 <- basal_area_plot %>% 
  group_by(plot) %>% 
  summarise(ba_all = sum(ba_tot))

combined_sd_ba <- merge(stem_dbh, basal_area_plot)

ba_dm <- combined_sd_ba %>% 
  group_by(plot, functional_group) %>% 
  summarise(fg_sd = sum(avg_stand_diam),
            fg_ba = sum(ba_tot))

basal_area <- basal_area %>% 
  group_by(plot, species) %>%
  summarise(ba_sp_in = sum(ba_in))

cleaned_trees <- trees

write_csv(cleaned_trees, "data/processed_data/MOTtrees_clean.csv")


test <- trees %>% 
  group_by(plot, functional_group) %>% 
    summarise(avg_dbh = mean(dbh),
              sum_dbh = sum(dbh))
f1 <- filter(trees, plot==1)
filter(f1, species=="PITA")
