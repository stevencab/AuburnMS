# script processing MOT tree data 2021 ####

library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)

trees <- read_csv("data/raw_data/MOTtrees2021csv.csv")

summary(trees)

unique(trees$species)
unique(trees$canopy)
unique(trees$location)
sort(unique(trees$dbh))
n_distinct(trees$species)
trees$functional_group[trees$species=="PRUNUS"] <- "mesophyte"
trees$functional_group[trees$species=="SW"] <- "mesophyte"

#assigning burn seasons (burn_szn) to all plots and quadrants randomly (did in excel)

trees$burn_szn[trees$plot==1 & trees$location==1] <- "late"
trees$burn_szn[trees$plot==1 & trees$location==2] <- "early"
trees$burn_szn[trees$plot==1 & trees$location==3] <- "growing"
trees$burn_szn[trees$plot==1 & trees$location==4] <- "control"

trees$burn_szn[trees$plot==2 & trees$location==1] <- "growing"
trees$burn_szn[trees$plot==2 & trees$location==2] <- "late"
trees$burn_szn[trees$plot==2 & trees$location==3] <- "early"
trees$burn_szn[trees$plot==2 & trees$location==4] <- "control"

trees$burn_szn[trees$plot==3 & trees$location==1] <- "early"
trees$burn_szn[trees$plot==3 & trees$location==2] <- "late"
trees$burn_szn[trees$plot==3 & trees$location==3] <- "control"
trees$burn_szn[trees$plot==3 & trees$location==4] <- "growing"

trees$burn_szn[trees$plot==4 & trees$location==1] <- "control"
trees$burn_szn[trees$plot==4 & trees$location==2] <- "late"
trees$burn_szn[trees$plot==4 & trees$location==3] <- "growing"
trees$burn_szn[trees$plot==4 & trees$location==4] <- "early"

trees$burn_szn[trees$plot==5 & trees$location==1] <- "early"
trees$burn_szn[trees$plot==5 & trees$location==2] <- "control"
trees$burn_szn[trees$plot==5 & trees$location==3] <- "growing"
trees$burn_szn[trees$plot==5 & trees$location==4] <- "late"

trees$burn_szn[trees$plot==6 & trees$location==1] <- "growing"
trees$burn_szn[trees$plot==6 & trees$location==2] <- "early"
trees$burn_szn[trees$plot==6 & trees$location==3] <- "control"
trees$burn_szn[trees$plot==6 & trees$location==4] <- "late"

trees$burn_szn[trees$plot==7 & trees$location==1] <- "late"
trees$burn_szn[trees$plot==7 & trees$location==2] <- "growing"
trees$burn_szn[trees$plot==7 & trees$location==3] <- "control"
trees$burn_szn[trees$plot==7 & trees$location==4] <- "early"

trees$burn_szn[trees$plot==8 & trees$location==1] <- "growing"
trees$burn_szn[trees$plot==8 & trees$location==2] <- "control"
trees$burn_szn[trees$plot==8 & trees$location==3] <- "early"
trees$burn_szn[trees$plot==8 & trees$location==4] <- "late"

trees$burn_szn[trees$plot==9 & trees$location==1] <- "early"
trees$burn_szn[trees$plot==9 & trees$location==2] <- "late"
trees$burn_szn[trees$plot==9 & trees$location==3] <- "control"
trees$burn_szn[trees$plot==9 & trees$location==4] <- "growing"

trees$burn_szn[trees$plot==10 & trees$location==1] <- "growing"
trees$burn_szn[trees$plot==10 & trees$location==2] <- "control"
trees$burn_szn[trees$plot==10 & trees$location==3] <- "early"
trees$burn_szn[trees$plot==10 & trees$location==4] <- "late"

trees$burn_szn[trees$plot==11 & trees$location==1] <- "control"
trees$burn_szn[trees$plot==11 & trees$location==2] <- "late"
trees$burn_szn[trees$plot==11 & trees$location==3] <- "early"
trees$burn_szn[trees$plot==11 & trees$location==4] <- "growing"

#write_csv(trees, "data/processed_data/MOTtrees_clean.csv")
# clean trees removing errors, adding treatments, etc
☺
##### #make separate DF for treatments ##########

treatments <- read_csv("data/raw_data/treatments.csv")

treatments_no_location <- treatments[-c(2)] %>% 
  group_by(plot) %>% 
  summarise(thin_lvl = thin_lvl) %>% 
  distinct(.keep_all = T)


# ^correct any errors in species name/location/canopy/dbh
#changed black cherry and sourwood temp to mesophytes
trees <- trees %>% 
  filter(location!="E")
#eliminating edge trees to calculate BA within the plot boundaries
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

pine_ba <- pine_sp %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
          dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(pine_ba_m2ha = sum(ba_m2)/0.1011714,
            pine_ba_ft2a = sum(ba_ft2)/0.25)

ba_x_pine <- merge(plot_basal_area, pine_ba)

ba_x_pine_plot <- ba_x_pine %>%
  group_by(plot) %>% 
  summarise(pct_pine = (pine_ba_m2ha/ba_m2ha)*100)


####### do the same for % pyrophye and mesophyte #########

pyro <- trees %>% 
  filter(functional_group=="pyrophyte")

pyro_ba <- pyro %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(pyro_ba_m2ha = sum(ba_m2)/0.1011714,
            pyro_ba_ft2a = sum(ba_ft2)/0.25)

ba_x_pyro <- merge(plot_basal_area, pyro_ba)

ba_x_pyro_plot <- ba_x_pyro %>%
  group_by(plot) %>% 
  summarise(pct_pyro_ba = (pyro_ba_m2ha/ba_m2ha)*100)

meso <- trees %>% 
  filter(functional_group=="mesophyte")

meso_ba <- meso %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(meso_ba_m2ha = sum(ba_m2)/0.1011714,
            meso_ba_ft2a = sum(ba_ft2)/0.25)

ba_x_meso <- merge(plot_basal_area, meso_ba)

ba_x_meso_plot <- ba_x_meso %>%
  group_by(plot) %>% 
  summarise(pct_meso_ba = (meso_ba_m2ha/ba_m2ha)*100)

pyromeso <- merge(ba_x_pyro_plot, ba_x_meso_plot)

pyro_x_meso <- merge(treatments_no_location, pyromeso) #good

##### making random plots for heather meeting

ggplot(trees, aes(dbh, color = functional_group, fill = functional_group)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80),
                     limits = c(0,55)) +
  ggtitle("All Individuals DBH by Functional Group") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(trees, aes(plot, color = functional_group, fill = functional_group)), +
  geom_histogram(binwidth = 1, center = 1) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11))


ggplot(plot_1, aes(dbh, color = functional_group, fill = functional_group)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80),
                     limits = c(0,55)) +
  ggtitle("Plot 1") +
  theme(plot.title = element_text(hjust = 0.5))



######## start plot by plot analysis for thinning treatments #####
# treatment codes for reference
  # 1 = control
  # 2 = high thin
  # 3 = low thin
  # 4 = med thin
#not counting EDGE individuals for thinning considerations

##### Plot 1 (low)

plot1 <- trees %>% 
  filter(plot==1) %>% 
  filter(location!="E")

plot1_meso <- plot1 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 33% of all stems in 0-12cm dbh
plot1_removestems <- plot1_meso[sample(NROW(plot1_meso), NROW(plot1_meso)*(1-0.33)),]

#these are the stems that were removed
removed_plot1 <- anti_join(plot1_meso, plot1_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot1_thinned <- anti_join(plot1, removed_plot1, by = "stem_id")

#basal area before thinning
ba_plot1_before <- plot1 %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

#basal area after thinning
ba_plot1_after <- plot1_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

ggplot(plot1, aes(dbh, color = functional_group, fill = functional_group)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
  ggtitle("All Individuals DBH by Functional Group") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(plot1_thinned, aes(dbh, color = functional_group, fill = functional_group)) +
  geom_histogram() +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)) +
  ggtitle("All Individuals DBH by Functional Group") +
  theme(plot.title = element_text(hjust = 0.5))

##### Plot 2 (med)

plot2 <- trees %>% 
  filter(plot==2) %>% 
  filter(location!="E")

plot2_meso <- plot2 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 33% of all stems in 0-12cm dbh
plot2_removestems <- plot2_meso[sample(NROW(plot2_meso), NROW(plot2_meso)*(1-0.66)),]

#these are the stems that were removed
removed_plot2 <- anti_join(plot2_meso, plot2_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot2_thinned <- anti_join(plot2, removed_plot2, by = "stem_id")

#basal area before thinning
ba_plot2_before <- plot2 %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

#basal area after thinning
ba_plot2_after <- plot2_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 3 (high)

plot3 <- trees %>% 
  filter(plot==3) %>% 
  filter(location!="E")

plot3_meso <- plot3 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 33% of all stems in 0-12cm dbh
plot3_removestems <- plot3_meso[sample(NROW(plot3_meso), NROW(plot3_meso)*(1-1)),]

#these are the stems that were removed
removed_plot3 <- anti_join(plot3_meso, plot3_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot3_thinned <- anti_join(plot3, removed_plot3, by = "stem_id")

#basal area before thinning
ba_plot3_before <- plot3 %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

#basal area after thinning
ba_plot3_after <- plot3_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 4 (low)
##### Plot 5 (med)
##### Plot 6 (high)
##### Plot 7 (low)
##### Plot 8 (control)
##### Plot 9 (high)
##### Plot 10 (control)
##### Plot 11 (med)


#plan. complete all 11 plot thinning simulations (randomly removing 33%, 66%, or 100% of mesophytes 
      # that are 0-12cm dbh. combine all ex. "plot1_thinned+plot2_thinned+plot3_etc" to make a new
      # df that is trees_thinned.   compare pyro/meso/total BA between "trees" x "trees_thinned"
      # potential figures = low/med/high thinning vs. % change in pyrophyte/mesophyte BA,
      # % change in total BA, % change stem density, total BA, 

