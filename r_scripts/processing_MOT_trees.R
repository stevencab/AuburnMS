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

write_csv(trees, "data/processed_data/MOTtrees_clean.csv")
# clean trees removing errors, adding treatments, etc

# read in new, cleaned treees

trees <- read_csv("data/processed_data/MOTtrees_clean.csv")
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
  summarise(before_ba_m2ha = sum(ba_m2)/0.1011714,
            before_ba_ft2a = sum(ba_ft2)/0.25)

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

##### Plot 1 (low) #####

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

##### Plot 2 (med) #####

plot2 <- trees %>% 
  filter(plot==2) %>% 
  filter(location!="E")

plot2_meso <- plot2 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 66% of all stems in 0-12cm dbh
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

##### Plot 3 (high) #####

plot3 <- trees %>% 
  filter(plot==3) %>% 
  filter(location!="E")

plot3_meso <- plot3 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 100% of all stems in 0-12cm dbh
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

##### Plot 4 (low) #####

plot4 <- trees %>% 
  filter(plot==4) %>% 
  filter(location!="E")

plot4_meso <- plot4 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 33% of all stems in 0-12cm dbh
plot4_removestems <- plot4_meso[sample(NROW(plot4_meso), NROW(plot4_meso)*(1-0.33)),]

#these are the stems that were removed
removed_plot4 <- anti_join(plot4_meso, plot4_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot4_thinned <- anti_join(plot4, removed_plot4, by = "stem_id")

#basal area before thinning
ba_plot4_before <- plot4 %>% 
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
ba_plot4_after <- plot4_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 5 (med) #####

plot5 <- trees %>% 
  filter(plot==5) %>% 
  filter(location!="E")

plot5_meso <- plot5 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 66% of all stems in 0-12cm dbh
plot5_removestems <- plot5_meso[sample(NROW(plot5_meso), NROW(plot5_meso)*(1-0.66)),]

#these are the stems that were removed
removed_plot5 <- anti_join(plot5_meso, plot5_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot5_thinned <- anti_join(plot5, removed_plot5, by = "stem_id")

#basal area before thinning
ba_plot5_before <- plot5 %>% 
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
ba_plot5_after <- plot5_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 6 (high) #####

plot6<- trees %>% 
  filter(plot==6) %>% 
  filter(location!="E")

plot6_meso <- plot6 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 100% of all stems in 0-12cm dbh
plot6_removestems <- plot6_meso[sample(NROW(plot6_meso), NROW(plot6_meso)*(1-1)),]

#these are the stems that were removed
removed_plot6 <- anti_join(plot6_meso, plot6_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot6_thinned <- anti_join(plot6, removed_plot6, by = "stem_id")

#basal area before thinning
ba_plot6_before <- plot6 %>% 
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
ba_plot6_after <- plot6_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 7 (low) #####

plot7 <- trees %>% 
  filter(plot==7) %>% 
  filter(location!="E")

plot7_meso <- plot7 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 33% of all stems in 0-12cm dbh
plot7_removestems <- plot7_meso[sample(NROW(plot7_meso), NROW(plot7_meso)*(1-0.33)),]

#these are the stems that were removed
removed_plot7 <- anti_join(plot7_meso, plot7_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot7_thinned <- anti_join(plot7, removed_plot7, by = "stem_id")

#basal area before thinning
ba_plot7_before <- plot7 %>% 
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
ba_plot7_after <- plot7_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 8 (control) #####

plot8 <- trees %>% 
  filter(plot==8) %>% 
  filter(location!="E")

plot8_meso <- plot8 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 0% of all stems in 0-12cm dbh
plot8_removestems <- plot8_meso[sample(NROW(plot8_meso), NROW(plot8_meso)*(1-0)),]

#these are the stems that were removed
removed_plot8 <- anti_join(plot8_meso, plot8_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot8_thinned <- anti_join(plot8, removed_plot8, by = "stem_id")

#basal area before thinning
ba_plot8_before <- plot8 %>% 
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
ba_plot8_after <- plot8_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 9 (high) #####

plot9 <- trees %>% 
  filter(plot==9) %>% 
  filter(location!="E")

plot9_meso <- plot9 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 100% of all stems in 0-12cm dbh
plot9_removestems <- plot9_meso[sample(NROW(plot9_meso), NROW(plot9_meso)*(1-1)),]

#these are the stems that were removed
removed_plot9 <- anti_join(plot9_meso, plot9_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot9_thinned <- anti_join(plot9, removed_plot9, by = "stem_id")

#basal area before thinning
ba_plot9_before <- plot9 %>% 
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
ba_plot9_after <- plot9_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 10 (control) #####

plot10 <- trees %>% 
  filter(plot==10) %>% 
  filter(location!="E")

plot10_meso <- plot10 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 0% of all stems in 0-12cm dbh
plot10_removestems <- plot10_meso[sample(NROW(plot10_meso), NROW(plot10_meso)*(1-0)),]

#these are the stems that were removed
removed_plot10 <- anti_join(plot10_meso, plot10_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot10_thinned <- anti_join(plot10, removed_plot10, by = "stem_id")

#basal area before thinning
ba_plot10_before <- plot10 %>% 
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
ba_plot10_after <- plot10_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

##### Plot 11 (med) #####

plot11 <- trees %>% 
  filter(plot==11) %>% 
  filter(location!="E")

plot11_meso <- plot11 %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(between(dbh,0,12))

#randomly removes 66% of all stems in 0-12cm dbh
plot11_removestems <- plot11_meso[sample(NROW(plot11_meso), NROW(plot11_meso)*(1-0.66)),]

#these are the stems that were removed
removed_plot11 <- anti_join(plot11_meso, plot11_removestems, by = "stem_id")

#match with those original plot 1 stems, antijoin removes the thinned stems
plot11_thinned <- anti_join(plot11, removed_plot11, by = "stem_id")

#basal area before thinning
ba_plot11_before <- plot11 %>% 
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
ba_plot11_after <- plot11_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.1011714,
            ba_ft2a = sum(ba_ft2)/0.25)

#plan. complete all 11 plot thinning simulations (randomly removing 33%, 66%, or 100% of mesophytes 
      # that are 0-12cm dbh. combine all ex. "plot1_thinned+plot2_thinned+plot3_etc" to make a new
      # df that is trees_thinned.   compare pyro/meso/total BA between "trees" x "trees_thinned"
      # potential figures = low/med/high thinning vs. % change in pyrophyte/mesophyte BA,
      # % change in total BA, % change stem density, total BA, 

###### combine all "plotX_thinned to make "trees_thinned"

trees_thinned <- list(plot1_thinned,
     plot2_thinned,
     plot3_thinned,
     plot4_thinned,
     plot5_thinned,
     plot6_thinned,
     plot7_thinned,
     plot8_thinned,
     plot9_thinned,
     plot10_thinned,
     plot11_thinned) %>%  
  reduce(full_join)

# calculate BA for trees_thinned

thinned_ba <- trees_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(after_ba_m2ha = sum(ba_m2)/0.1011714,
            after_ba_ft2a = sum(ba_ft2)/0.25)

before_x_after_ba <- merge(plot_basal_area, thinned_ba)

percent_ba_change <- before_x_after_ba %>% 
  group_by(plot) %>% 
  summarise(change_ba = ((1-(after_ba_ft2a/before_ba_ft2a))*100))

trt_change <- merge(percent_ba_change, treatments_no_location)
treatments
treatments_no_location
trees_thinned
trees

#test compared to meso BA

meso_thinned <- trees_thinned %>% 
  filter(functional_group=="mesophyte")

meso_ba
meso_ba_thinned <- meso_thinned %>% 
  group_by(stem_id, plot, species, functional_group) %>% 
  summarise(dbh_cm = dbh,
            dbh_in = (dbh)*0.393701) %>% 
  summarise(ba_m2 = ((pi)*(dbh_cm/2)^2)/(10000),
            ba_ft2 = ((pi)*(dbh_in/2)^2)/(144)) %>% 
  ungroup(.) %>% 
  group_by(plot) %>% 
  summarise(after_ba_m2ha = sum(ba_m2)/0.1011714,
            after_ba_ft2a = sum(ba_ft2)/0.25)

meso_before_x_after_ba <- merge(meso_ba_thinned, meso_ba)

meso_percent_ba_change <- meso_before_x_after_ba %>% 
  group_by(plot) %>% 
  summarise(change_ba = ((1-(after_ba_ft2a/meso_ba_ft2a))*100))
meso_trt_change <- merge(meso_percent_ba_change, treatments_no_location)

# current low/med/high method likely not possible without increasing DBH classes. try at home 6/9
