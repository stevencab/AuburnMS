library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)

mixed_trees <- read_csv("data/raw_data/Mixed Stands/MixedStand_Trees.csv")



dbh_cm_in <- mixed_trees %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(dbh_cm = DBH,
            dbh_in = (DBH)*0.393701)

basal_area_m2_ft2 <- dbh_cm_in %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(ba_m2 = dbh_cm^2*(0.00007854),
            ba_ft2 = dbh_in^2*(0.005454))

basal_area_m2_ft2_plot <- basal_area_m2_ft2 %>% 
  group_by(Plot,Site) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.02023428,
            ba_ft2a = sum(ba_ft2)/0.05)

basal_area_m2_ft2_plot_PINE <- basal_area_m2_ft2 %>% 
  filter(Genus == "Pinus") %>% 
  group_by(Plot, Site) %>% 
  summarise(pine_metric = sum(ba_m2)/0.02023428,
            pine_american = sum(ba_ft2)/0.05)

Trees_for_master <- inner_join(basal_area_m2_ft2_plot, basal_area_m2_ft2_plot_PINE)

test <- test %>% 
  group_by(Plot, Site) %>% 
  summarise(percent = pine_american/ba_ft2a*100)

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

percent_pine_fig <- ggplot(test, aes(x=percent)) + scale_fill_manual(values =cbPalette) +
  geom_histogram(aes(fill=Site),
                 binwidth = 10,
                 col="black", 
                 size=.1) +
  scale_x_continuous(breaks=c(0,10,30,50,70,90,100)) +
  labs(title="Percent Pine Basal Area Distribution",
       x="Pine Basal Area (%)",
       y="Number of plots") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "figures/percent_pine_ba.png", plot = percent_pine_fig)

n_distinct(test$Plot & Site=="Tuskegee")

cr <- test %>% 
  filter(Site=="Tuskegee")


total_basal <- ggplot(basal_area_m2_ft2_plot, aes(x=ba_ft2a)) + scale_fill_manual(values =cbPalette) +
  geom_histogram(aes(fill=Site),
                 binwidth = 15,
                 col="black", 
                 size=.1) +
  scale_x_continuous(breaks=c(0,50,100,150,200,250,300,350)) +
  labs(title="Basal Area Distribution",
       x="Total Basal Area (ft2/acre)",
       y="Number of plots") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "figures/total_basal.png", plot = total_basal)


summary(basal_area_m2_ft2_plot)

## SORTED LITTER PLAYING


library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)

sorted_litter <- read_csv("data/raw_data/Mixed Stands/MixedStand_LitterSortingALL.csv")

sorted <- sorted_litter %>% 
  group_by(Site, Plot) %>% 
  summarise(Avg_Pine = mean(Pine_wt),
            Avg_UO = mean(UO_wt),
            Avg_SG = mean(SG_wt),
            Avg_WO = mean(WO_wt),
            Avg_Meso = mean(MESO_wt),
            Avg_Total = mean(Total_wt))

write_csv(sorted, file = "data/processed_data/MixedStand_sorted_litter.csv")
unique(sorted$Plot)

## canpoy cones veg

herbs <- read_csv("data/raw_data/Mixed Stands/MixedStand_CanopyWoodyCoverCones.csv")

summary(herbs)
herbs$X20 <- NULL
herbs[is.na(herbs)] <- 0

herbs1 <- herbs %>% 
  group_by(Site, Plot) %>% 
  summarise(Avg_CC = mean(CC))


#without Center CC 



herbs2 <- herbs %>% 
  group_by(Site, Plot) %>% 
  summarise(Avg_Woody_Live = mean(as.numeric(Live_Woody), na.rm = T),
            Avg_Woody_Dead = mean(as.numeric(Dead_Woody), na.rm = T),
            Avg_Woody_Ht = mean(as.numeric(Woody_Height), na.rm = T),
            Avg_Herb_Live = mean(as.numeric(Live_Herbs), na.rm = T),
            Avg_Herb_Dead = mean(as.numeric(Dead_Herbs), na.rm = T),
            Avg_Herb_Ht = mean(as.numeric(Herb_Height), na.rm = T),
            Avg_Cones = mean(as.numeric(Cones), na.rm = T),
            Avg_Acorns = mean(as.numeric(Acorns), na.rm = T),
            Avg_Balls = mean(as.numeric(SG), na.rm = T))

herbs3 <- inner_join(x = herbs1, y = herbs2)
summary(herbs3)     

write_csv(herbs3, file = "data/processed_data/MixedStand_woodyherbcanopy.csv")

## slope, duff

slopeduff <- read_csv("data/raw_data/Mixed Stands/MixedMaster.csv")

summary(slopeduff
)

slopeduff <- slopeduff %>% 
  group_by(Site, Plot, Aspect) %>% 
  summarise(Slope = Slope,
            Avg_Depth_cm = as.numeric(de1 + de2)/2,
            Avg_Duff_cm = as.numeric(du1 + du2)/2)

lastone <- inner_join(x = herbs3, y = slopeduff)

master_notrees <- inner_join(x = lastone, y = sorted)
Master <- inner_join(x = master_notrees, y = Trees_for_master)
#View(Master)
write_csv(Master, file = "data/processed_data/MixedStand_Master.csv")


### end all processing for master, use above master file for all analysis minus trees