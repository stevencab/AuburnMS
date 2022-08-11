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
  summarise(BA_m2ha = sum(ba_m2)/0.02023428,
            BA_ft2a = sum(ba_ft2)/0.05)

basal_area_m2_ft2_plot_PINE <- basal_area_m2_ft2 %>% 
  filter(Genus == "Pinus") %>% 
  group_by(Plot, Site) %>% 
  summarise(Pine_m2ha = sum(ba_m2)/0.02023428,
            Pine_ft2a = sum(ba_ft2)/0.05)

Trees_for_master <- inner_join(basal_area_m2_ft2_plot, basal_area_m2_ft2_plot_PINE)


test <- Trees_for_master %>% 
  group_by(Plot, Site) %>% 
  summarise(Pine_pctBAft2a = Pine_ft2a/BA_ft2a*100)

Trees_for_master <- inner_join(Trees_for_master, test)


# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pine_ba_fig <- ggplot(Trees_for_master, aes(x=pine_american)) + scale_fill_manual(values =cbPalette) +
  geom_histogram(aes(fill=Site),
                 binwidth = 10,
                 col="black", 
                 size=.1) +
  #scale_x_continuous(breaks=c(0,10,30,50,70,90,100)) +
  labs(title="Pine Basal Area Distribution",
       x="Pine Basal Area (ft2/acre)",
       y="Number of plots") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "figures/pine_ba_fig.png", plot = pine_ba_fig)

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
            Avg_Total = mean(Total_wt)) %>% 
  group_by(Site, Plot) %>% 
  summarise(Pine_pct = (Avg_Pine/Avg_Total)*100,
            UO_pct = (Avg_UO/Avg_Total)*100,
            SG_pct = (Avg_SG/Avg_Total)*100,
            WO_pct = (Avg_WO/Avg_Total)*100,
            Meso_pct = (Avg_Meso/Avg_Total)*100)

fuel_load <- sorted_litter %>% 
  group_by(Site, Plot) %>% 
  summarise(Avg_Pine_load = mean(Pine_wt)*11.11,
            Avg_UO_load = mean(UO_wt)*11.11,
            Avg_SG_load = mean(SG_wt)*11.11,
            Avg_WO_load = mean(WO_wt)*11.11,
            Avg_Meso_load = mean(MESO_wt)*11.11,
            Avg_Total_load = mean(Total_wt)*11.11) 
  
all_litter <- inner_join(x = sorted, y = fuel_load)

write_csv(all_litter, file = "data/processed_data/MixedStand_sorted_litter.csv")



## canpoy cones veg

herbs <- read_csv("data/raw_data/Mixed Stands/MixedStand_CanopyWoodyCoverCones.csv")

summary(herbs3)
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

herbs3$Avg_CC[herbs3$Plot==60] <- 95.84


write_csv(herbs3, file = "data/processed_data/MixedStand_woodyherbcanopy.csv")


canopy_distribution <- ggplot(herbs3, aes(x=Avg_CC)) + scale_fill_manual(values =cbPalette) +
  geom_histogram(aes(fill=Site),
                 binwidth = 3,
                 col="black", 
                 size=.1) +
  scale_x_continuous(breaks=c(75,80,85,90,95,100)) +
  labs(title="Canopy Cover Distribution",
       x="Canopy Coverage (%)",
       y="Number of plots") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "figures/canopy_distribution.png", plot = canopy_distribution)


## slope, duff

slopeduff <- read_csv("data/raw_data/Mixed Stands/MixedMaster.csv")

summary(slopeduff
)

slopeduff <- slopeduff %>% 
  group_by(Site, Plot, Aspect) %>% 
  summarise(Slope = Slope,
            Avg_Depth_cm = as.numeric(de1 + de2)/2,
            Avg_Duff_cm = as.numeric(du1 + du2)/2)

### woody debris 

fine_woody <- read_csv("data/raw_data/Mixed Stands/MixedStand_FWD.csv")


fine <- fine_woody %>% 
  group_by(Site, Plot) %>% 
  summarise(Pieces_1hr =  sum(Woody_0.25),
            Pieces_10hr = sum(Woody_1),
            Pieces_100hr = sum(Woody_3))

calcs1 <- fine %>% 
  group_by(Site, Plot) %>% 
  summarise(Biomass_TA_1hr = (11.64*Pieces_1hr*0.00151*0.48*1.13)/18,
            Biomass_TA_10hr = (11.64*Pieces_10hr*0.289*0.48*1.13)/18,
            Biomass_TA_100hr = (11.64*Pieces_100hr*2.76*0.40*1.13)/36)

FWD <- calcs1 %>% 
  group_by(Site,Plot) %>% 
  summarise(FWD_Mgha = ((sum(Biomass_TA_1hr,Biomass_TA_10hr,Biomass_TA_100hr)*2242)/1000))

hist(log(FWD$FWD_Mgha))

coarse_woody <- read_csv("data/raw_data/Mixed Stands/MixedStand_CWD.csv")


coarse <- coarse_woody %>% 
  group_by(Site, Plot, Direction) %>% 
  summarise(Transect_biomass =  sum(biomass))

CWD <- coarse %>% 
  group_by(Site, Plot) %>% 
  summarise(CWD_Mgha = mean(Transect_biomass)*2242/1000)

hist(CWD$CWD_Mgha)

Woody_Debirs <- inner_join(FWD,CWD)
write_csv(Woody_Debirs, file = "data/processed_data/MixedStand_Debris.csv")

### make everything

lastone <- inner_join(x = herbs3, y = slopeduff)

master_notrees <- inner_join(x = lastone, y = all_litter)

Master <- inner_join(x = master_notrees, y = Trees_for_master)
#View(Master)
write_csv(Master, file = "data/processed_data/MixedStand_Master.csv")


### end all processing for master, use above master file for all analysis minus trees


### fuel load playing

fuel_load <- sorted_litter %>% 
  group_by(Site, Plot, Location) %>% 
  summarise(Pine_load = Pine_wt*11.11,
            UO_load = UO_wt*11.11,
            SG_load = SG_wt*11.11,
            WO_load = WO_wt*11.11,
            Meso_load = MESO_wt*11.11,
            Total_load = sum(Pine_wt,UO_wt,SG_wt,WO_wt,MESO_wt)*11.11)
    
load_pct <- fuel_load %>% 
  group_by(Site, Plot) %>% 
  summarise(Pine_load = mean(Pine_load),
            UO_load = mean(UO_load),
            SG_load = mean(SG_load),
            WO_load = mean(WO_load),
            Meso_load = mean(Meso_load),
            Total_load = mean(Total_load)) %>% 
  group_by(Site,Plot) %>% 
  summarise(Pine_load_pct = Pine_load/Total_load*100,
            UO_load_pct = UO_load/Total_load*100,
            SG_load_pct = SG_load/Total_load*100,
            WO_load_pct = WO_load/Total_load*100,
            Meso_load_pct = Meso_load/Total_load*100
            )

write_csv(load_pct, file = "data/processed_data/load_pct.csv")
  
aggregate(Meso_load_pct  ~ Site,
          data = load_pct,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2))
  
    
    Avg_Pine = mean(Pine_wt),
            Avg_UO = mean(UO_wt),
            Avg_SG = mean(SG_wt),
            Avg_WO = mean(WO_wt),
            Avg_Meso = mean(MESO_wt),
            Avg_Total = mean(Total_wt))


%>% 
  group_by(Site, Plot) %>% 
  summarise(Pine_pct = (Avg_Pine/Avg_Total)*100,
            UO_pct = (Avg_UO/Avg_Total)*100,
            SG_pct = (Avg_SG/Avg_Total)*100,
            WO_pct = (Avg_WO/Avg_Total)*100,
            Meso_pct = (Avg_Meso/Avg_Total)*100)

summary(mixed_trees)

mixed_trees <- mixed_trees %>% 
  filter(Species %in% c("ACRU","LITU","QUNI","LIST","QUAL","QUFA", "QURU", "PITA","PIEL","PIPA"))

ggplot(mixed_trees, aes(Species))+
  geom_bar() + 
  facet_wrap(~Site)

fine_woody <- read_csv("data/raw_data/Mixed Stands/MixedStand_FWD.csv")


fine <- fine_woody %>% 
  group_by(Site, Plot) %>% 
  summarise(Pieces_1hr =  sum(Woody_0.25),
            Pieces_10hr = sum(Woody_1),
            Pieces_100hr = sum(Woody_3))

calcs1 <- fine %>% 
  group_by(Site, Plot) %>% 
  summarise(Biomass_TA_1hr = (11.64*Pieces_1hr*0.00151*0.48*1.13)/18,
            Biomass_TA_10hr = (11.64*Pieces_10hr*0.289*0.48*1.13)/18,
            Biomass_TA_100hr = (11.64*Pieces_100hr*2.76*0.40*1.13)/36)

FWD <- calcs1 %>% 
  group_by(Site,Plot) %>% 
  summarise(FWD_Mgha = ((sum(Biomass_TA_1hr,Biomass_TA_10hr,Biomass_TA_100hr)*2242)/1000))

hist(log(FWD$FWD_Mgha))

coarse_woody <- read_csv("data/raw_data/Mixed Stands/MixedStand_CWD.csv")


coarse <- coarse_woody %>% 
  group_by(Site, Plot, Direction) %>% 
  summarise(Transect_biomass =  sum(biomass))

CWD <- coarse %>% 
  group_by(Site, Plot) %>% 
  summarise(CWD_Mgha = mean(Transect_biomass)*2242/1000)

hist(CWD$CWD_Mgha)

Woody_Debirs <- inner_join(FWD,CWD)
