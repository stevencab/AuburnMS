# scripts for beginning midstory removal analysis
#things to do
#all leaf litter sorted analysis, box plots for each collection 
#sort leaf litter for comp at time of fire
#tree overstory composition
#burn data differences between season, os treat, and thinning
#interaction between all 3

library(ggplot2)
library(readr)
library(dplyr)
library(cowplot)
library(ggsignif)

litter <- read_csv("C:/Users/steve/Desktop/AuburnMS/data/raw_data/Midstory Removal/most important/LitterSortingALL_MSR.csv")

litter$canopy_trt <- factor(litter$canopy_trt, levels = c("low","med","high"))
litter$collection <- factor(litter$collection, levels = c("6/24/2021","1/17/2022","4/12/2022","6/24/2022"))


boxplot(data = litter, MESO_wt~collection)

pct_litter <- litter %>% 
  group_by(collection, Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(pine_pct = (Pine_wt/Total_m*100),
            uo_pct = (UO_wt/Total_m*100),
            sg_pct = (SG_wt/Total_m*100),
            wo_pct = (WO_wt/Total_m*100),
            meso_pct = (MESO_wt/Total_m*100))

edlitter <- pct_litter %>% 
  filter(Burn_Szn %in% c("ED")) %>% 
  group_by(collection, Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(pine = mean(pine_pct),
            uo = mean(uo_pct),
            sg = mean(sg_pct),
            wo = mean(wo_pct),
            meso = mean(meso_pct))
ldlitter <- pct_litter %>% 
  filter(Burn_Szn %in% c("LD")) %>% 
  group_by(collection, Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(pine = mean(pine_pct),
            uo = mean(uo_pct),
            sg = mean(sg_pct),
            wo = mean(wo_pct),
            meso = mean(meso_pct))

edl <- edlitter %>% 
  filter(collection=="1/17/2022")
ldl <- ldlitter %>% 
  filter(collection=="4/12/2022")

edlldl <- rbind(edl,ldl)

ggplot(pct_litter, aes(x = Treatment), color = Treatment) +
  geom_boxplot(aes(y=meso_pct)) +
  facet_wrap(~collection+canopy_trt)

#write_csv(pct_litter, "data/processed_data/Midstory Removal/longlitter.csv")
#fire stuff

burn <- read_csv("data/raw_data/Midstory Removal/most important/MOTburndataforR_ALLSEASONS.csv")
head(burn)


burn$meso_os <- factor(burn$canopy_trt, levels = c("low","med","high"))
burn$season <- factor(burn$season, levels = c("6/24/2021","1/17/2022","4/12/2022","6/24/2022"))

ggplot(burn, aes(x = Treatment)) +
  geom_boxplot(aes(y=fuel_moist, color = meso_os)) +
  facet_wrap(~season)

#combining fire season with specific litter collection
edlldl <- edlldl[-c(1)]
burnxlitter <- left_join(edlldl,burn)
write_csv(burnxlitter, "data/processed_data/midstory removal/litterxEDLD.csv")

ggplot(burnxlitter, aes(x = pine)) +
  geom_smooth(aes(y = avg_fh, color = canopy_trt), method = "lm") +
  facet_wrap(~Treatment)

res <- lm(data = typeburn, avg_max_temp~percent+(Treatment)+(Burn_Szn)+(canopy_trt)+
            (Burn_Szn*canopy_trt)+(Burn_Szn+Treatment)+(Treatment*canopy_trt))
summary(res)
res <- lm(data = typeburn, avg_max_temp~percent+(Treatment)+(Burn_Szn)+(canopy_trt)+
            (Burn_Szn*canopy_trt)+(Burn_Szn+Treatment)+(Treatment*canopy_trt))


typeburn <- read_csv("data/processed_data/midstory removal/litterbytypexEDLD.csv")
typeburn$canopy_trt <- factor(typeburn$canopy_trt, levels = c("low","med","high"))

ggplot(typeburn, aes(x = percent, y = frs1_cms, color = litter_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point()+
  facet_wrap(~Treatment+canopy_trt)

ggplot(typeburn, aes(x = Treatment, y = percent, color = litter_type)) +
  geom_boxplot() +
  facet_wrap(~canopy_trt+Burn_Szn)



litterlong <- read_csv("data/processed_data/midstory removal/longlitter.csv")

litterlong$canopy_trt <- factor(litterlong$canopy_trt, levels = c("low","med","high"))
litterlong$litter_type <- factor(litterlong$litter_type, levels = c("pine","uo","sg","wo","meso"))
litterlong$collection <- factor(litterlong$collection, levels = c("6/24/2021","1/17/2022","4/12/2022","6/24/2022"))

ggplot(litterlong, aes(x = Treatment, y = litter_pct, color = litter_type)) +
  geom_boxplot() +
  facet_wrap(~canopy_trt)

ggplot(litterlong)

littertest <- litterlong %>% 
  group_by(collection, Plot, Treatment, canopy_trt, litter_type) %>% 
  summarise(plot_litter = mean(litter_pct))

ggplot(littertest, aes(x = Treatment, y = plot_litter, color = litter_type)) +
  geom_boxplot() +
  facet_wrap(~collection+canopy_trt)
