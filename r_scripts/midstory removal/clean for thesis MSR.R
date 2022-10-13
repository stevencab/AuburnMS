####thesis final midstory analysis for pub 

library(ggplot2)
library(readr)
library(dplyr)
library(cowplot)
library(ggsignif)
library(factoextra)
library(nlme)
library(ggpubr)
library(emmeans)

#datasets
fli
litter
litterlong
burn
typeburn
canopy
understory <- read_csv("data/raw_data/Midstory Removal/Fuels_UnderstoryLitterDuff.csv")
FWD_data <- read_csv("data/raw_data/Midstory Removal/Fuels_FWD.csv")
CWD_data <- read_csv("data/raw_data/Midstory Removal/Fuels_CWD.csv")
FWD_data <- FWD_data[,-7]


Master

herbcalcs <- understory %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_Herb_kgm2 = (Herb_Ht/100)*(Herb_Cover+Grass_Cover/100)*0.8,
            Biomass_Shrub_kgm2 = (Woody_Ht/100)*(Woody_Cover/100)*1.8)

herbcalcsplot <-  herbcalcs %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_Herb = mean(Biomass_Herb_kgm2)*1000,
            Biomass_Shrub = mean(Biomass_Shrub_kgm2)*1000)


litterduf <- understory %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_Litter_kgm2 = (Litter/100)*44.051,
            Biomass_Duff_kgm2 = (Duff/100)*88.102)
litterduff <- litterduf %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_Litter_kgm2 = mean(Biomass_Litter_kgm2)*1000,
            Biomass_Duff_kgm2 = mean(Biomass_Duff_kgm2)*1000)

FWD <- FWD_data %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Pieces_1hr =  sum(woody_25),
            Pieces_10hr = sum(woody_1),
            Pieces_100hr = sum(woody_3))

calcs1 <- FWD %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_TA_1hr = (11.64*Pieces_1hr*0.00151*0.48*1.13)/36,
            Biomass_TA_10hr = (11.64*Pieces_10hr*0.289*0.48*1.13)/36,
            Biomass_TA_100hr = (11.64*Pieces_100hr*2.76*0.40*1.13)/72)

FWD <- calcs1 %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(FWD_Mgha = ((sum(Biomass_TA_1hr,Biomass_TA_10hr,Biomass_TA_100hr)*224.2)))

coarse <- CWD_data %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Transect_biomass =  sum(biomass))

CWD <- coarse %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(CWD_Mgha = mean(Transect_biomass)*224.2)

#join all fuelloads -- they are all in g/m2
herbcalcsplot
litterduff
FWD
CWD
join1 <- left_join(herbcalcsplot,litterduff)
join2 <- left_join(join1,FWD)
MasterFuelLoadsgm2_MSR <- left_join(join2,CWD)

#make figure for fuel loads
#write_csv(MasterFuelLoadsgm2_MSR, "data/processed_data/midstory removal/TotalFuelLoads.csv")

allloads <- read_csv("data/processed_data/midstory removal/TotalFuelLoads_Long.csv")

allloads <- allloads %>% 
  group_by(Collection, Treatment, Source) %>% 
  summarise(Biomass_gm2 = sum(Biomass_gm2)/100)

usloadsgroup <- usloads %>% 
  group_by(group, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2))
cbbPalette <- c("#FF0000", "#FF9999", "#FFCC99", "#CC99FF", "#0033FF", "#999999")

usloadssite <- usloads %>% 
  group_by(Site, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2)/100)

allloadsfig <- ggplot(allloads, aes(x = Treatment, y = Biomass_gm2))+
  geom_bar(position = "stack", stat = "identity", aes(fill = Source), size = 0.6,color = "black") +
  labs(y = ~Fuel~ ~load~ Mg/ha) +
  xlab("Treatment") +
  guides(fill=guide_legend(title="Fuel load type")) + 
  facet_wrap(~Collection)


allloads$Source <- factor(allloads$Source, levels = c("Litter", "Duff", "Herbaceous", "Shrub", "FWD", "CWD"))
allloads$Collection <- factor(allloads$Collection, levels = c("Pre-thin","Post-thin"))

ggplot(allloads, aes(x = Treatment, y = Biomass_gm2, fill = Source)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~Collection)
test <- allloadsfig + scale_fill_grey(start = 0.2, end = .85) 
test + theme_bw() 

cwdonly <- allloads %>% 
  filter(Source=="Duff")
teststat <- lme(data = cwdonly, Biomass_gm2~Treatment*Collection, random = ~1|Plot/Treatment)
summary(teststat)

#ggtitle("Biomass of fuel sources across Pine, Mixed, and Hardwood Forests")
ggsave(plot = allloads, "figures/biomass_allfuelsources.png", height = 6, width = 9)
filter(usloads$group=="< 30% Pine")







emmeans(res_frs1, pairwise ~ Burn_Szn)


trees

log_loads <- read_csv("data/processed_data/midstory removal/TotalFuelLoads_log.csv")

#anovas for change in fuel loads

herbtest <- lme(data=log_loads, Biomass_Herb~Collection+Treatment*meso_os,random = ~1|Plot/Treatment)
summary(herbtest)

shrubtest <- lme(data=log_loads, Biomass_Shrub~Collection+Treatment*meso_os,random = ~1|Plot/Treatment)
summary(shrubtest)

cwdtest <- lme(data=log_loads, CWD_Mgha~Collection+Treatment*meso_os,random = ~1|Plot/Treatment)
summary(cwdtest)

fwdtest <- lme(data=log_loads, FWD_Mgha~Collection+Treatment*meso_os,random = ~1|Plot/Treatment)
summary(fwdtest)

littest <- lme(data=log_loads, Biomass_Litter_kgm2~Collection+Treatment*meso_os,random = ~1|Plot/Treatment)
summary(littest)

duftest <- lme(data=log_loads, Biomass_Duff_kgm2~Collection+Treatment*meso_os,random = ~1|Plot/Treatment)
summary(duftest)

#no sig changes in fuel load between any parameter 
#in pre/post treatment, thin, or canopy gradient

#how about total fuel load between pre thin and post thin 

totals <- log_loads %>% 
  group_by(Collection, Plot, Treatment, meso_os) %>% 
  summarise(total_bio = sum(Biomass_Duff_kgm2+Biomass_Herb+Biomass_Litter_kgm2+Biomass_Shrub+CWD_Mgha+FWD_Mgha)) %>% 
  filter(Collection=="post-thin")
 
totaltest <- lme(data=totals, total_bio~Treatment,random = ~1|Plot/Treatment)
summary(totaltest)



#start testing changes in leaf litter comp

litter
combinedmeso <- litter %>% 
  group_by(collection,Plot,Treatment,Burn_Szn,canopy_trt) %>% 
  summarise(Pine_wt = Pine_wt,
            UO_wt = UO_wt,
            Other_wt = sum(SG_wt,WO_wt,MESO_wt),
            Total_wt = Total_m)
litterpct <- combinedmeso %>% 
  group_by(collection,Plot,Treatment,Burn_Szn,canopy_trt) %>% 
  summarise(Pine = Pine_wt/Total_wt*100,
            UO = UO_wt/Total_wt*100,
            Other = Other_wt/Total_wt*100) %>% 
  filter(collection=="6/24/2022")

litmodels <- lme(data=litterpct, UO~canopy_trt, random = ~1|Plot/canopy_trt/Treatment/collection)
summary(litmodels)

emmeans(litmodels, pairwise ~ collection*Treatment)


litterlong <- litterlong %>% 
  filter(litter_type %in% c("sg","wo","meso")) %>% 
  group_by(collection,Plot,Treatment,Burn_Szn,canopy_trt) %>% 
  summarise(litter_pct = sum(litter_pct)) 
  

ggplot(compchange, aes(x = Treatment, y = litter_pct, fill = litter_type)) +
  geom_boxplot() +
  facet_wrap(~collection) + 
  theme_bw()

testy <- litter %>% 
  filter(collection %in% c("6/24/2021","6/24/2022"))
boxplot(data=testy, Load_gm2~Treatment+collection)


##burn data #typeburn

fli_litterpct
#big four fire params

#fli
res_fli <- lme(data = fli_litterpct, fli~Treatment*Burn_Szn, random = ~1|Plot/canopy_trt/Treatment)
summary(res_fli)
intervals(res_fli)
#frs1
res_frs1 <- lme(data = fli_litterpct, frs1~Treatment+Burn_Szn, random = ~1|Plot/Treatment/Burn_Szn)
summary(res_frs1)
intervals(res_frs1)
#consump
res_consump <- lme(data = fli_litterpct, consump~Treatment+Burn_Szn, random = ~1|Plot/Treatment/Burn_Szn)
summary(res_consump)
intervals(res_consump)
#avg_max_temp
res_temp <- lme(data = fli_litterpct, avg_max_temp~Treatment+Burn_Szn, random = ~1|Plot/Treatment/Burn_Szn)
summary(res_temp)
intervals(res_temp)

#relevel for meso os and burn szn
fli_litterpct$meso_os <- relevel(fli$meso_os, ref = "med")
fli_litterpct$Burn_Szn <- relevel(fli$Burn_Szn, ref = "ED")

#litter pct pine etc
fli_litterpct

fli_pine <- lm(data = fli_litterpct, consump~pine)
summary(fli_pine)
intervals(fli_pine)

plot(res_fli, 2)
car::leveneTest(fli~Treatment*meso_os*Burn_Szn, data = fli)
aov_res <- residuals(object = res_fli)
shapiro.test(x = aov_res)

ggline(fli, x = "Burn_Szn", y = "consump", color = "Treatment",
       add = c("mean_se", "dotplot"))


fli_litterpcttest <- fli_litterpct %>% 
  group_by(Burn_Szn) %>% 
  summarise(meanconsump = mean(consump))
