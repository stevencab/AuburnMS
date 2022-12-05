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
library(car)

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
  summarise(Biomass_Herb = mean(Biomass_Herb_kgm2)*10,
            Biomass_Shrub = mean(Biomass_Shrub_kgm2)*10)


litterduf <- understory %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_Litter_kgm2 = (Litter/100)*44.051,
            Biomass_Duff_kgm2 = (Duff/100)*88.102)
litterduff <- litterduf %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Biomass_Litter_kgm2 = mean(Biomass_Litter_kgm2)*10,
            Biomass_Duff_kgm2 = mean(Biomass_Duff_kgm2)*10)

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
  summarise(FWD_Mgha = (sum(Biomass_TA_1hr,Biomass_TA_10hr,Biomass_TA_100hr)*2.242))

coarse <- CWD_data %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(Transect_biomass =  sum(biomass))

CWD <- coarse %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(CWD_Mgha = mean(Transect_biomass)*2.242)

#join all fuelloads -- they are all in g/m2
herbcalcsplot
litterduff
FWD
CWD
join1 <- left_join(herbcalcsplot,litterduff)
join2 <- left_join(join1,FWD)
MasterFuelLoadsMgha_MSR <- left_join(join2,CWD)

write_csv(MasterFuelLoadsMgha_MSR,"data/processed_data/midstory removal/FinalTotalLoads_kgha.csv")

allloads2 <- MasterFuelLoadsMgha_MSR %>% 
  group_by(Collection, Plot, Treatment) %>% 
  summarise(total = sum(Biomass_Duff_kgm2,Biomass_Herb, Biomass_Shrub, Biomass_Litter_kgm2,
                        FWD_Mgha, CWD_Mgha),
            avg = mean(Biomass_Duff_kgm2,Biomass_Herb, Biomass_Shrub, Biomass_Litter_kgm2,
                       FWD_Mgha, CWD_Mgha))
#make figure for fuel loads
#write_csv(MasterFuelLoadsMgha_MSR, "data/processed_data/midstory removal/TotalFuelLoads_Mgha.csv")

allloads <- read_csv("data/processed_data/midstory removal/TotalFuelLoads_Mgha_long2.csv") 
allloads$Load_type <- factor(allloads$Load_type, 
                                      levels = c("FWD","CWD", "Herbaceous", "Shrubs","Duff", "Litter"))
allloads$Collection   <- factor(allloads$Collection  , 
                             levels = c("Pre-Thin", "Post-Thin"))

#^this is for figure making!!!!

allloads <- allloads %>% 
  group_by(Collection, Treatment, Load_type) %>% 
  summarise(Total_load = mean(Load_Mgha))

usloadsgroup <- usloads %>% 
  group_by(group, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2))
cbbPalette <- c("#FF0000", "#FF9999", "#FFCC99", "#CC99FF", "#0033FF", "#999999")

usloadssite <- usloads %>% 
  group_by(Site, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2)/100)

g1 <- ggplot(allloads, aes(x = Treatment, y = Total_load))+
  geom_bar(position = "stack", stat = "identity", aes(fill = Load_type), size = 0.6,color = "black") +
  theme_bw() +
  labs(y = ~Fuel~ ~load~ Mg/ha) +
  labs(x="Treatment",
       y=expression(paste("Mean fuel load  Mg ha"^~-1)),
       fill="Fuel load type") +
  scale_fill_grey(start = 0.35, end = 0.9) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  facet_wrap(~Collection)

legendg1 <- get_legend(
  
  g1 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
             legend.title=element_text(size=12), 
             legend.text=element_text(size=12)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

g2 <- plot_grid(g1, legendg1, ncol = 1, rel_heights = c(1, .1))


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


littermsr <- read_csv("data/processed_data/midstory removal/longlitter.csv") %>% 
  filter(litter_type!=c("pine")) %>% 
  filter(litter_type!=c("uo")) %>% 
  group_by(collection, Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(litter_pct = sum(litter_pct))

littermsr2 <- read_csv("data/processed_data/midstory removal/longlitter.csv") %>% 
  filter(litter_type!=c("sg")) %>%
  filter(litter_type!=c("wo")) %>%
  filter(litter_type!=c("meso"))
  
littermsr <- littermsr %>% 
  mutate(litter_type=c("encroaching"))
  

msrlit <- rbind(littermsr2,littermsr) %>% 
  group_by(collection, Plot, Treatment, canopy_trt, litter_type) %>% 
  summarise(litter_pct = mean(litter_pct))

%>%
  filter(litter_type=="encroaching")

msrlit$collection <- factor(msrlit$collection, levels = c("6/24/2021","1/17/2022","4/12/2022","6/24/2022"))


ggplot(msrlit, aes(x=collection, y = litter_pct)) +
  geom_boxplot() +
  facet_wrap(~litter_type) +
  theme_bw()

res25 <- lme(data = msrlit, litter_pct~collection, random = ~1|Plot/canopy_trt/Treatment)
summary(res25)
TukeyHSD(aov(res25))
confint(emmeans(res25, pairwise ~ collection))
intervals(res25)

trees %>% 
  filter(dbh < 20) %>% 
  filter(functional_group=="mesophyte") %>% 
  filter(location %in% c(1,2,3,4))


trees <- read_csv("data/processed_data/MOTtrees_clean.csv")

trees$functional_group[trees$functional_group=="mesophyte"] <- "Encroaching"
trees$functional_group[trees$functional_group=="hardwood pyro"] <- "Upland oak"
trees$functional_group[trees$functional_group=="pine"] <- "Pine"
trees$functional_group <- factor(trees$functional_group, levels = c(
  "Encroaching","Pine", "Upland oak"))
trees_msrdist <- trees %>% 
  group_by(stem_id, plot, thin_lvl, functional_group) %>% 
  summarise(QMD = dbh)
sqrt((sum((dbh)^2))/n_distinct(stem_id))

g3 <- ggplot(trees_msrdist, aes(x=QMD,fill = functional_group)) +
geom_histogram(stat="bin", color = "black",binwidth = 4) +
  theme_bw() +
  labs(x= expression(paste("DBH (cm)")),
       y= "Number of individuals",
       fill = "Functional Group") +
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0,70)) + 
  scale_fill_grey(start = 0.35, end =0.9) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))

legendg2 <- get_legend(
  
  g3 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
             legend.title=element_text(size=12), 
             legend.text=element_text(size=12)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

g5 <- plot_grid(g3,g4,
                ncol = 2)  

g6 <- plot_grid(g5, legendg2, ncol = 1, rel_heights = c(1, .1))

msrtpha <- trees %>% 
  filter(location!="edge") %>% 
  filter(species!="VACCI") %>% 
  filter(dbh < 20) %>% 
  group_by(functional_group) %>%
  summarise(cmb20 = n_distinct(stem_id))
msrtpha <- trees %>% 
  filter(location!="edge") %>% 
  filter(species!="VACCI") %>% 
  group_by(functional_group) %>% 
  summarise(TPHA = n_distinct(stem_id)/2.471)


msrtpha$functional_group <- factor(msrtpha$functional_group, levels = c(
  "Encroaching","Pine", "Upland oak"))

g4 <- ggplot(msrtpha, aes(x = functional_group, y = TPHA, fill = functional_group)) +
  geom_bar(stat="identity", color = "black") +
  theme_bw() +
  labs(x= expression(paste("Functional Group")),
       y= "Density (trees per hectare)",
       color = "Functional Group",
       fill = "Functional Group") +
  scale_fill_grey(start = 0.35, end =0.9) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  theme(legend.position = "none", legend.title = element_blank()) 
  
#litter change figs
compchange <- read_csv("data/processed_data/midstory removal/littercompchangesplotALL.csv")

compchangeplot
compchangeplot$litter_type[compchangeplot$litter_type=="meso"] <- "Encroaching"
compchangeplot$litter_type[compchangeplot$litter_type=="uo"] <- "Upland oak"
compchangeplot$litter_type[compchangeplot$litter_type=="pine"] <- "Pine"


compchangeplot <- compchangeplot %>% 
  filter(collection!=c("4/12/2022")) %>%
  filter(collection!=c("1/17/2022")) 

change1 <- ggplot(compchangeplot, aes(x = Treatment, y = litter_pct, fill = litter_type)) +
  geom_boxplot() +
  facet_wrap(~collection) + 
  theme_bw() + 
  scale_fill_grey(start = 0.35, end =0.9) +
  theme(plot.title = element_text(hjust =0), legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
  labs(y = "Leaf litter by mass (%)",
       x="Treatment",
       fill = "Functional Group") +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
             legend.title=element_text(size=12), 
             legend.text=element_text(size=12))

#decomp angle
compchange

compchange$Treatment[compchange$Treatment=="thin"] <- "Thin"
compchange$Treatment[compchange$Treatment=="control"] <- "Control"
compchange$canopy_trt[compchange$canopy_trt=="low"] <- "Low"
compchange$canopy_trt[compchange$canopy_trt=="med"] <- "Medium"
compchange$canopy_trt[compchange$canopy_trt=="high"] <- "High"
compchange$collection[compchange$collection=="6/24/2021"] <- "June 2021 Growing Season - Before Thinning"
compchange$collection[compchange$collection=="1/17/2022"] <- "January 2022 Early Dormant Season - After Thinning"
compchange$collection[compchange$collection=="4/12/2022"] <- "April 2022 Late Dormant Season - After Thinning"
compchange$collection[compchange$collection=="6/24/2022"] <- "June 2022 Growing Season - After Thinning"

compchange$collection[compchange$collection=="2021 - Before Thinning"] <- "June 2021 Growing Season - Before Thinning"
compchange$collection[compchange$collection=="1/17/2022"] <- "January 2022 Early Dormant Season - After Thinning"
compchange$collection[compchange$collection=="4/12/2022"] <- "April 2022 Late Dormant Season - After Thinning"
compchange$collection[compchange$collection=="6/24/2022"] <- "June 2022 Growing Season - After Thinning"

compchange$collection <- factor(compchange$collection, levels = c(
  "6/24/2021",
  "1/17/2022",
  "4/12/2022",
  "6/24/2022"))

compchange$collection <- factor(compchange$collection, levels = c(
  "June 2021 Growing Season - Before Thinning",
  "January 2022 Early Dormant Season - After Thinning",
  "April 2022 Late Dormant Season - After Thinning",
  "June 2022 Growing Season - After Thinning"))

compchange$canopy_trt <- factor(compchange$canopy_trt, levels = c("Low","Medium","High"))

compchange$litter_type[compchange$litter_type=="meso"] <- "Encroaching"
compchange$litter_type[compchange$litter_type=="uo"] <- "Upland oak"
compchange$litter_type[compchange$litter_type=="pine"] <- "Pine"
compchange$litter_type <- factor(compchange$litter_type, levels = c(
  "Encroaching","Pine", "Upland oak"))
decomp <- ggplot(compchange, aes(x = Treatment, y = litter_pct, fill = litter_type)) +
  geom_boxplot() +
  facet_wrap(~collection) + 
  theme_bw() + 
  scale_fill_grey(start = 0.35, end =0.9) +
  theme(plot.title = element_text(hjust =0), legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
  labs(y = "Leaf litter by mass (%)",
       x="Treatment",
       fill = "Species Group")

decomp <- compchange %>% 
  group_by(collection, Plot, Treatment, canopy_trt, litter_type) %>% 
  summarise(mean_pct = mean(litter_pct))

ggplot(decomp, aes(x = collection, y = mean_pct, color = litter_type, group = litter_type))+
  facet_wrap(~Treatment) +
  theme_bw() +
  geom_point(alpha = 0.35) +
  stat_summary(fun=mean, geom="line", size = 1) +
  stat_summary(fun=mean, geom="point", size = 5)

decomp$collection <- factor(decomp$collection, levels = c(
  "6/24/2021",
  "1/17/2022",
  "4/12/2022",
  "6/24/2022"))
#for pub
g7 <- ggplot(decomp, aes(x = collection, y = mean_pct, color = litter_type, group = litter_type))+
  facet_wrap(~Treatment) +
  theme_bw() +
  geom_point(alpha = 0.35) +
  stat_summary(fun=mean, geom="line", size = 2) +
  stat_summary(fun=mean, geom="point", size = 5) +
  labs(x = "\nLeaf litter harvest (m/dd/yyyy)",
       y = "\nLeaf litter by mass (%)",
       color = "Functional Group") +
  scale_color_grey(start = 0.05, end =0.7) + 
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  theme(legend.position = "none")

legendg3 <- get_legend(
  
  g7 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
             legend.title=element_text(size=12), 
             legend.text=element_text(size=12)) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

g8 <- plot_grid(g7, legendg3, ncol = 1, rel_heights = c(1, .1))


  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) 
  
  

ggplot(decomp, aes(x = collection, y = mean_pct, color = litter_type, group = litter_type))+
  facet_wrap(~canopy_trt) +
  theme_bw() +
  geom_point(alpha = 0.35) +
  stat_summary(fun=mean, geom="line", size = 1) +
  stat_summary(fun=mean, geom="point", size = 5) +
  labs(x = "Leaf litter harvest (m/dd/yyyy)",
       y = "Leaf litter by mass (%)",
       color = "Species Group")



#### models for comparing total and indiviudal fuel loads

log_FuelMaster <- read_csv("data/processed_data/midstory removal/TotalFuelLoads_Mgha_log.csv") %>% 
  filter(Collection=="post-thin")

herbtest <- lme(data=log_FuelMaster, Biomass_Herb~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(herbtest)

shrubtest <- lme(data=log_FuelMaster, Biomass_Shrub~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(shrubtest)

cwdtest <- lme(data=log_FuelMaster, CWD_Mgha~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(cwdtest)

fwdtest <- lme(data=log_FuelMaster, FWD_Mgha~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(fwdtest)

littest <- lme(data=log_FuelMaster, Biomass_Litter_kgm2~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(littest)

duftest <- lme(data=log_FuelMaster, Biomass_Duff_kgm2~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(duftest)

totallogs <- log_FuelMaster %>% 
  group_by(Collection, Plot, Treatment, canopy_trt) %>% 
  summarise(sumtotallogfl =sum(Biomass_Herb+Biomass_Shrub+CWD_Mgha+FWD_Mgha+
                                 Biomass_Litter_kgm2+Biomass_Duff_kgm2))
totaltest2 <- lme(data=totallogs, sumtotallogfl~Collection+canopy_trt ,random = ~1|Plot/Treatment)
summary(totaltest2)

# for actual value differences
fornumbsmaster <- read_csv("data/processed_data/midstory removal/TotalFuelLoads_Mgha.csv") %>% 
  filter(Collection=="pre-thin")


herbtest <- lme(data=fornumbsmaster, Biomass_Herb~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(herbtest)

shrubtest <- lme(data=fornumbsmaster, Biomass_Shrub~Treatment+canopy_trt ,random = ~1|Plot/Treatment)
summary(shrubtest)

cwdtest <- lme(data=fornumbsmaster, CWD_Mgha~Treatment+canopy_trt,random = ~1|Plot/Treatment)
summary(cwdtest)

fwdtest <- lme(data=fornumbsmaster, FWD_Mgha~Treatment+canopy_trt,random = ~1|Plot/Treatment)
summary(fwdtest)

littest <- lme(data=fornumbsmaster, Biomass_Litter_kgm2~Treatment+canopy_trt,random = ~1|Plot/Treatment)
summary(littest)

duftest <- lme(data=fornumbsmaster, Biomass_Duff_kgm2~Treatment+canopy_trt,random = ~1|Plot/Treatment)
summary(duftest)

totalraws <- fornumbsmaster %>% 
  group_by(Collection, Plot, Treatment, canopy_trt) %>% 
  mutate(sumtotallogfl2 =sum(Biomass_Herb+Biomass_Shrub+CWD_Mgha+FWD_Mgha+
                                 Biomass_Litter_kgm2+Biomass_Duff_kgm2))


totaltest3 <- lme(data=totalraws, sumtotallogfl2~Treatment+canopy_trt,random = ~1|Plot/Treatment)
summary(totaltest3)

rawmeanse <- totalraws %>% 
  group_by(Collection,Treatment) %>% 
  summarise(total_mean = mean(sumtotallogfl2),
            total_se = std.error(sumtotallogfl2))

# models for changing fuel comp in litter
compchange
decompmod


pinedecomp <- compchange %>% 
  filter(litter_type=="pine") %>% 
  filter(Treatment=="thin")
uodecomp <- compchange %>% 
  filter(litter_type=="uo") %>% 
  filter(Treatment=="thin")
mesodecomp <- compchange %>% 
  filter(litter_type=="meso") %>% 
  filter(Treatment=="thin")

modpine <- lme(data=pinedecomp, litter_pct~canopy_trt+collection, random = ~1|Plot/canopy_trt/Treatment/Burn_Szn) 
summary(modpine)
emmeans(modpine, pairwise ~ canopy_trt)

moduo <- lme(data=uodecomp, litter_pct~canopy_trt+collection, random = ~1|Plot/canopy_trt/Treatment) 
summary(moduo)
emmeans(moduo, pairwise ~ canopy_trt)

modmeso <- lme(data=mesodecomp, litter_pct~canopy_trt+collection, random = ~1|Plot/canopy_trt/Treatment) 
summary(modmeso)
emmeans(modmeso, pairwise ~ canopy_trt)

