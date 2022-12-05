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
library(factoextra)
library(nlme)

litter <- read_csv("C:/Users/smc0124/OneDrive - Auburn University/Desktop/AuburnMS/data/raw_data/Midstory Removal/most important/LitterSortingALL_MSR.csv")

litter$canopy_trt <- factor(litter$canopy_trt, levels = c("low","med","high"))
litter$collection <- factor(litter$collection, levels = c("6/24/2021","1/17/2022","4/12/2022","6/24/2022"))
litter$Treatment[litter$Treatment=="thin"] <- "Thin"
litter$Treatment[litter$Treatment=="control"] <- "Control"

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
gslitter <- pct_litter %>% 
  filter(Burn_Szn %in% c("GS")) %>% 
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
gsl <- gslitter %>% 
  filter(collection=="6/24/2022")

edlldgsl <- rbind(edl,ldl,gsl)

burnlittercomp <- edlldgsl %>% 
  group_by(Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(pine = pine,
            uo = uo,
            meso = sum(sg+wo+meso))

ggplot(pct_litter, aes(x = Treatment), color = Treatment) +
  geom_boxplot(aes(y=meso_pct)) +
  facet_wrap(~collection+canopy_trt)

#write_csv(pct_litter, "data/processed_data/Midstory Removal/longlitter.csv")
#fire stuff

burn <- read_csv("data/raw_data/Midstory Removal/most important/MOTburndataforR_ALLSEASONS.csv")
head(burn)


burn$meso_os <- factor(burn$canopy_trt, levels = c("low","med","high"))
burn$Burn_Szn <- factor(burn$Burn_Szn, levels = c("ED","LD","GS"))

ggplot(burn, aes(x = Burn_Szn)) +
  geom_boxplot(aes(y=consump)) +
  facet_wrap(~meso_os) +
  theme_bw() 

#combining fire season with specific litteN=r collection
edlldl <- edlldl[-c(1)]
burnxlitter <- left_join(burnlittercomp,burn)
#write_csv(burnxlitter, "data/processed_data/midstory removal/litterxEDLD.csv")


typeburn <- read_csv("data/processed_data/midstory removal/litterbytypexEDLD.csv")
typeburn$canopy_trt <- factor(typeburn$canopy_trt, levels = c("low","med","high"))

ggplot(burnxlitter, aes(x = meso, y = consump, color = canopy_trt)) +
  geom_smooth(method = "lm", se = F) +
  geom_point()+
  facet_wrap(~Treatment+Burn_Szn)

summary(burnxlitter)

lowconsump <- burnxlitter %>%
  filter(consump < 15)
summary(lowconsump)
ggplot(typeburn, aes(x = Treatment, y = percent, color = litter_type)) +
  geom_boxplot() +
  facet_wrap(~canopy_trt+Burn_Szn)

lowconsump_meanse <- lowconsump %>% 
  ungroup(.) %>% 
  summarise(meso_mean = mean(meso),
            meso_se = std.error(meso))



litterlong <- read_csv("data/processed_data/midstory removal/longlitter.csv")

litterlong$canopy_trt <- factor(litterlong$canopy_trt, levels = c("low","med","high"))
litterlong$litter_type <- factor(litterlong$litter_type, levels = c("sg","wo","meso", "pine","uo"))
litterlong$collection <- factor(litterlong$collection, levels = c("6/24/2021","1/17/2022","4/12/2022","6/24/2022"))

ggplot(litterlong, aes(x = Treatment, y = litter_pct, color = litter_type)) +
  geom_boxplot() +
  facet_wrap(~canopy_trt)


littertest <- litterlong %>% 
  group_by(collection, Plot, Treatment, canopy_trt, litter_type) %>% 
  summarise(plot_litter = mean(litter_pct))

ggplot(littertest, aes(x = Treatment, y = plot_litter, color = litter_type)) +
  geom_boxplot() +
  facet_wrap(~canopy_trt)

res1 <- lm(data=littertest, plot_litter~canopy_trt*litter_type*Treatment)
summary(res1)

#calc fli

fli <- burn %>%
  group_by(Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(fli = mean(258*(avg_fh/100)^2.17),
            frs1 = mean(frs1_cms),
            frs2 = mean(frs2_cms),
            avg_fh = mean(avg_fh),
            consump = mean(consump),
            avg_max_temp = mean(avg_max_temp),
            fuel_moist = mean(fuel_moist))

p1 <- ggplot(fli, aes(x = Burn_Szn, y = fli, fill = Burn_Szn)) +
  geom_boxplot() +
  facet_wrap(~Treatment) +
  theme_bw()+
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y=expression(paste("Fireline Intensity  kW  ",  m^{-1}))) +
  scale_fill_manual(labels = c("ED", "LD", "GS"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
p2 <- ggplot(fli, aes(x = Burn_Szn, y = consump, fill = Burn_Szn)) +
  geom_boxplot() +
  facet_wrap(~Treatment) +
  theme_bw()+
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y="Fuel Consumption (%)") +
  scale_fill_manual(labels = c("ED", "LD", "GS"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
p3 <- ggplot(fli, aes(x = Burn_Szn, y = frs1, fill = Burn_Szn)) +
  geom_boxplot() +
  facet_wrap(~Treatment) +
  theme_bw()+
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y=expression(paste("Fire Spread Rate  cm  ", s^{-1}))) +
  scale_fill_manual(labels = c("ED", "LD", "GS"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
p4 <- ggplot(fli, aes(x = Burn_Szn, y = avg_max_temp, fill = Burn_Szn)) +
  geom_boxplot() +
  facet_wrap(~Treatment) +
  theme_bw()+
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y = "Maximum Temperature °C") +
  scale_fill_manual(labels = c("ED", "LD", "GS"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))

legend <- get_legend(
  p3 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
             legend.title=element_text(size=12), 
             legend.text=element_text(size=12)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
    labs(fill = "Burn Season") 
  
  +
    scale_fill_grey(start = 0.35, end =0.9))
    
newcolorsSEASON <- scale_fill_manual(labels = c("Early Dormant (ED)", "Late Dormant (LD)", "Growing Season (GS)"), 
                      values = c("#00AFBB","#E7B800","#FC4E07"))
newcolorsSPECIES <- scale_color_manual(labels = c("Encroaching", "Pine", "Upland oak"), 
                               values = c("#00AFBB","#E7B800","#FC4E07"))

title <- ggdraw() +
  draw_label(
    "Four fire behavior parameters between control and thin treatments at three burn times",
    fontface = "bold",
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0,0,0,40)
  )

p5 <- cowplot::plot_grid(p1 + theme(axis.title.x = element_blank()) + scale_fill_grey(start = 0.2, end = .85)
,
                         p2 + theme(axis.title.x = element_blank())+ scale_fill_grey(start = 0.2, end = .85)
,
                         p3 + theme(axis.title.x = element_blank())+ scale_fill_grey(start = 0.2, end = .85)
,
                         p4 + theme(axis.title.x = element_blank())+ scale_fill_grey(start = 0.2, end = .85)
,
                         ncol = 2,
                         labels = "AUTO")

p5 <- cowplot::plot_grid(p2 + theme(axis.title.x = element_blank()) +   scale_fill_manual(labels = c("ED", "LD", "GS"), 
                                                                                          values = c("#00AFBB","#E7B800","#FC4E07"))
                         ,
                         p3 + theme(axis.title.x = element_blank()) +   scale_fill_manual(labels = c("ED", "LD", "GS"), 
                                                                                          values = c("#00AFBB","#E7B800","#FC4E07")),
                         ncol = 2,
                         labels = "AUTO")

p6 <- plot_grid(p5,legend, ncol = 1,rel_heights = c(1,.1))
#ggsave(plot=p6,"figures/midstory removal figs/firebehaviorxtrtseason.png", height = 10, width = 10)
#^final for four major fire metrics per control/thin
res1 <- lm(data=fli, fli~Burn_Szn)
summary(res1)
TukeyHSD(aov(res1))

fli_litterpct <- left_join(fli, burnlittercomp)
fli_lit_burn <- anti_join(fli_litterpct, typeburn)

typeburn$litter_type <- NULL
typeburn$percent <- NULL
typeburn$meso_os <- NULL

ggplot(fli_litterpct, aes(x = pine, y = fli, color = Treatment)) +
  geom_smooth(method = "lm", se = F) + 
  geom_point() +
  facet_wrap(~canopy_trt) + 
  theme_bw()

res2 <- lm(data=fli_litterpct, fli~pine+Treatment+pine:Treatment)
summary(res2)

fli_litterpct$collection <- NULL

#pca attempt for fli
myPr <- prcomp(fli_litterpct[,5:16], center = T, scale = T)
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0
)

fit <- princomp(fli_litterpct[,5:10], cor = T)
fviz_pca_biplot(fit)

#extract pc scores

str(myPr)
myPr$x
fli_litterpct2 <- cbind(fli_litterpct, myPr$x[,1:2])

## ggplotting with pc scores

ggplot(fli_litterpct2, aes(PC1, PC2, col = canopy_trt, fill = canopy_trt)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(fli_litterpct2[,5:10], fli_litterpct2[,11:12])

#pca attempt for litterlong
myPr <- prcomp(litterlong[,7], center = T, scale = T)
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0
)

fit <- princomp(fli_litterpct[,5:10], cor = T)
fviz_pca_biplot(fit)

#extract pc scores

str(myPr)
myPr$x
fli_litterpct2 <- cbind(fli_litterpct, myPr$x[,1:2])

## ggplotting with pc scores

ggplot(fli_litterpct2, aes(PC1, PC2, col = canopy_trt, fill = canopy_trt)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(fli_litterpct2[,5:10], fli_litterpct2[,11:12])

#random

ggplot(fli_litterpct2, aes(x = canopy_trt, y = fli)) + 
  geom_boxplot() +
  facet_wrap(~Treatment)
  

res5 <- lm(data = fli_litterpct2, avg_max_temp~Burn_Szn*canopy_trt)
summary(res5)




#testing litter comp at time of burning relationships

fli
burnlittercomp
firecomp <- left_join(fli,burnlittercomp)
firecomp$Burn_Szn <- factor(firecomp$Burn_Szn, levels = c("ED","LD","GS"))

fc1 <- ggplot(firecomp, aes(x = pine, y = fli)) +
  geom_point(aes(color = Burn_Szn), size = 2) +
  geom_smooth(aes(color = Burn_Szn), method = "lm", se = F) +
  theme_bw() +
  facet_wrap(~Treatment) +
  scale_color_manual(labels = c("Early Dormant (ED)", "Late Dormant (LD)", "Growing Season (GS)"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y=expression(paste("Fireline Intensity  kW  ",  m^{-1}))) +
  theme(axis.title=element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 

fc2 <- ggplot(firecomp, aes(x = pine, y = consump)) +
  geom_point(aes(color = Burn_Szn), size = 2) +
  geom_smooth(aes(color = Burn_Szn), method = "lm", se = F) +
  theme_bw() +
  facet_wrap(~Treatment) +
  scale_color_manual(labels = c("Early Dormant (ED)", "Late Dormant (LD)", "Growing Season (GS)"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y="Fuel Consumption (%)") +
  theme(axis.title=element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 

fc3 <- ggplot(firecomp, aes(x = pine, y = frs1)) +
  geom_point(aes(color = Burn_Szn), size = 2) +
  geom_smooth(aes(color = Burn_Szn), method = "lm", se = F) +
  theme_bw() +
  facet_wrap(~Treatment) +
  scale_color_manual(labels = c("Early Dormant (ED)", "Late Dormant (LD)", "Growing Season (GS)"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y=expression(paste("Fire Spread Rate  cm  ", s^{-1})),
       x="Fuel bed pine litter (%) ") +
  theme(axis.title=element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 

fc4 <- ggplot(firecomp, aes(x = pine, y = avg_max_temp)) +
  geom_point(aes(color = Burn_Szn), size = 2) +
  geom_smooth(aes(color = Burn_Szn), method = "lm", se = F) +
  theme_bw() +
  facet_wrap(~Treatment) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  labs(y = "Maximum Temperature (°C)",
       x="Fuel bed pine litter (%) ") +
  theme(axis.title=element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 

legend2 <- get_legend(
  fc1 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
              legend.title=element_text(size=16), 
              legend.text=element_text(size=16)) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
    labs(color = "Burn Season") +
    scale_color_grey(start = 0.2, end = .85))

    scale_color_manual(labels = c("Early Dormant (ED)", "Late Dormant (LD)", "Growing Season (GS)"), 
                      values = c("#00AFBB","#E7B800","#FC4E07")))
title2 <- ggdraw() +
  draw_label(
    "Four fire behavior parameters vs. pine litter fuel (%) between control and thin treatments at three burn times ",
    fontface = "bold",
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0,0,0,40)
  )

fc5 <- cowplot::plot_grid(fc1 + theme(axis.title.x = element_blank()) + scale_color_grey(start = 0.2, end = .85),
                         fc2 + theme(axis.title.x = element_blank()) + scale_color_grey(start = 0.2, end = .85),
                         fc3 + scale_color_grey(start = 0.2, end = .85),
                         fc4 + scale_color_grey(start = 0.2, end = .85),
                         ncol = 2, labels = "AUTO",
                         align = "hv")
                         


fc6 <- plot_grid(fc5, ncol = 1, legend2,rel_heights = c(1, .1)) 

fc7 <- plot_grid(fc6,ncol=1,rel_heights = c(0.1,1))


res8a <- lm(data=firecomp, avg_max_temp~pine+Burn_Szn)
summary(res8a)
TukeyHSD(aov(res8a))
confint(res8a)
res8 <- lme(data=firecomp, avg_max_temp~pine, random = ~1|Plot)
summary(res8)
intervals(res8)
anova(res8,res8a)

#litter for comparison of 2021 to 2022 
litter
pct_litter

litterlong

pineuo <- litterlong %>% 
  filter(litter_type %in% c("pine","uo"))

testlit <- litterlong %>%
  filter(litter_type %in% c("sg","wo","meso")) %>% 
  group_by(collection, Plot, Treatment, Burn_Szn, canopy_trt) %>% 
  summarise(litter_type = "meso",
            litter_pct = sum(litter_pct))

compchange <- rbind(pineuo,testlit)
compchange$litter_type <- factor(compchange$litter_type, levels = c("pine","uo","meso"))

compchangeplot <- compchange %>% 
  group_by(collection, Plot, Treatment, canopy_trt, litter_type) %>% 
  summarise(litter_pct = mean(litter_pct))
#write_csv(compchangeplot,"data/processed_data/midstory removal/littercompchangesplotALL.csv")
compchangeplot2 <- read_csv("data/processed_data/midstory removal/littercompchangesplot.csv")
compchangeplot$Treatment[compchangeplot$Treatment=="thin"] <- "Thin"
compchangeplot$Treatment[compchangeplot$Treatment=="control"] <- "Control"
compchangeplot$canopy_trt[as.character(compchangeplot$canopy_trt=="low")] <- as.character("Low")
compchangeplot$canopy_trt[compchangeplot$canopy_trt=="med"] <- "Medium"
compchangeplot$canopy_trt[compchangeplot$canopy_trt=="high"] <- "High"
compchangeplot2$collection[compchangeplot2$collection=="6/24/2021"] <- "2021 - Before Thinning"
compchangeplot2$collection[compchangeplot2$collection=="6/24/2022"] <- "2022 - After Thinning"
compchangeplot$canopy_trt <- factor(compchangeplot$canopy_trt, levels = c("Low","Medium","High"))
compchangeplot$litter_type <- factor(compchangeplot$litter_type, levels = c("pine","uo","meso"))


change1 <- ggplot(compchangeplot, aes(x = Treatment, y = litter_pct, fill = litter_type)) +
  geom_boxplot() +
  facet_wrap(~collection) + 
  theme_bw() + 
  scale_fill_grey(start = 0.2, end = .85) +
  theme(plot.title = element_text(hjust =0), legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
  labs(y = "Leaf litter by mass (%)",
       x="Treatment",
       fill = "Species Group")
change2 <- ggplot(compchangeplot, aes(x = canopy_trt, y = litter_pct, fill = litter_type)) +
  geom_boxplot() +
  theme_bw() + 
  scale_fill_manual(labels = c("Pine", "Upland Oak", "Mesophyte"), 
                    values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(plot.title = element_text(hjust =0), legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
  labs(y = "Leaf litter by mass (%)",
       x="Mesophyte Overstory",
       fill = "Litter Type") +
  ggtitle("Change in fuel composition between low, medium, and high mesophyte overstory")


mesoonly <- compchangeplot %>% 
  filter(litter_type=="uo")
comp1 <- lm(data=mesoonly, litter_pct~canopy_trt) 
summary(comp1)
TukeyHSD(aov(comp1))

comp2 <- lme(data=compchangeplot, litter_pct~Treatment*collection, random = ~1|Plot/Treatment) 
summary(comp2)
intervals(comp2)
anova(comp2,comp3)
 

TukeyHSD(aov(comp1))


#canopy <- read_csv("data/raw_data/Midstory Removal/most important/CanopyCoversALL.csv")

canopy <- read_csv("data/processed_data/midstory removal/CanopyCoverBeforeAfter.csv")
beforeafter <- canopy %>% 
  filter(time %in% c("7/1/2021","9/28/2022")) %>% 
  group_by(time, plot, treatment) %>%
  summarise(avg_cc = mean(pct_cc)) %>% 
  mutate(meso_os = "hi")

beforeafter$meso_os[beforeafter$plot==c(1)] <- "High"
beforeafter$meso_os[beforeafter$plot==c(3)] <- "High"
beforeafter$meso_os[beforeafter$plot==c(9)] <- "High"
beforeafter$meso_os[beforeafter$plot==c(12)] <- "High"


beforeafter$meso_os[beforeafter$plot==c(2)] <- "Medium"
beforeafter$meso_os[beforeafter$plot==c(7)] <- "Medium"
beforeafter$meso_os[beforeafter$plot==c(8)] <- "Medium"
beforeafter$meso_os[beforeafter$plot==c(10)] <- "Medium"

beforeafter$meso_os[beforeafter$plot==c(4)] <- "Low"
beforeafter$meso_os[beforeafter$plot==c(5)] <- "Low"
beforeafter$meso_os[beforeafter$plot==c(6)] <- "Low"
beforeafter$meso_os[beforeafter$plot==c(11)] <- "Low"

boxplot(data=beforeafter, avg_cc~meso_os+time+treatment)

beforeafter$treatment <- factor(beforeafter$treatment, levels = c(
  "Before Thinning",
  "After Thinning"))
beforeafter$meso_os <- factor(beforeafter$meso_os, levels = c("Low","Medium","High"))

g45 <- ggplot(beforeafter, aes(x = meso_os, y = avg_cc, fill = meso_os)) +
  geom_boxplot() +
  facet_wrap(~treatment) +
  theme_bw() +
  scale_fill_manual(values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = "none") +
  labs(y = "\nAverage Canopy Cover (%)",
       x = "\nResidual Encroaching Overstory Group")
legendg45 <- get_legend(
  
  g45 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
             legend.title=element_blank(), 
             legend.text=element_text(size=12)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))
  

g46 <- plot_grid(g45, legendg45, ncol = 1, rel_heights = c(1, .1))


onlypre <- beforeafter %>% 
  filter(time=="7/1/2021")
onlypost <- beforeafter %>% 
  filter(time=="9/28/2022")
onlycon <- beforeafter %>% 
  filter(treatment=="control")
onlythin <- beforeafter %>% 
  filter(treatment=="thin")

cc1 <- lme(data=beforeafter, avg_cc~time+treatment*meso_os, random = ~1|plot/treatment)
summary(cc1)
TukeyHSD(aov(cc1))

pre <- lm(data = onlypre, avg_cc~treatment)
summary(pre)
intervals(pre)
post <- lm(data = onlypost, avg_cc~meso_os)
summary(post)
confint(post)
post2 <- lm(data = onlypost, avg_cc~treatment)
summary(post2)
anova(post,post2)

thin <- lm(data=onlythin, avg_cc~time+meso_os)
summary(thin)
confint(thin)


fli
new <- lme(fli~Treatment+Burn_Szn+meso_os, data = fli, random = ~1|Plot/Treatment/Burn_Szn)
summary(new)


trees <- read_csv("data/processed_data/MOTtrees_clean.csv")

boxplot(data=burn, fuel_moist~canopy_trt)
burn$canopy_trt <- factor(burn$canopy_trt, levels = c("low","med","high"))
burn$Burn_Szn <- factor(burn$Burn_Szn, levels = c("ED","LD","GS"))

g95 <- ggplot(burn, aes(x = Burn_Szn, y = fuel_moist, fill = Treatment)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Burn Season",
       y = "Fuel moisture (%)",
       fill = "Treatment") +
  scale_fill_manual(values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = "none") +
  facet_wrap(~canopy_trt)

legendg95 <- get_legend(
  
  g95 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
              legend.title=element_blank(), 
              legend.text=element_text(size=12)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))


g96 <- plot_grid(g95, legendg95, ncol = 1, rel_heights = c(1, .1))

litter
