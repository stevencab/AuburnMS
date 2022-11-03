#playing
library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggsignif
        )
library(factoextra)
MasterPCA <- read_csv("data/processed_data/MixedStand_MasterPCA.csv")




myPr <- prcomp(MasterPCA[,4:22], center = T, scale = T)
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0
)

fit <- princomp(MasterPCA[,4:22], cor = T)
fviz_pca_biplot(fit)

#extract pc scores

str(myPr)
myPr$x
MasterPCA2 <- cbind(MasterPCA, myPr$x[,1:2])

## ggplotting with pc scores

ggplot(MasterPCA2, aes(PC1, PC2, col = group, fill = group)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(MasterPCA[,4:22], MasterPCA2[,23:24])

##process saps and seeds

sapseed <- read_csv("data/raw_data/Mixed Stands/MixedStand_SaplingSeedlings.csv")

sap <- sapseed %>% 
  filter(Type=="Sapling")
seed <- sapseed %>% 
  filter(Type=="Seedling")


sapcalc <- sap %>% 
  group_by(Site, Plot, Functional) %>%
  summarise(sum_count = sum(Count),
            Saplings_100m2 = log(((sum_count+1)/3.6)*100))

seedcalc <- seed %>% 
  group_by(Site, Plot, Functional) %>%
  summarise(sum_count = sum(Count),
            Seedlings_100m2 = log(((sum_count+1)/1.13)*100))

#more sap seed for transfer to master for PCA

sapseedsums <- sapseed %>% 
  group_by(Site, Plot, Type, Functional) %>% 
  summarise(pine_sum = sum(Count[Functional=="pine"]),
            uo_sum = sum(Count[Functional=="upland oak"]),
            meso_sum = sum(Count[Functional=="mesophyte"])) 

sapseedarea <- sapseedsums %>% 
  group_by(Site, Plot) %>% 
  summarise(Pine_Seedlings100m2 = ((pine_sum[Type=="Seedling"]/1.13)*100),
            UO_Seedlings100m2 = ((uo_sum[Type=="Seedling"]/1.13)*100),
            Meso_Seedlings100m2 = ((meso_sum[Type=="Seedling"]/1.13)*100),
            Pine_Saplings100m2 = ((pine_sum[Type=="Sapling"]/3.6)*100),
            UO_Saplings100m2 = ((uo_sum[Type=="Sapling"]/3.6)*100),
            Meso_Saplings100m2 = ((meso_sum[Type=="Sapling"]/3.6)*100))

sapseedareaHA <- sapseedsums %>% 
  group_by(Site, Plot, Functional) %>% 
  summarise(Pine_SeedlingsHA = ((pine_sum[Type=="Seedling"]/4.01)*10000),
            UO_SeedlingsHA = ((uo_sum[Type=="Seedling"]/4.01)*10000),
            Meso_SeedlingsHA = ((meso_sum[Type=="Seedling"]/4.01)*10000),
            Pine_SaplingsHA = ((pine_sum[Type=="Sapling"]/40.72)*10000),
            UO_SaplingsHA = ((uo_sum[Type=="Sapling"]/40.72)*10000),
            Meso_SaplingsHA = ((meso_sum[Type=="Sapling"]/40.72)*10000))

write_csv(sapseedarea, file = "data/processed_data/MixedStand_SaplingsSeedlings.csv")
            
            
n_distinct(mesos$Plot)
  filter(Functional=="mesophyte")
pines<- sapseed %>% 
  filter(Functional=="pine")
uos<- sapseed %>% 
  filter(Functional=="upland oak")






####### making figures for presentation

Master <- read_csv("data/processed_data/MixedStand_Master.csv")
litter <-read_csv("data/processed_data/litterxba_mesossummed.csv")
basapseed <- read_csv("data/processed_data/MixedStand_baxsapseed.csv")

litter$Little_type <- factor(litter$Little_type, levels = c("Pinus","Upland oak","Mesophyte","Total"))

#fuel composition 

  geom_point(aes(color = Little_type)) +
  geom_smooth(method = "lm", aes(color = Little_type), se = F) +
  ylab("Leaf liter fuel composition by mass (%)") +
  xlab("Pine Basal Area (%)") +
  geom_vline(xintercept = 30) +
  geom_hline(yintercept = 30)

ggplot(litter, aes(x=Pine_pctBAft2a, y=Pct_wt)) +
  geom_point(aes(color = Little_type)) +
  geom_smooth(method = "lm", aes(color = Little_type), se = F) +
  facet_wrap(~Site) +
  ylab("Leaf liter fuel composition by mass (%)") +
  xlab("Pine Basal Area (%)")

#fuel load

ggplot(litter, aes(x=Pine_pctBAft2a, y=Load_wt)) +
  geom_point(aes(color = Little_type)) +
  geom_smooth(method = "lm", aes(color = Little_type), se = F) +
  ylab("Leaf liter fuel load (g/m2)") +
  xlab("Pine Basal Area (%)")

ggplot(litter, aes(x=Pine_pctBAft2a, y=Load_wt)) +
  geom_point(aes(color = Little_type)) +
  geom_smooth(method = "lm", aes(color = Little_type), se = F) +
  facet_wrap(~Site) +
  ylab("Leaf liter fuel load (g/m2)") +
  xlab("Pine Basal Area (%)")

###### other fuels -----
Master$group <- factor(Master$group, levels = c(">70% Pine","30 - 70% Pine","< 30% Pine"))
seedcalc$Functional <- factor(seedcalc$Functional, levels = c("Pine","Upland Oak","Mesophyte","#N/A"))

#canopy cover
ggplot(Master, aes(x=group, y= Avg_CC)) +
  geom_boxplot(aes(y=Avg_CC)) +
  theme_bw() +
  scale_x_discrete(labels=c(">70% Pine" = "Pine","30 - 70% Pine" = "Mixed","< 30% Pine" = "Hardwood")) +
  xlab("Forest Type") +
  ylab("Canopy Cover %") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
lol2 <- lm(data = Master, Avg_CC~group)

summary(lol2)
TukeyHSD(aov(lol2))
cc <- ggplot(Master, aes(x=group, y = Avg_CC)) +
  geom_boxplot(aes(y=Avg_CC)) +
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Canopy Cover (%)") +
  ylab("Canopy Cover (%)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#herb ht
cp1 <- ggplot(Master, aes(x=group, y = Avg_Herb_Ht)) +
  geom_boxplot(aes(y=Avg_Herb_Ht)) +
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Herbaceous Height (cm)") +
  ylab("Herbaceous Height (cm)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#herb cover
cp2 <- ggplot(Master, aes(x=group, y = Avg_Herb_Live)) +
  geom_boxplot(aes(y=Avg_Herb_Live)) +
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Herbaceous Cover (%)") +
  ylab("Herbaceous Cover (%)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#woody ht
cp3 <- ggplot(Master, aes(x=group, y = Avg_Woody_Ht)) +
  geom_boxplot(aes(y=Avg_Woody_Ht)) +
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Woody Height (cm)") +
  ylab("Woody Height (cm)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#woody cover
cp4 <- ggplot(Master, aes(x=group, y = Avg_Woody_Live)) +
  geom_boxplot(aes(y=Avg_Woody_Live)) +
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Woody Cover (%)") +
  ylab("Woody Cover (%)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#fwd
cp5 <- ggplot(Master, aes(x=group, y = FWD_Mgha)) +
  geom_boxplot(aes(y=FWD_Mgha)) +
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Fine Woody Debris (Mgha)") +
  ylab("Fine Woody Debris (Mgha)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#cwd
cp6 <- ggplot(Master, aes(x=group, y = CWD_Mgha)) +
  geom_boxplot(aes(y=CWD_Mgha)) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Coarse Woody Debris (Mgha)") +
  ylab("Coarse Woody Debris (Mgha)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#litter depth
cp7 <- ggplot(Master, aes(x=group, y = Avg_Depth_cm)) +
  geom_boxplot(aes(y=Avg_Depth_cm)) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Litter Depth (cm)") +
  ylab("Litter Depth (cm)") +
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#duff depth
cp8 <- ggplot(Master, aes(x=group, y = Avg_Duff_cm)) +
  geom_boxplot(aes(y=Avg_Duff_cm)) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Duff Depth (cm)") + 
  ylab("Duff Depth (cm)") + 
  geom_signif(comparisons = mycomps1, map_signif_level = T)


#seedling density
test45 <- left_join(sapseedarea,Master, by = "Plot")
cp9 <- ggplot(seedgroup %>% filter(!is.na(Functional)), aes(x=group, y = Seedlings_100m2, fill = Functional)) +
  geom_boxplot(aes(y=Seedlings_100m2)) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ggtitle("log(seedlings per 100m2)") +
  labs(y="log(Seedlings per 100m2)",
       x = "Forest Type",
       color = "Species Group") +
  scale_fill_manual(labels = c("#N/A","Encroaching", "Pine", "Upland oak"), 
                     values = c("#FFFFFF","#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_signif(comparisons = mycomps2, map_signif_level = F)



mycomps1 <- list(c("30 - 70% Pine",">70% Pine"), c("< 30% Pine",">70% Pine"),
                 c("30 - 70% Pine", "< 30% Pine"))

mycomps2 <- list(c("Pine","Upland oak"), c("Pine","Encroaching"),
                c("Encroaching", "Upland oak"))

seedcalc_hl <- seedcalc %>% 
  filter(Functional=="pine", Seedlings_100m2 > 7 )


#sapling density

cp10 <- ggplot(sapgroup, aes(x=group, y = Saplings_100m2, fill = Functional)) +
  geom_boxplot(aes(y=Saplings_100m2)) +
theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ggtitle("log(Saplings per 100m2)") +
  labs(y="log(Saplings per 100m2)",
       x = "Forest Type",
       color = "Species Group") +  scale_fill_manual(labels = c("#N/A","Encroaching", "Pine", "Upland oak"), 
                     values = c("#FFFFFF","#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))


#cones
cp11 <- ggplot(Master, aes(x=group, y = Avg_Cones)) +
  geom_boxplot(aes(y=Avg_Cones)) +
  theme_bw() +
  geom_jitter(aes(y=Avg_Cones), width = 0.1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Cones per m2") + 
  ylab("Cones per m2") + 
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#acorns
cp12 <- ggplot(Master, aes(x=group, y = Avg_Acorns)) +
  geom_boxplot(aes(y=Avg_Acorns)) +
  theme_bw() +
  geom_jitter(aes(y=Avg_Acorns), width = 0.1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Acorns per m2") + 
  ylab("Acorns per m2") + 
  geom_signif(comparisons = mycomps1, map_signif_level = T)
#gum balls
cp13 <- ggplot(Master, aes(x=group, y = Avg_Balls)) +
  geom_boxplot(aes(y=Avg_Balls)) +
  theme_bw() +
  geom_jitter(aes(y=Avg_Balls), width = 0.1) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank()) +
  ggtitle("Sweetgum Balls per m2") + 
  ylab("Sweetgum Balls per m2") + 
  geom_signif(comparisons = mycomps1, map_signif_level = T)

eightgrid <- plot_grid(plot = cp1,cp2,cp3,cp4,cp5,cp6,cp7,cp8, ncol = 4)

sapseedgrid <- plot_grid(plot = cp9, cp10, ncol = 2)

structuresgrod <- plot_grid(plot = cp11,cp12,cp13, ncol = 3)

pine <- litter %>% 
  filter(Little_type=="Pinus")
uo <- litter %>% 
  filter(Little_type=="Upland oak")
meso <- litter %>% 
  filter(Little_type=="Mesophyte")

pinereg <- lm(data = pine, Pct_wt ~ Pine_pctBAft2a)
summary(pinereg)
confint(pinereg)

plot(data = pine, Pct_wt~Pine_pctBAft2a)
abline(pinereg)

For each 10 basal area increase in pine, we observed a 3.317 percent
weight increase in pine leaf litter (p=2.69e-14, r2 = 0.4583)

For each 10 percent increase in pine basal area, we observed a 6.94 percent 
increase in pine leaf litter within the fuel bed (p=1.1e-12, r2 = 0.4149) 


uoreg <- lm(data = uo, Pct_wt ~ Pine_pctBAft2a)
summary(uoreg)
confint(uoreg)

plot(data = uo, Pct_wt~Pine_pctBAft2a)
abline(uoreg)


sapgroup <- left_join(sapcalc,Master, by = "Plot")
seedgroup <- left_join(seedcalc,Master, by = "Plot")


#work on total fuel loads 
#leaf litter, litter bed, duff, herbs, woody, fwd, cwd

Master

herbcalcs <- herbs %>% 
  group_by(Site, Plot, Direction) %>% 
  summarise(Biomass_Herb1 = (Herb_Height/100)*(Live_Herbs/100)*0.8,
            Biomass_Shrub1 = (Woody_Height/100)*(Live_Woody/100)*1.8)

herbcalcsplot <-  herbcalcs %>% 
  group_by(Site, Plot) %>% 
  summarise(Biomass_Herb = mean(Biomass_Herb1),
            Biomass_Shrub = mean(Biomass_Shrub1))

depthduff <- slopeduff %>% 
  group_by(Site, Plot) %>% 
  summarise(Bio_de1 = (de1/100)*44.051,
            Bio_de2 = (de2/100)*44.051,
            Bio_du1 = (du1/100)*88.102,
            Bio_du2 = (du2/100)*88.102)

biodepthduff <- depthduff %>% 
  group_by(Site,Plot) %>% 
  summarise(Biomass_Litter = (Bio_de1+Bio_de2)/2,
            Biomass_Duff = (Bio_du1+Bio_du2)/2)

usloads <- left_join(herbcalcsplot,biodepthduff)

#write_csv(usloads, "data/processed_data/understoryloads.csv")

usloads <- read_csv("data/processed_data/understoryloads.csv")
usloads$group <- factor(usloads$group, levels = c("Pine","Mixed","Hardwood"))
usloads$Biomass_Load_Source <- factor(usloads$Biomass_Load_Source, levels = c("Litter", "Duff", "Herbaceous", "Shrubs", "FWD", "CWD"))
usloadsgroup <- usloads %>% 
  group_by(group, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2))
cbbPalette <- c("#FF0000", "#FF9999", "#FFCC99", "#CC99FF", "#0033FF", "#999999")

usloadssite <- usloads %>% 
  group_by(Site, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2))

allloads <- ggplot(usloadsgroup, aes(x = group, y = Load_kgm2))+
  geom_bar(position = "stack", stat = "identity", aes(fill = Biomass_Load_Source)) +
  theme_bw() + 
  labs(y = ~Biomass~ kg/m^2) +
  xlab("Forest Type") +
  scale_fill_manual(name = "Biomass_Load_Source", values =cbbPalette) +
  guides(fill=guide_legend(title="Biomass Source")) 
  #ggtitle("Biomass of fuel sources across Pine, Mixed, and Hardwood Forests")
ggsave(plot = allloads, "figures/biomass_allfuelsources.png", height = 6, width = 9)
filter(usloads$group=="< 30% Pine")



res1 <- lm(data = Master, Avg_CC~group)
summary(res1)
TukeyHSD(aov(res1))
unique(usloads$Biomass_Load_Source)

herby <- filter(usloads, Biomass_Load_Source=="Herbaceous")           
shrubs <- filter(usloads, Biomass_Load_Source=="Shrubs")   
litty <- filter(usloads, Biomass_Load_Source=="Litter")   
duffy <- filter(usloads, Biomass_Load_Source=="Duff")   
fwd <- filter(usloads, Biomass_Load_Source=="FWD")   
cwd <- filter(usloads, Biomass_Load_Source=="CWD")   

res1 <- lm(data = herby, Load_kgm2~Site)
summary(res1)
anova(res1)
tuk1 <- TukeyHSD(aov(res1))
cld1 <- multcompLetters4(res1, tuk1)


res2 <- lm(data = shrubs, Load_kgm2~Site)
summary(res2)
anova(res2)
tuk2 <- TukeyHSD(aov(res2))
cld2 <- multcompLetters4(res2, tuk2)

res3 <- lm(data = litty, Load_kgm2~Site)
summary(res3)
anova(res3)
tuk3 <- TukeyHSD(aov(res3))
cld3 <- multcompLetters4(res3, tuk3)

res4 <- lm(data = duffy, Load_kgm2~Site)
summary(res4)
anova(res4)
tuk4 <- TukeyHSD(aov(res4))
cld4 <- multcompLetters4(res4, tuk4)


res5 <- lm(data = fwd, Load_kgm2~Site)
summary(res5)
anova(res5)
tuk5 <- TukeyHSD(aov(res5))
cld5 <- multcompLetters4(res5, tuk5)


res6 <- lm(data = cwd, Load_kgm2~Site)
summary(res6)
anova(res6)
tuk6 <- TukeyHSD(aov(res6))
cld6 <- multcompLetters4(res6, tuk6)


res6.1 <- lm(data = cwd, Load_kgm2~Site)
summary(res6.1)
anova(res6.1)
TukeyHSD(aov(res6.1))
anova(res6,res6.1)
n = 97
Master

duffy %>% 
  summarise(cc = mean(Load_kgm2),
            se = sd(Load_kgm2)/(sqrt(n)))


###### for canopy groups

res1 <- lm(data = herby, Load_kgm2~group)
summary(res1)
anova(res1)
tuk1 <- TukeyHSD(aov(res1))
cld1 <- multcompLetters4(res1, tuk1)


res2 <- lm(data = shrubs, Load_kgm2~group)
summary(res2)
anova(res2)
tuk2 <- TukeyHSD(aov(res2))
cld2 <- multcompLetters4(res2, tuk2)

res3 <- lm(data = litty, Load_kgm2~group)
summary(res3)
anova(res3)
tuk3 <- TukeyHSD(aov(res3))
cld3 <- multcompLetters4(res3, tuk3)

res4 <- lm(data = duffy, Load_kgm2~group)
summary(res4)
anova(res4)
tuk4 <- TukeyHSD(aov(res4))
cld4 <- multcompLetters4(res4, tuk4)


res5 <- lm(data = fwd, Load_kgm2~group)
summary(res5)
anova(res5)
tuk5 <- TukeyHSD(aov(res5))
cld5 <- multcompLetters4(res5, tuk5)


res6 <- lm(data = cwd, Load_kgm2~group)
summary(res6)
anova(res6)
tuk6 <- TukeyHSD(aov(res6))
cld6 <- multcompLetters4(res6, tuk6)

#### for thesis big fuel comp

litter <-read_csv("data/processed_data/litterxba_mesossummed.csv")
newtest <- c("#FF0000", "#FF9999", "#FFCC99")
litter$Little_type <- factor(litter$Little_type, levels = c("Pinus","Upland oak","Mesophyte"))


hardwood <- litter %>% 
  filter(group=="Hardwood")

mixed <- litter %>% 
  filter(group=="Mixed")

pine <- litter %>% 
  filter(group=="Pine")


t1 <- ggplot(hardwood, aes(x = Pine_pctBAft2a, y = Pct_wt)) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Litter type by mass (%)") +
  xlab("Pine Basal Area (%)") + 
  theme_bw() +
  labs(title = "Hardwood Stand", color = "Litter Type") +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 22.5, y = 100, label = "n = 8", fontface = 2) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none", axis.title.y = element_blank())
  
  
t2 <- ggplot(mixed, aes(x = Pine_pctBAft2a)) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Litter type by mass (%)") +
  xlab("Pine Basal Area (%)") + 
  theme_bw() +
  labs(title = "Mixed Stand", color = "Litter Type") +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 50, y = 100, label = "n = 68", fontface = 2) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none", axis.title.y = element_blank())

t3 <- ggplot(pine, aes(x = Pine_pctBAft2a)) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Litter type by mass (%)") +
  xlab("Pine Basal Area (%)") + 
  theme_bw() +   labs(title = "Pine Stand", color = "Litter Type") +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 85, y = 100, label = "n = 21", fontface = 2) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none")

legend <- get_legend(
  
  t1 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

t5 <- cowplot::plot_grid(t3, t2, t1,
                         ncol = 3)
final_pineba <- plot_grid(t5, legend, ncol = 1, rel_heights = c(1, .1))

#ggsave(plot = final_pineba, filename = "figures/litterxpineba.png", height = 4.2, width = 11)


t5 <- cowplot::plot_grid(t1 + theme(axis.title.x = element_blank()), 
                         t2 + theme(axis.title.x = element_blank()),
                         t3 + theme(axis.title.x = element_blank()),
                         t4 + theme(axis.title.x = element_blank()),
                         ncol = 4)

t7 <- ggplot(litter, aes(x = Pine_pctBAft2a)) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Litter type by mass (%)") +
  xlab("Pine Basal Area (%)") + 
  theme_bw() +   labs(color = "Litter Type") +
  scale_y_continuous(limits = c(0,100)) +
  theme(legend.position = "bottom")


pinuslitter <- lm(data=mixed, Pct_wt~Pine_pctBAft2a + Little_type)
summary(pinuslitter)
confint(pinuslitter)
TukeyHSD(aov(pinuslitter))
anova(pinuslitter,model2)
lol23 <- litter %>% 
  filter(Little_type=="Mesophyte")
car::vif(pinuslitter)

tuk7 <- TukeyHSD(aov(pinuslitter))
cld7 <- multcompLetters4(pinuslitter, tuk7)


#testing longleaf and other pine things

mixed_trees <- read_csv("data/raw_data/Mixed Stands/MixedStand_Trees.csv")


dbh_cm_in <- mixed_trees %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(dbh_cm = DBH,
            dbh_in = (DBH)*0.393701)

basal_area_m2_ft2 <- dbh_cm_in %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(ba_m2 = dbh_cm^2*(0.00007854),
            ba_ft2 = dbh_in^2*(0.005454))

allbutlongleaf <- basal_area_m2_ft2 %>% 
  filter(Genus=="Pinus") %>% 
  filter(Species!="PIPA")

longleaf <- basal_area_m2_ft2 %>% 
  filter(Species=="PIPA")

abl_ba <- allbutlongleaf %>% 
  group_by(Plot,Site) %>% 
  summarise(otherpinesba = sum(ba_ft2)/0.05)

ll_ba <- longleaf %>% 
  group_by(Plot,Site) %>% 
  summarise(longleafba = sum(ba_ft2)/0.05)

llxlitter <- left_join(ll_ba, litter) %>% 
  group_by(Site, Plot, group, Little_type, Pct_wt, Load_wt) %>% 
  summarise(longleafpctba = longleafba/BA_ft2a*100)

ablxlitter <- left_join(abl_ba, litter) %>% 
  group_by(Site, Plot, group, Little_type, Pct_wt, Load_wt) %>% 
  summarise(otherpinespctba = otherpinesba/BA_ft2a*100)


longleaf <- ggplot(llxlitter, aes(x = longleafpctba)) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Litter type by mass (%)") +
  xlab("Longleaf Pine Basal Area (%)") + 
  theme_bw() +   labs(color = "Litter Type") +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 50, y = 100, label = "n = 28", fontface = 2) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none")

otherpines <- ggplot(ablxlitter, aes(x = otherpinespctba)) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Litter type by mass (%)") +
  xlab("Other Pine Basal Area (%)") + 
  theme_bw() +   labs(color = "Litter Type") +
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 50, y = 100, label = "n = 93", fontface = 2) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none")
t10 <- cowplot::plot_grid(longleaf, otherpines,
                         ncol = 2)
final_pineba <- plot_grid(t10, legend, ncol = 1, rel_heights = c(1, .1))

lltest <- llxlitter %>% 
  filter(Little_type=="Pinus")
long <- lm(data = lltest, Pct_wt ~ longleafpctba)
summary(long)
TukeyHSD(aov(long))
alltest <- ablxlitter %>% 
  filter(Little_type=="Pinus")
long2 <- lm(data = alltest, Pct_wt ~ otherpinespctba)
summary(long2)
TukeyHSD(aov(long2))

