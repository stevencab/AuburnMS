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
            Saplings_100m2 = log((sum_count/3.6)*100))

seedcalc <- seed %>% 
  group_by(Site, Plot, Functional) %>%
  summarise(sum_count = sum(Count),
            Seedlings_100m2 = log((sum_count/1.13)*100))

#more sap seed for transfer to master for PCA

sapseedsums <- sapseed %>% 
  group_by(Site, Plot, Type) %>% 
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

ggplot(litter, aes(x=Pine_pctBAft2a, y=Pct_wt)) +
  geom_point(aes(color = Little_type)) +
  geom_smooth(method = "lm", aes(color = Little_type), se = F) +
  ylab("Leaf liter fuel composition by mass (%)") +
  xlab("Pine Basal Area (%)")

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
ggplot(Master, aes(x=group)) +
  geom_boxplot(aes(y=Avg_CC)) +
  theme_bw()
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

cp9 <- ggplot(seedgroup, aes(x=group, y = Seedlings_100m2, color = Functional)) +
  geom_boxplot(aes(y=Seedlings_100m2)) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ggtitle("log(seedlings per 100m2)") +
  ylab("log(Seedlings per 100m2") +
  geom_signif(comparisons = mycomps2, map_signif_level = T)


mycomps1 <- list(c("30 - 70% Pine",">70% Pine"), c("< 30% Pine",">70% Pine"),
                 c("30 - 70% Pine", "< 30% Pine"))

mycomps2 <- list(c("pine","upland oak"), c("pine","mesophyte"),
                c("mesophyte", "upland oak"))

seedcalc_hl <- seedcalc %>% 
  filter(Functional=="pine", Seedlings_100m2 > 7 )


#sapling density

cp10 <- ggplot(sapgroup, aes(x=group, y = Saplings_100m2, color = Functional)) +
  geom_boxplot(aes(y=Saplings_100m2)) +
theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ggtitle("log(saplings per 100m2)") +
  ylab("log(Saplings per 100m2") +  
  geom_signif(comparisons = mycomps2, map_signif_level = T)


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
usloads$group <- factor(usloads$group, levels = c(">70% Pine","30 - 70% Pine","< 30% Pine"))
usloads$Biomass_Load_Source <- factor(usloads$Biomass_Load_Source, levels = c("FWD", "CWD", "Herbaceous", "Shrubs", "Litter", "Duff"))
usloadsgroup <- usloads %>% 
  group_by(group, Biomass_Load_Source) %>% 
  summarise(Load_kgm2 = mean(Load_kgm2))
cbbPalette <- c("#FF0000", "#FF9999", "#FFCC99", "#CC99FF", "#0033FF", "#999999")


allloads <- ggplot(usloadsgroup, aes(x = group, y = Load_kgm2))+
  geom_bar(position = "stack", stat = "identity", aes(fill = Biomass_Load_Source)) +
  theme_bw() + 
  labs(y = ~Biomass~ kg/m^2) +
  xlab("Stand Type") +
  scale_fill_manual(name = "Biomass_Load_Source", values =cbbPalette) +
  guides(fill=guide_legend(title="Biomass Source")) +
  ggtitle("Biomass of fuel sources across Pine, Mixed, and Hardwood Forests")
ggsave(plot = allloads, "figures/biomass_allfuelsources.png", height = 6, width = 9)
filter(usloads$group=="< 30% Pine")

