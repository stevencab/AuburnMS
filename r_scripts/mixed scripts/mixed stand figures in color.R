####thesis final mixed stand analysis for pub IN COLOR

library(ggplot2)
library(readr)
library(dplyr)
library(cowplot)
library(ggsignif)
library(factoextra)
library(nlme)
library(ggpubr)
library(emmeans)

Master <- read_csv("data/processed_data/MixedStand_Master.csv")
mixed_trees <- read_csv("data/raw_data/Mixed Stands/MixedStand_Trees.csv")
usloads_short <- read_csv("data/raw_data/Mixed Stands/understoryloads_short.csv")
usloads <- read_csv("data/processed_data/understoryloads.csv")
litter <- read_csv("data/processed_data/litterxba_mesossummed.csv")
littershort <- read_csv("data/processed_data/litterba_short.csv")

cbbPalette <- c("#FF0000", "#FF9999", "#FFCC99", "#CC99FF", "#0033FF", "#999999")
newtest <- c("#FF0000", "#FF9999", "#FFCC99")

Master # this is literally everything raw data
NOT THIS FOR TREES MAKE SURE ITS MIXED STAND TREES!! # this is all trees
  usloads #this is for figure making only
usloads_short #this is for data analysis
litter #this is for figure making only
litter_short #this is for data analysis

usloads_short <- read_csv("data/raw_data/Mixed Stands/understoryloads_short.csv")

usloads_short


mixed_trees <- read_csv("data/raw_data/Mixed Stands/MixedStand_Trees.csv")
# remove plot 71 cus its a straight pine planatation, not what we want

mixed_trees <- mixed_trees %>% 
  filter(Plot!=71) 



test <- mixed_trees %>% 
  group_by(Site, Plot, tert) %>% 
  summarise(TPHA = n_distinct(Stem_id)*50,
            QMD = sqrt((sum((DBH)^2))/n_distinct(Stem_id)))

ggplot(test, aes(x = Mean_DBH, y = TPA, 
                 color = Functional, fill = Functional,
                 width = 1)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~poly(x,3)", SE = F)

test$Functional <- factor(test$Functional, levels = 
                            c("other","Red maple","Tulip poplar",
                              "water oak","Sweetgum", "upland oak", "Pinus"))

ggplot(test, aes(width = 0.8, color = Functional)) +
  geom_histogram(aes(x=Mean_DBH,y=TPA, fill = Functional),stat="identity",
                 position = "jitter",binwidth = 6,)

ggplot(test2, aes(width = 0.8, fill = Functional)) +
  geom_histogram(aes(x=TPA, fill = Functional), color = "black",binwidth = 50)

t1 + scale_fill_gradient2(low="red",high="blue",mid="white")
width = 0.5)

res1 <- lm(data=test, TPHA~QMD*secondary)
summary(res1)
ggplot(test, aes(x=QMD,y=TPHA, color = secondary)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) +
  theme_bw()

test2 <- test %>% 
  mutate(logTPHA = log(TPHA),
         logQMD = log(QMD))

ggplot(test2, aes(x=logQMD,y=logTPHA, color = tert)) +
  geom_point(position = "jitter", size =2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = F) +
  theme_bw() +
  geom_vline(xintercept=2) +
  labs(x = "logQMD (cm)",
       y = "logTPHA",
       color="Species Group")


ggplot(test3, aes(x=QMD, fill = tert)) +
  geom_bar(stat="bin", position = "stack", color = "black",
           binwidth = 7) +
  theme_bw() +
  facet_wrap(~group)
ggplot(test3, aes(x=TPHA, fill = tert)) +
  geom_bar(stat="bin", position = "stack", color = "black",
           binwidth = 75) +
  theme_bw() +
  facet_wrap(~Site.x)

geom_smooth(method="lm", SE = F) +
  theme_bw() + 
  
  
  #models#
  
  sitevar <- lm(Litter~Site+group, data = usloads_short)
summary(sitevar)
TukeyHSD(aov(sitevar))



test3 <- inner_join(test2,Master, by="Plot")
test4 <- inner_join(test,Master, by="Plot")


#overall ba and pine ba figures

Master_figs <- Master %>% 
  filter(Plot!=c(99)) %>% 
  filter(Plot!=c(65)) %>% 
  filter(Plot!=c(19))  
Master_figs$Site[Master_figs$Site=="CR"] <- "CRAT"
Master_figs$Site[Master_figs$Site=="MOT"] <- "MOTDF"
Master_figs$Site[Master_figs$Site=="Talladega"] <- "TANF"
Master_figs$Site[Master_figs$Site=="Tuskegee"] <- "TUNF"
Master_figs$Site[Master_figs$Site=="Kreher"] <- "KNP"

f1 <- ggplot(Master_figs, aes(x = BA_m2ha, fill = Site)) +
  geom_histogram(stat="bin", color = "black",binwidth = 4) +
  theme_bw() +
  labs(x= expression(paste("Total stand basal area m"^2~ha^{~-1})),
       y= "Number of stands") +
  theme(legend.position = "none") + 
  scale_fill_grey(start = 0.35, end =0.9)

f2 <- ggplot(Master, aes(x = Pine_pctBAft2a, fill = Site)) +
  geom_histogram(stat="bin", color = "black",binwidth = 4) +
  theme_bw() +
  geom_vline(xintercept = 30, linetype = "longdash", size = 1.5) +
  geom_vline(xintercept = 70, linetype = "longdash", size = 1.5) +
  xlab("Relative pine basal area (%)")+
  ylab("Number of stands") +
  annotate("text", x = 20, y = 8.8, label =  "Hardwood Forest\n n = 8", fontface = 2, size = 8) +
  annotate("text", x = 50, y = 8.8, label =  "Mixed Forest\n n = 68" , fontface = 2, size = 8) +
  annotate("text", x = 89, y = 8.8, label =  "Pine Forest\n n = 21" , fontface = 2, size = 8) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) 

+
  scale_fill_manual(values = c(cbbPalette))
  scale_fill_grey(start = 0.35, end = 0.9)



legend <- get_legend(
  
  f2 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f3 <- cowplot::plot_grid(f1,f2,
                         ncol = 2)  

f4 <- plot_grid(f2, legend, ncol = 1, rel_heights = c(1, .1))

# QMD and TPHA figures

test3

f5 <- ggplot(test3, aes(x = QMD, fill = tert)) +
  geom_histogram(stat="bin", color = "black",binwidth = 4) +
  theme_bw() +
  xlab("Stand quadratic mean diameter (cm)")+
  ylab("Number of stands") +
  labs(fill="Species Group") +
  #annotate("text", x = 20, y = 8.5, label =  "Hardwood Forest\n n = 8", fontface = 2, size = 4) +
  #annotate("text", x = 50, y = 8.5, label =  "Mixed Forest\n n = 68" , fontface = 2, size = 4) +
  #annotate("text", x = 89, y = 8.5, label =  "Pine Forest\n n = 21" , fontface = 2, size = 4) +
  theme(legend.position = "none") +
  scale_fill_manual(labels = c("Encroaching", "Pine", "Upland oak"), 
                     values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_x_continuous(limits=c(0,80))
f6 <- ggplot(test3, aes(x = TPHA, fill = tert)) +
  geom_histogram(stat="bin", color = "black",binwidth = 50) +
  theme_bw() +
  xlab(expression(paste("Density (trees per hectare)")))+
  ylab("Number of stands") +
  labs(fill="Species Group") +
  #annotate("text", x = 20, y = 8.5, label =  "Hardwood Forest\n n = 8", fontface = 2, size = 4) +
  #annotate("text", x = 50, y = 8.5, label =  "Mixed Forest\n n = 68" , fontface = 2, size = 4) +
  #annotate("text", x = 89, y = 8.5, label =  "Pine Forest\n n = 21" , fontface = 2, size = 4) +
  theme(legend.position = "none") +
  scale_fill_manual(labels = c("Encroaching", "Pine", "Upland oak"), 
                     values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  scale_x_continuous(limits=c(0,900))

legend2 <- get_legend(
  
  f5 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f7 <- plot_grid(f5,f6,
                ncol = 2)  

f8 <- plot_grid(f7, legend2, ncol = 1, rel_heights = c(1, .1))

## fuel loads 

usloads_short

#make to Mg ha-1
usloads$group[usloads$group=="Hardwood"] <- "Hardwood Forest"
usloads$group[usloads$group=="Mixed"] <- "Mixed Forest"
usloads$group[usloads$group=="Pine"] <- "Pine Forest"
usloads$Biomass_Load_Source <- factor(usloads$Biomass_Load_Source, 
                                      levels = c("FWD","CWD", "Herbaceous", "Shrubs","Duff", "Litter"))
usloads$Site[usloads$Site=="CR"] <- "CRAT"
usloads$Site[usloads$Site=="MOT"] <- "MOTDF"
usloads$Site[usloads$Site=="Talladega"] <- "TANF"
usloads$Site[usloads$Site=="Tuskegee"] <- "TUNF"
usloads$Site[usloads$Site=="Kreher"] <- "KNP"

usloads_mghagroup <- usloads %>% 
  group_by(Biomass_Load_Source,group) %>% 
  summarise(Mean_Load_Mgha = mean(Load_kgm2*10))
usloads_mghasite <- usloads %>% 
  group_by(Biomass_Load_Source,Site) %>% 
  summarise(Mean_Load_Mgha = mean(Load_kgm2*10))

f9 <- ggplot(usloads_mghasite, aes(x=Site,y=Mean_Load_Mgha, fill=Biomass_Load_Source)) +
  geom_bar(stat = "identity",position="stack", color = "black") +
  theme_bw() +
  labs(x="Site",
       y=expression(paste("Mean fuel load  Mg ha"^~-1)),
       fill="Fuel load type") +
  scale_fill_grey(start = 0.22, end = 0.9) +
  theme(legend.position = "none")

f10 <- ggplot(usloads_mghagroup, aes(x=group,y=Mean_Load_Mgha, fill=Biomass_Load_Source)) +
  geom_bar(stat = "identity",position="stack", color = "black") +
  theme_bw() +
  labs(x="Forest Type",
       y=expression(paste("Mean fuel load  Mg ha"^~-1)),
       fill="Fuel load type") +
  scale_fill_brewer(palette = "YlOrBr") +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  theme(legend.position = "none")
  

(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
  legend.text = element_text(size = 12)) 


legend3 <- get_legend(
  
  f10 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
              legend.text = element_text(size = 12))  +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f11 <- plot_grid(f9,f10,
                 ncol = 2)  

f12 <- plot_grid(f10, legend3, ncol = 1, rel_heights = c(1, .1))

#canopy cover

Master
Master$group[Master$group=="< 30% Pine"] <- "Hardwood Forest"
Master$group[Master$group=="30 - 70% Pine"] <- "Mixed Forest"
Master$group[Master$group==">70% Pine"] <- "Pine Forest"
Master$Site[Master$Site=="CR"] <- "CRAT"
Master$Site[Master$Site=="MOT"] <- "MOTDF"
Master$Site[Master$Site=="Talladega"] <- "TANF"
Master$Site[Master$Site=="Tuskegee"] <- "TUNF"
Master$Site[Master$Site=="Kreher"] <- "KNP"

my_comparisons1 <- list( c("CRAT", "MOTDF"),
                         c("MOTDF", "TANF"),
                         c("CRAT", "TANF"),
                         c("TANF","TUNF"),
                         c("TUNF","CRAT"),
                         c("TUNF","MOTDF"),
                         c("KNP","MOTDF"),
                         c("KNP","TUNF"),
                         c("KNP","CRAT"),
                         c("KNP","TANF"))
my_comparisons2 <- list( c("Hardwood Forest", "Mixed Forest"),
                         c("Pine Forest", "Hardwood Forest"),
                         c("Mixed Forest", "Pine Forest") )

f13 <- ggplot(Master, aes(x=Site,y=Avg_CC, fill = Site)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="\nSite",
       y= "Mean canopy cover (%)") +
  #stat_compare_means(comparisons = my_comparisons1) +
  scale_fill_grey(start = 0.22, end = 0.9) +
  theme(legend.position = "none")

f14 <- ggplot(Master, aes(x=group,y=Avg_CC, fill = group)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="\nForest Type",
       y= "Mean canopy cover (%)") +
  stat_compare_means(comparisons = my_comparisons2, hide.ns = T) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  theme(legend.position = "none")

legend4 <- get_legend(f13 + theme(legend.position = "none"))

f15 <- plot_grid(f13,f14,
                 ncol = 2)  

f16 <- plot_grid(f15, legend4, ncol = 1, rel_heights = c(1, .1))



# litter across site and forest type

litter$group[litter$group=="Hardwood"] <- "Hardwood Forest"
litter$group[litter$group=="Mixed"] <- "Mixed Forest"
litter$group[litter$group=="Pine"] <- "Pine Forest"
litter$Little_type[litter$Little_type=="Mesophyte"] <- "Encroaching"
litter$Little_type[litter$Little_type=="Pinus"] <- "Pine"

litter$Little_type <- factor(litter$Little_type, 
                             levels = c("Encroaching",
                                        "Pine",
                                        "Upland oak"))
litter$Site[litter$Site=="CR"] <- "CRAT"
litter$Site[litter$Site=="MOT"] <- "MOTDF"
litter$Site[litter$Site=="Talladega"] <- "TANF"
litter$Site[litter$Site=="Tuskegee"] <- "TUNF"
litter$Site[litter$Site=="Kreher"] <- "KNP"


f17 <- ggplot(litter, aes(x=Site,y=Pct_wt, fill = Little_type)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="\nSite",
       y= "Percent leaf litter by mass (%",
       fill = "Leaf Litter Group") +
  #stat_compare_means(comparisons = my_comparisons1) +
  scale_fill_grey(start = 0.35, end = 0.9) +
  theme(legend.position = "none")


f18 <- ggplot(litter, aes(x=group,y=Pct_wt, fill = Little_type)) +
  geom_boxplot() +
  theme_bw() +
  labs(x="\nForest Type",
       y= "Percent leaf litter by mass (%)", 
       fill = "Leaf Litter Group") +
  #stat_compare_means(comparisons = my_comparisons) +
  scale_fill_brewer(palette = "YlOrBr") +
  theme(axis.title=element_text(size=14)) +
  theme(legend.position = "none")

legend5 <- get_legend(
  
  f18 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
              legend.text = element_text(size = 12)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f19 <- plot_grid(f17,f18,
                 ncol = 2)  

f20 <- plot_grid(f18, legend5, ncol = 1, rel_heights = c(1, .1))

## litter regression figs

littershort <- read_csv("data/processed_data/litterba_short.csv")
littershort$Little_type <- factor(littershort$Little_type, 
                                  levels = c("Encroaching",
                                             "Pine",
                                             "Upland oak"))

ggplot(litter, aes(x=Pine_pctBAft2a,y=Pct_wt,color=Little_type))+
  geom_point(size = 1.5) +
  geom_smooth(method="lm", se=F)

littershort <- as.factor(littershort$Plot)

#for comparing boxplots
litres <- lm(data=littershort,total_load~Site*group)
summary(litres)
confint(litres)
TukeyHSD(aov(litres))

#for linear regs
litres2 <- lm(data=littershort,encr_pct~Pine_pctBAft2a)
summary(litres)
litres3 <- lm(data=littershort,pine_load~Pine_ft2a)
summary(litres3)

f21 <- ggplot(litter, aes(x=Pine_pctBAft2a,y=Pct_wt,color=Little_type,
                          shape = Little_type))+
  geom_smooth(method="lm", se=F, size = 2) +
  theme_bw() +
  geom_vline(xintercept = 30, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 70, linetype = "dashed", size = 1) +
  annotate("text", x = 18, y = 75, label =  "Hardwood Forest\n", fontface = 2, size = 8) +
  annotate("text", x = 50, y = 75, label =  "Mixed Forest\n" , fontface = 2, size = 8) +
  annotate("text", x = 85.5, y = 75, label =  "Pine Forest\n" , fontface = 2, size = 8) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                     limits=c(10,100)) +
  labs(x="Relative pine basal area (%)",
       y="Percent leaf litter by mass (%)",
       color="Species Group",
       shape="Species Group",
       size="Species Group") +
  geom_point(size = 4,stroke = 1.1, alpha = 0.4, position = "jitter") +
  scale_shape_manual(values=c(21,22,24)) +
  scale_color_manual(labels = c("Encroaching", "Pine", "Upland oak"), 
                     values = c("#FFFFFF","#FFFFFF","#FFFFFF")) +
  theme(axis.title=element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  theme(legend.position = "none")

legend6 <- get_legend(
  
  f21 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
              legend.text = element_text(size = 12)) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f22 <- plot_grid(f21, legend6, ncol = 1, rel_heights = c(1, .1))

#### figure for longleaf only vs other pines

dbh_cm_in <- mixed_trees %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(dbh_cm = DBH,
            dbh_in = (DBH)*0.393701)

basal_area_m2_ft2 <- dbh_cm_in %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(ba_m2 = dbh_cm^2*(0.00007854),
            ba_ft2 = dbh_in^2*(0.005454))

pine_ba <- basal_area_m2_ft2 %>% 
  filter(Genus=="Pinus") %>% 
  group_by(Plot, Site) %>% 
  summarise(pine_bam2 = sum(ba_m2)/0.05,
            pine_baft2 = sum(ba_ft2)/0.05)

longleaf_ba <- basal_area_m2_ft2 %>% 
  filter(Species=="PIPA") %>% 
  group_by(Plot, Site) %>% 
  summarise(longleaf_bam2 = sum(ba_m2)/0.05,
            longleaf_baft2 = sum(ba_ft2)/0.05)

otherpines_ba <- basal_area_m2_ft2 %>% 
  filter(Genus=="Pinus") %>% 
  filter(Species!="PIPA") %>% 
  group_by(Plot,Site) %>% 
  summarise(otherpine_bam2 = sum(ba_m2)/0.05,
            otherpine_baft2 = sum(ba_ft2)/0.05)

longleafx_total <- left_join(longleaf_ba,pine_ba) ### this is for calc LL/total ba
otherpinex_total <- left_join(otherpines_ba,pine_ba) ### this is for calc others/total ba

#LL
longleafx_total
longleaf_pct <- longleafx_total %>% 
  group_by(Plot,Site) %>% 
  summarise(longleafpct = longleaf_bam2/pine_bam2*100)
combinedpines_pct[is.na(combinedpines_pct)] <- 0 #fix na's for 100% LL plots

#### use this for making the longleaf figure, n=27 plots w/ LL
longleafxlitter <- left_join(longleafx_total, litter)

## other pines
otherpinex_total
otherpine_pct <- otherpinex_total %>% 
  group_by(Plot,Site) %>% 
  summarise(otherpinepct = otherpine_baft2 /pine_baft2*100)

#use this for making other pine figure n = 92
othersxlitter <- left_join(otherpinex_total, litter)

##figure making

longleafxlitter$Little_type[longleafxlitter$Little_type=="Mesophyte"] <- "Encroaching"
longleafxlitter$Little_type[longleafxlitter$Little_type=="Pinus"] <- "Pine"

longleafxlitter$Little_type <- factor(longleafxlitter$Little_type, 
                                      levels = c("Encroaching",
                                                 "Pine",
                                                 "Upland oak"))

othersxlitter$Little_type[othersxlitter$Little_type=="Mesophyte"] <- "Encroaching"
othersxlitter$Little_type[othersxlitter$Little_type=="Pinus"] <- "Pine"

othersxlitter$Little_type <- factor(othersxlitter$Little_type, 
                                    levels = c("Encroaching",
                                               "Pine",
                                               "Upland oak"))

f23 <- ggplot(longleafxlitter, aes(x = longleaf_bam2, shape = Little_type)) +
  geom_point(aes(y = Pct_wt, color = Little_type), size = 2, stroke = 1.1) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), size = 2,method = lm, se = F) +
  labs(y = "Litter type by mass (%)",
       x = expression(paste("Longleaf pine basal area m"^2~ha^{~-1})),
       color = "Species Group",
       shape = "Species Group") +
  theme_bw() +   
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 6.2, y = 87.5, label = "n = 27", fontface = 2, size = 8) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  scale_shape_manual(values=c(21,22,24)) +
  scale_color_manual(labels = c("Encroaching", "Pine", "Upland oak"), 
                     values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=14)) 


f24 <- ggplot(othersxlitter, aes(x = otherpine_bam2, shape = Little_type)) +
  geom_point(aes(y = Pct_wt, color = Little_type), size = 2, stroke = 1.1) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), size = 2,method = lm, se = F) +
  labs(y = "Litter type by mass (%)",
       x = expression(paste("Other pine species basal area m"^2~ha^{~-1})),
       color = "Species Group",
       shape = "Species Group") + 
  theme_bw() +  
  scale_y_continuous(limits = c(0,100)) +
  annotate("text", x = 10.84, y = 87.5, label = "n = 92", fontface = 2, size = 8) +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  scale_shape_manual(values=c(21,22,24)) +
  scale_color_manual(labels = c("Encroaching", "Pine", "Upland oak"), 
                     values = c("#00AFBB","#E7B800","#FC4E07")) +
  theme(axis.title=element_text(size=14))


legend7 <- get_legend(
  
  f23 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20),
              legend.text = element_text(size = 12)) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f25 <- plot_grid(f23,f24,
                 ncol = 2)  

f26 <- plot_grid(f25, legend7, ncol = 1, rel_heights = c(1, .1))

##### end figure making. f1-26 are figure making process


#begin lm analysis of everything ##### 
#start from the top

#stand descriptions of basal area and canopy cover

standchars <- Master %>% 
  summarise(stand_ba = mean(BA_m2ha),
            ba_se = std.error(BA_m2ha),
            pine_ba = mean(Pine_m2ha),
            pine_se = std.error(Pine_m2ha),
            pine_pct = mean(Pine_pctBAft2a),
            pine_pct = std.error(Pine_pctBAft2a))

mean(Master_figs$BA_m2ha)
Across all sites, mean stand basal area was 39.5 +/- 1.1.
The pine basal area was 22.5 +/- 0.958, and 
mean relative pine basal ara was 57.3 pct +/- 1.9 (Figure 2)

qmdandtpha <- test3 %>% 
  summarise(qmd_mean = mean(QMD),
            qmd_se = std.error(QMD),
            tpha_mean = mean(TPHA),
            tpha_se = std.error(TPHA)) %>% 
  summarise(qmd_mean2 = mean(qmd_mean),
            qmd_se2 = std.error(qmd_se),
            tpha_mean2 = mean(tpha_mean),
            tpha_se2 = std.error(tpha_se))


mean qmd all stands was 30.8 cm +/- 0.5 
mean tpha all stands was 263 tpha +/- 8.2

HOWEVER broken up into species categories

qmdandtpha2 <- test3 %>% 
  group_by(tert) %>% 
  summarise(qmd_mean = mean(QMD),
            qmd_se = std.error(QMD),
            tpha_mean = mean(TPHA),
            tpha_se = std.error(TPHA)) 

encroaching sp QMD 20.4 +/- 0.7
encroaching sp tpha 364 +/- 25

pine sp QMD 39.3 +/- 1.2
pine sp tpha 218 +/- 13

up oak QMD 30.6 +/- 1.6
up oak tpha 134 +/- 11

#this is a problem because........ oak bottle neck........
#size class and density disparity........... (in discussion)

#canopy cover
f14

Masterlogcc <- Master %>% 
  mutate(sqrtCC = sqrt(Avg_CC))

cclm <- lm(data=Master,Avg_CC~group*Site)

summary(cclm)
plot(cclm, 2)
car::leveneTest(Avg_CC~Site*group, data = Master)
aov_res <- residuals(object = cclm)
shapiro.test(x = aov_res)

cclm <- lm(data=Master,Avg_CC~Site*group)
hist(Master$Avg_CC)
hist(iris$Sepal.Length)
summary(cclm)
TukeyHSD(aov(cclm))

cclm2 <- lm(data=Masterlogcc,sqrtCC~group)
anova(cclm2,cclm)
summary(cclm2)
TukeyHSD(aov(cclm2))

ccmeans <- Master %>% 
  group_by(group) %>% 
  summarise(cc_mean = mean(Avg_CC),
            cc_se = std.error(Avg_CC))

among all sites, CC was 92.3 +/- 0.5.

hardwood forest had 93.1 +/- 1.9
mixed forest had 92.8 +/- 0.5
pine forest had 90.4 +/- 1.4


Due to larger sample sizes at 
between TUNF and MOTDF and TANF, we detected significant differences  (p < 0.05)
but effect size 5% differnce, are not meaning full as they are still
closed canopy forests.

there were no significant differences in canopy cover among 
forest types (p = 0.13), and the interaction of site*forest type 
was non-significant (0.25).


#fuel load differences 

usloads_short
logloads <- usloads_short %>% 
  mutate(logcwd = log(CWD+1.01),
         logfwd = log(FWD+1.01),
         logherbs = log(Herbs+1.01),
         logshrubs= log(Shrub+1.01),
         logduff = log(Duff+1.01),
         loglitter = log(Litter+1.01))

car::leveneTest(logherbs~Site*group, data = logloads)
hist(logloads$loglitter)
aov_res <- residuals(object = cwdtest)
shapiro.test(x = aov_res)


cwdtest <- lm(data=logloads, logcwd~Site*group)
fwdtest <- lm(data=logloads, logfwd~Site*group)
herbtest <- lm(data=logloads, logherbs~Site*group)
shrubtest <- lm(data=logloads, logshrubs~Site*group)
dufftest <- lm(data=logloads, logduff~Site*group)
littertest <- lm(data=logloads, loglitter~Site*group)

summary(cwdtest)
summary(fwdtest)
summary(herbtest)
summary(shrubtest)
summary(dufftest)
summary(littertest)

TukeyHSD(aov(cwdtest))
TukeyHSD(aov(fwdtest))
TukeyHSD(aov(herbtest))
TukeyHSD(aov(shrubtest))
TukeyHSD(aov(dufftest))
TukeyHSD(aov(littertest))
library(plotrix)
loads_sum <- usloads_short %>%
  group_by(group) %>% 
  summarise(mean_cwd = mean(CWD*10),
            se_cwd = std.error(CWD*10),
            mean_fwd = mean(FWD*10),
            se_fwd = std.error(FWD*10),
            mean_herb = mean(Herbs*10),
            se_herb = std.error(Herbs*10),
            mean_shrub = mean(Shrub*10),
            se_shrub = std.error(Shrub*10),
            mean_duff = mean(Duff*10),
            se_duff = std.error(Duff*10),
            mean_litter = mean(Litter*10),
            se_litter = std.error(Litter*10))

total_loads <- usloads_short %>% 
  group_by(Plot) %>% 
  summarise(plot_total = sum(CWD*10,FWD*10,Herbs*10,Shrub*10,Duff*10,Litter*10),
            se_total = std.error(sum(CWD,FWD,Herbs,Shrub,Duff,Litter)))

total_loads2 <- total_loads %>% 
  summarise(mean_total = mean(plot_total),
            se_total = std.error(plot_total))

#understory vegetation
ALL STANDS (Mg ha-1)
herbs: 0.32 +/- 0.04 DIFF IN TANF/TUNF/(MOT) P < 0.05 DUE TO SAMP SIZE
shrubs: 1.99 +/- 0.3 NO DIFF IN SITE OR FOREST TYPE
#coarse and fine woody debris
ALL STANDS (Mg ha-1)
coarse: 7.58 +/- 1.1 NO DIFF IN SITE OR FOREST TYPE
fine: 3.57 +/- 0.2 NO DIFF IN SITE OR FOREST TYPE

#litter and duff
ALL STANDS (Mg ha-1)
duff: 5.12 +/- 0.5 NO DIFF IN SITE OR FOREST TYPE
litter: 5.0 +/- 0.1 HERE WEEE GOOOOOO
*signif dif in fuel load between pine-hardwood and mixed-hardwood
1.0 mg ha-1 and 0.7 mg ha-1 respectively

#total loads 
ALL STANDS (Mg ha-1)
23.6 +/- 1.3

#comparison of litter composition and regression results

littershort

sqrtlitter <- littershort %>% 
  group_by(Site,Plot,group, Pine_pctBAft2a) %>% 
  summarise(sqrtpine = asin(sqrt(pine_pct/100)),
            sqrtuo = asin(sqrt(uo_pct/100)),
            sqrtencr = asin(sqrt(encr_pct/100)),
            sqrtpctBA = asin(sqrt(Pine_pctBAft2a/100)))

car::leveneTest(sqrtencr~Site*group, data = sqrtlitter)
hist(sqrtlitter$sqrtpine)
aov_res <- residuals(object = fueltest)
shapiro.test(x = aov_res)

pinetest <- lm(data=sqrtlitter, sqrtpine~group+Site)
uotest <- lm(data=sqrtlitter, sqrtuo~group+Site)
encrtest <- lm(data=sqrtlitter, sqrtencr~group+Site)

summary(pinetest)
summary(uotest)
summary(encrtest)

TukeyHSD(aov(pinetest))
TukeyHSD(aov(uotest))
TukeyHSD(aov(encrtest))
confint(pinetest)

#pine
pine is sig diff in all three groups p < 0.001
pine had 38.3% +/- 17.6 more leaf litter in pine than hard !!!
  pine had 23.4% +/- 15.8 more leaf litter in mixed than hardwood
pine had 14.8% +/- 10.6 more leaf litter in pine than mixed
#uo
uo is sig diff in mix-hard and pine-hard comp p < 0.001
uo had 34.9% +/- 17.5 LESS leaf litter in pine than hard !!!
  uo had 28.1% +/- 15.5 LESS leaf litter in mixed in than hard !!! 
  uo had 6.8% +/- 10.5 less leaf litter in pine than mixed ~nonsig
#encroaching
encr is signif between mixed and pine p < 0.05
encr had 3.4% +/- 13.2 less leaf litter in pine than hard
encr had 4.6% +/- 11.9 more leaf litter in mixed than hardwood
encr had 8.1% +/- 7.9 less more leaf litter in pine than mixed



#### for reg of litter comps x pine BA 

car::leveneTest(sqrtencr~Pine_pctBAft2a, data = sqrtlitter)
hist(sqrtlitter$sqrtpine)
aov_res <- residuals(object = encrcompba)
shapiro.test(x = aov_res)

#transformed data for analysis
pinecompba <- lm(sqrtpine~sqrtpctBA, data = sqrtlitter)
uocompba <- lm(sqrtuo~sqrtpctBA, data = sqrtlitter)
encrcompba <- lm(sqrtencr~sqrtpctBA, data = sqrtlitter)

summary(pinecompba)
summary(uocompba)
summary(encrcompba)

#regular data for raw numbers to report
pine1 <- lm(data=littershort, pine_pct~Pine_pctBAft2a)
uo1 <- lm(data=littershort, uo_pct~Pine_pctBAft2a)
encr1 <- lm(data=littershort, encr_pct~Pine_pctBAft2a)
summary(pine1)
summary(uo1)
summary(encr1)


#pine
pine is signif, p < 0.001, r2 = 0.40
each 10% increase in pine BA = 6.9 +/- 0.1 increase pine leaf litter mass
#uo
uo is signif, p < 0.001, r2 = 0.13
each 10% increase in pine BA = 4.5 +/- 0.1 decrease uo leaf litter mass

#encroaching
encr is signif p < 0.01, r2 = 0.07
each 10% increase in pine BA = 2.3 +/- 0.1 decrease encrac leaf litter mass

#something about t he intersection of all three at around 35%
#litter with BA of longleaf pine vs other pines

longleaffortest <- left_join(longleafx_total,littershort) 
otherpinefortest <- left_join(otherpinex_total,littershort) 

hist(longleaffortest$uo_pct)


llmod1 <- lm(data=longleaffortest, pine_pct~longleaf_bam2)
llmod2 <- lm(data=longleaffortest, uo_pct~longleaf_bam2)
llmod3 <- lm(data=longleaffortest, encr_pct~longleaf_bam2)

summary(llmod1)
summary(llmod2)
summary(llmod3)

#longleaf differences
#pine
pine is signif, p < 0.05, r2 = 0.17
each 1 increase in pine BA m2 = 2.1 +/- 0.9 increase pine leaf litter mass
#uo
uo is not, p = 0.1, r2 = 0.10
each 1 increase in pine BA m2 = 1.9 +/- 1.1 decrease uo leaf litter mass

#encroaching
encr is not p = 0.94 , r2 = 0.00
each 1 increase in pine BA m2 = 0.9 +/- 1.1 decrease encrac leaf litter mass


othermod1 <- lm(data=otherpinefortest, pine_pct~otherpine_bam2)
othermod2 <- lm(data=otherpinefortest, uo_pct~otherpine_bam2)
othermod3 <- lm(data=otherpinefortest, encr_pct~otherpine_bam2)

summary(othermod1)
summary(othermod2)
summary(othermod3)

#other pine differences
#pine
pine is signif, p < 0.001, r2 = 0.30
each 1 increase in pine BA m2 = 2.4 +/- 0.4 increase pine leaf litter mass
#uo
uo is p < 0.001, r2 = 0.29
each 1 increase in pine BA m2 = 2.9 +/- 0.5 decrease uo leaf litter mass

#encroaching
encr is not p = 0.27 , r2 = 0.01
each 1 increase in pine BA m2 = 0.4 +/- 0.4 decrease encrac leaf litter mass

ggplot(longleaffortest, (aes(x=longleaf_bam2,y=pine_pct)))+
  geom_smooth(method="lm") +
  geom_point()

ggplot(otherpinefortest, (aes(x=otherpine_bam2,y=pine_pct)))+
  geom_smooth(method="lm") +
  geom_point()

usloads_mghagroup2 <- usloads_mghagroup %>% 
  group_by(group) %>% 
  summarise(total_mean = sum(Mean_Load_Mgha),
            total_se = std.error(Mean_Load_Mgha))
