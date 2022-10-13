####thesis final mixed stand analysis for pub 

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
litter <- read_csv("data/processed_data/litterxba.csv")
littershort <- read_csv("data/processed_data/litterba_short.csv")


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
  annotate("text", x = 20, y = 8.5, label =  "Hardwood Forest\n n = 8", fontface = 2, size = 4) +
  annotate("text", x = 50, y = 8.5, label =  "Mixed Forest\n n = 68" , fontface = 2, size = 4) +
  annotate("text", x = 89, y = 8.5, label =  "Pine Forest\n n = 21" , fontface = 2, size = 4) +
  theme(legend.position = "none") +
  scale_fill_grey(start = 0.35, end = 0.9)



legend <- get_legend(
  
  f1 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f3 <- cowplot::plot_grid(f1,f2,
                   ncol = 2)  

f4 <- plot_grid(f3, legend, ncol = 1, rel_heights = c(1, .1))

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
  scale_fill_grey(start = 0.35, end = 0.9) +
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
  scale_fill_grey(start = 0.35, end = 0.9) +
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
  scale_fill_grey(start = 0.35, end = 0.9) +
  theme(legend.position = "none")




legend3 <- get_legend(
  
  f9 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f11 <- plot_grid(f9,f10,
                ncol = 2)  

f12 <- plot_grid(f11, legend3, ncol = 1, rel_heights = c(1, .1))

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
  #stat_compare_means(comparisons = my_comparisons) +
  scale_fill_grey(start = 0.35, end = 0.9) +
  theme(legend.position = "none")
  
legend4 <- get_legend(f13 + theme(legend.position = "none"))

f15 <- plot_grid(f13,f14,
                 ncol = 2)  

f16 <- plot_grid(f15, legend4, ncol = 1, rel_heights = c(1, .1))

Masterlogcc <- Master %>% 
  mutate(sqrtCC = sqrt(Avg_CC))


plot(cclm, 2)
car::leveneTest(logCC~Site, data = Masterlogcc)
aov_res <- residuals(object = cclm)
shapiro.test(x = aov_res)

cclm <- lm(data=Master,Avg_CC~Site*group)
hist(log(Master$Avg_CC))
hist(data=iris, iris$Sepal.Length)
summary(cclm)
TukeyHSD(aov(cclm))

cclm2 <- lm(data=Masterlogcc,sqrtCC~Site*group)
summary(cclm2)
TukeyHSD(aov(cclm2))

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
  scale_fill_grey(start = 0.35, end = 0.9) +
  theme(legend.position = "none")

legend5 <- get_legend(
  
  f17 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f19 <- plot_grid(f17,f18,
                 ncol = 2)  

f20 <- plot_grid(f19, legend5, ncol = 1, rel_heights = c(1, .1))

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
  geom_smooth(method="lm", se=F, size = 1.5) +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_bw() +
  geom_vline(xintercept = 30, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 70, linetype = "dashed", size = 1) +
  annotate("text", x = 18, y = 75, label =  "Hardwood Forest\n", fontface = 2, size = 6) +
  annotate("text", x = 50, y = 75, label =  "Mixed Forest\n" , fontface = 2, size = 6) +
  annotate("text", x = 85.5, y = 75, label =  "Pine Forest\n" , fontface = 2, size = 6) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                                limits=c(10,100)) +
  labs(x="Relative pine basal area (%)",
       y="Percent leaf litter by mass (%)",
       color="Species Group",
       shape="Species Group",
       size="Species Group") +
  geom_point(size = 4,stroke = 1.1, position = "jitter") +
  scale_shape_manual(values=c(21,22,24)) +
  theme(legend.position = "none")

legend6 <- get_legend(
  
  f21 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

f22 <- plot_grid(f21, legend6, ncol = 1, rel_heights = c(1, .1))


f21
f22
f23
f24
f25
f26
f27
f28


#### figure for longleaf only vs other pines

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