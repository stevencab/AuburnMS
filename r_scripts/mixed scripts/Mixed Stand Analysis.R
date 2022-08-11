library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)
library(tidyr)
library(cowplot)

# Colorblind ggplot palette with black:
cbbPalette <- c("#FF0000", "#FF9999", "#FFCC99", "#CC99FF", "#0033FF", "#999999")
# Colorblind ggplot palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


Master <- read_csv("data/processed_data/MixedStand_Master.csv")

#####Let's begin with data visualization ----- 

#View(Master) regressions with CC and fuel comp+loads based on pine pct and BA

p1 <- ggplot(Master, aes(x=BA_ft2a)) +
  geom_point(aes(y = Avg_CC, color = Site)) +
  geom_smooth(aes(y = Avg_CC), method = lm, se = F) +
  scale_y_continuous(breaks = c(75,80,85,90,95,100), limits = c(73,100)) +
  annotate("text", x = 100, y = 80, label =  lb1 , parse = T, fontface = 2) +
  ylab("Canopy Cover (%)") +
  xlab("Total BA (ft2/acre)") +
  facet_wrap(~Site)

hist(log(Master$Avg_CC))

lb1 <- paste("R^2 == 0.001793")
x1 <- lm(Avg_CC ~ BA_ft2a, Master)
summary(x1)
ggsave(plot = p1, filename = "figures/canopyxtotalba.png")

p2 <- ggplot(Master, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Avg_CC, color = Site)) +
  geom_smooth(aes(y = Avg_CC), method = lm, se = F) +
  scale_y_continuous(breaks = c(75,80,85,90,95,100), limits = c(73,100)) +
  annotate("text", x = 50, y = 80, label =  lb2 , parse = T, fontface = 2) +
  annotate("text", x = 50, y = 78, label =  "t = -2.139 *" , fontface = 2) +
  ylab("Canopy Cover (%)") +
  xlab("Pine BA (ft2/acre)") +  
  facet_wrap(~Site)



lb2 <- paste("R^2 == 0.0455")

x2 <- lm(Avg_CC ~ Pine_ft2a, Master)
summary(x2)
ggsave(plot = p2, filename = "figures/canopyxpineba.png")



litterba <- read_csv("data/processed_data/litterxba.csv")


comp_pinepct <- ggplot(litterba, aes(x = Pine_pctBAft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = T) +
  ylab("Litter type by mass (%)") +
  xlab("Pine BA (%)") + 
  labs(title = "Litter Composition vs. Pine BA %", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5)) +
  facet_wrap(~Site)
  
ggsave(plot = comp_pinepct, filename = "figures/comp_pinepct.png", height = 6, width = 12.7)

comp_pinecont <- ggplot(litterba, aes(x = Pine_ft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = T) +
  ylab("Litter type by mass (%)") +
  xlab("Pine BA (ft2/acre)") + 
  labs(title = "Litter Composition vs. Pine BA Raw", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5)) 
annotate("text", x = 20, y = 100, label = "n = 98 plots", fontface = 2)
ggsave(plot = comp_pinecont, filename = "figures/comp_pinecont.png", height = 6, width = 12.7)

load_pinepct <- ggplot(litterba, aes(x = Pine_pctBAft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Load_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Load_wt, color = Little_type), method = lm, se = T) +
  ylab("Fuel load (g/m2)") +
  xlab("Pine BA (%)") + 
  labs(title = "Fuel load vs. Pine BA %", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5)) 
annotate("text", x = 20, y = 100, label = "n = 98 plots", fontface = 2)
ggsave(plot = load_pinepct, filename = "figures/load_pinepct.png", height = 6, width = 12.7)

load_pinecont <- ggplot(litterba, aes(x = Pine_ft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Load_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Load_wt, color = Little_type), method = lm, se = T) +
  ylab("Fuel load (g/m2)") +
  xlab("Pine BA (ft2/acre)") + 
  labs(title = "Fuel load vs. Pine BA Raw", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5)) 
annotate("text", x = 20, y = 100, label = "n = 98 plots", fontface = 2)
ggsave(plot = load_pinecont, filename = "figures/load_pinecont.png", height = 6, width = 12.7)

x3 <- lm(Avg_Meso_load ~ Pine_ft2a, Master)
summary(x3)


#### creating pine pct separations with litter ----

litterba <- read_csv("data/processed_data/litterxba.csv")


litterba$Little_type <- factor(litterba$Little_type, levels = c("Pinus","Upland oak","Sweetgum","Water oak","Mesophyte", "Total"))

ggplot(litterba, aes(x=Pine_pctBAft2a)) + 
  scale_color_manual(name = "Litter Type", values =c("#FF0000","#FF9999","#FFCC99","#CC99FF","#0033FF", "#0072B2")) +
  geom_jitter(aes(y = Pct_wt, color = Little_type), alpha = 0.6, width = 0.25, height = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  labs(title="Percent Leaf Litter x Percent Pine BA",
       x="Pine Basal Area (%)",
       y="Leaf Litter Type") +
  theme(plot.title = element_text(hjust = 0.5))


aggregate(BA_ft2a  ~ Site,
          data = Master,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2))

litterba

pine0_30 <- litterba %>% 
  filter(Pine_pctBAft2a <29.999)

pine30_50 <- litterba %>% 
  filter(between(Pine_pctBAft2a,30,49.999))

pine30_70 <- litterba %>% 
  filter(between(Pine_pctBAft2a,30,69.9999))

pine70_100 <- litterba %>% 
  filter(between(Pine_pctBAft2a,70,100))

plot(pine0_30)

test0_30 <- pine0_30 %>% 
  group_by(Little_type) %>% 
  summarise(Percent_Litter_mean = mean(Pct_wt),
            sd = sd(Pct_wt),
            n = n(),
            Percent_litter_se = sd/sqrt(n))

test30_50 <- pine30_50 %>% 
  group_by(Little_type) %>% 
  summarise(Percent_Litter_mean = mean(Pct_wt),
            Percent_litter_sd = sd(Pct_wt))

test30_70 <- pine50_70 %>% 
  group_by(Little_type) %>% 
  summarise(Percent_Litter_mean = mean(Pct_wt),
            Percent_litter_sd = sd(Pct_wt))

test70_100 <- pine70_100 %>% 
  group_by(Little_type) %>% 
  summarise(Percent_Litter_mean = mean(Pct_wt),
            Percent_litter_sd = sd(Pct_wt))

t1 <- ggplot(pine0_30, aes(x = Pine_pctBAft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = T) +
  scale_y_continuous(limits = c(-4,100)) +
  scale_x_continuous(limits = c(10,30)) +
  ylab("Litter type by mass (%)") +
  xlab("Pine BA (%)") + 
  labs(title = "0 - 30% Pine BA", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none") +
  annotate("text", x = 20, y = 100, label = "n = 8", fontface = 2)

t2 <- ggplot(pine30_50, aes(x = Pine_pctBAft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) + 
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = T) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(30,50)) +
  #ylab("Litter type by mass (%)") +
  xlab("Pine BA (%)") + 
  labs(title = "30 - 50% Pine BA", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none", axis.title.y = element_blank()) +
  annotate("text", x = 40, y = 100, label = "n = 29", fontface = 2)



t3 <- ggplot(pine30_70, aes(x = Pine_pctBAft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = T) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(50,70)) +
  ylab("Litter type by mass (%)") +
  xlab("Pine BA (%)") + 
  labs(title = "50 - 70% Pine BA", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none", axis.title.y = element_blank()) +
  annotate("text", x = 60, y = 100, label = "n = 33", fontface = 2)


t4 <- ggplot(pine70_100, aes(x = Pine_pctBAft2a)) +
  scale_color_manual(name = "Litter type", values =cbbPalette) +
  geom_point(aes(y = Pct_wt, color = Little_type), alpha = 0.5) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = T) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(70,100)) +
  ylab("Litter type by mass (%)") +
  xlab("Pine BA (%)") + 
  labs(title = "70 - 100% Pine BA", color = "Litter Type") +
  theme(plot.title = element_text(hjust =0.5), legend.position = "none", axis.title.y = element_blank()) +
  annotate("text", x = 85, y = 100, label = "n = 28", fontface = 2)


legend <- get_legend(
  
  t1 + theme(legend.position = "bottom", legend.box.margin = margin(2,0,0,20)) +
    guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)))

t5 <- cowplot::plot_grid(t1, t3, t4,
                         ncol = 3)
final_pineba <- plot_grid(t5, legend, ncol = 1, rel_heights = c(1, .1))

ggsave(plot = final_pineba, filename = "figures/litterxpineba.png", height = 4.2, width = 11)
       

t5 <- cowplot::plot_grid(t1 + theme(axis.title.x = element_blank()), 
                         t2 + theme(axis.title.x = element_blank()),
                         t3 + theme(axis.title.x = element_blank()),
                         t4 + theme(axis.title.x = element_blank()),
                         ncol = 4)


read_csv(load_pct2, )



## pca testing ----


MasterPCA <- read_csv("data/processed_data/MixedStand_MasterPCA.csv")

l_fun <- sapply(MasterPCA, function(x) log(x, base = 10))
negs <- l_fun

l_fun[!is.infinite(.)] <- 0

test <- cbind(MasterPCA,l_fun)
test <- test[,-(3:22), drop = F]
myPr <- prcomp(MasterPCA[,3:20], scale = T)
summary(myPr)
plot(myPr, type = "l")
  biplot(myPr, scale = 0
       )



#extract pc scores

str(myPr)
myPr$x
MasterPCA2 <- cbind(test, myPr$x[,1:2])

## ggplotting with pc scores

ggplot(MasterPCA2, aes(PC1, PC2, col = Site, fill = Site)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(MasterPCA[,3:27], MasterPCA2[,28:29])


## testing cluster analysis

MasterPCAscaled <- scale(MasterPCA[,3:12])
motscale <- scale(MOTPCA[,3:10])

# k-means clustering

fitK <- kmeans(MasterPCA2, 4)
plot(motscale, col = fitK$cluster)

k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(motscale, i)
}
k

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10, betweenss_totss, type = "b",
     ylab = "Between SS/ Total SS", xlab = "# Clusters k")

for(i in 1:6){
  plot(MasterPCA, col = k[[i]]$cluster)
}


### hierachial clustering

d <- dist(scalelog)
fitH <- hclust(d, "ward.D2")
plot(fitH
     )

rect.hclust(fitH, k = 5, border = "red")
clusters <- cutree(fitH, 5)
clusters
plot(scalelog, col = clusters)

#density based

install.packages("dbscan")
library(dbscan)
kNNdistplot(MasterPCAscaled, k = 5)
fitD <- dbscan(MasterPCAscaled, eps = 0.5, minPts = 16)


###### site specific regressions -----

#canopy
ggplot(Master, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Avg_CC)) +
  geom_smooth(aes(y = Avg_CC), method = lm, se = F) +
  scale_y_continuous(breaks = c(75,80,85,90,95,100), limits = c(73,100)) +
  ylab("Canopy Cover (%)") +
  xlab("Pine BA (ft2/acre)") +
  facet_wrap(~Site)

hist(log(Master$Avg_CC))

lb1 <- paste("R^2 == 0.001793")
x1 <- lm(Avg_CC ~ BA_ft2a, Master)
summary(x1)
ggsave(plot = p1, filename = "figures/canopyxtotalba.png")


#fuel comp

ggplot(litterba, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Pct_wt, color = Little_type)) +
  geom_smooth(aes(y = Pct_wt, color = Little_type), method = lm, se = F) +
  ylab("Fuel type by mass (%)") +
  xlab("Pine BA (ft2/acre)") + 
  facet_wrap(~Site)

#fuel load

ggplot(litterba, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Load_wt, color = Little_type)) +
  geom_smooth(aes(y = Load_wt, color = Little_type), method = lm, se = F) +
  ylab("Fuel load (g/m2)") +
  xlab("Pine BA (ft2/acre)") + 
  facet_wrap(~Site)

#herb cover

ggplot(Master, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Avg_Herb_Live)) +
  geom_smooth(aes(y = Avg_Herb_Live), method = lm, se = F) +
  ylab("Herbaceous fuel cover m2") +
  xlab("Pine BA (ft2/acre)") + 
  facet_wrap(~Site)
#herb height

ggplot(Master, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Avg_Herb_Ht)) +
  geom_smooth(aes(y = Avg_Herb_Ht), method = lm, se = F) +
  ylab("Herbaceous fuel cover m2") +
  xlab("Pine BA (ft2/acre)") + 
  facet_wrap(~Site)

#seed structures

ggplot(Master, aes(x=Pine_ft2a)) +
  geom_point(aes(y = Avg_Cones)) +
  geom_smooth(aes(y = Avg_Cones), method = lm, se = F) +
  ylab("Cones m2") +
  xlab("Pine BA (ft2/acre)") + 
  facet_wrap(~Site)




#new pca log scale
pcalog <- read_csv("data/processed_data/MixedStand_MasterPCAlogscale.csv")


  


myPr <- prcomp(pcalog[,3:22], scale = F)
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0
)



#extract pc scores

str(myPr)
myPr$x
MasterPCA2 <- cbind(pcalog, myPr$x[,1:2])

## ggplotting with pc scores

ggplot(MasterPCA2, aes(PC1, PC2, col = Site, fill = Site)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

cor(pcalog[,3:20], MasterPCA2[,21:22])


scalelog <- scale(pcalog[,3:22])
fitK <- kmeans(scalelog, 4)
plot(scalelog, col = fitK$cluster)

k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(scalelog, i)
}
k

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10, betweenss_totss, type = "b",
     ylab = "Between SS/ Total SS", xlab = "# Clusters k")

for(i in 1:6){
  plot(MasterPCA, col = k[[i]]$cluster)
}