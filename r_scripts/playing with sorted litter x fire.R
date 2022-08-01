######playing with sorted litter and fire data

library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)

litter <- read_csv("C:/Users/smc0124/OneDrive - Auburn University/Desktop/Auburn/FireMS/data/LitterSortingALLforR.csv")
fire <- read_csv("C:/Users/smc0124/OneDrive - Auburn University/Desktop/Auburn/FireMS/data/MOTburndataforR.csv")

litter_avgs <- litter %>% 
  group_by(Plot, Treatment) %>% 
  summarise(avg_pine = mean(Pine_wt),
            avg_uo = mean(UO_wt),
            avg_sg = mean(SG_wt),
            avg_wo = mean(WO_wt),
            avg_meso = mean(MESO_wt),
            avg_total = mean(Total_wt))

fire_x_litter <- merge(litter_avgs, fire, by = c("Plot","Treatment"))

testing <- fire_x_litter %>% 
  summarise(pine = mean(avg_pine),
            uo = mean(avg_uo),
            sg = mean(avg_sg),
            wo = mean(avg_wo),
            meso = mean(avg_meso),
            total_biomass = mean(avg_total),
            fh = mean(avg_fh),
            #fd = mean(fd),
            consump = mean(consump),
            max_temp = mean(avg_max_temp))

testing.pca <- prcomp(fire_x_litter[,c(3:8,13,15,21)], center = T, scale. = T)

autoplot(testing.pca)

mantest <- manova(cbind(avg_pine, avg_uo, avg_sg, avg_wo, avg_meso, avg_fh, avg_max_temp) ~ Treatment, data = fire_x_litter)

summary.aov(mantest)
