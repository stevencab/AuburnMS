library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)
library(tidyr) 

trees <- read_csv("data/raw_data/testingtreesforiv.csv")

trees


test <- trees %>% 
  group_by(Site, Plot, functional_group) %>% 
  summarise(N = n())


d1 <- test %>% 
  group_by(Site,Plot) %>% 
  summarise(plot_total = sum(N))

test1 <- inner_join(test,d1)



write.table(rd, "clipboard", sep="\t", row.names=FALSE)

den <- read_csv("data/raw_data/ivdensity.csv")

relden <- den %>% 
  group_by(Site,Plot, functional_group) %>% 
  summarise(rel_den = N[unique(functional_group)]/plot_total*100)
            
rd <- relden[!duplicated(relden),]


dom <- trees %>% 
  group_by(Site,Plot,functional_group) %>% 
  summarise(dbh_in = DBH/2.54)
dom <- dom %>% 
  group_by(Site,Plot,functional_group) %>% 
  summarise(basal = dbh_in^2*0.005454)

dom <- dom %>% 
  group_by(Site,Plot,functional_group) %>% 
  summarise(ba = sum(basal)*20)

write.table(dom, "clipboard", sep="\t", row.names=FALSE)

dom1 <- read_csv("data/raw_data/ivdom.csv")

reldom <- dom1 %>% 
  group_by(Site,Plot) %>% 
  summarise(rd_pine = (ba[functional_group=="pyrophyte"]/sum(ba))*100,
            rd_uo = (ba[functional_group=="uo"]/sum(ba))*100,
            rd_meso = (ba[functional_group=="mesophyte"]/sum(ba))*100)

write.table(reldom, "clipboard", sep="\t", row.names=FALSE)

plotiv <- read_csv("data/raw_data/Mixed Stands/MixedStand_PlotIV.csv")

hist(log(iv$iv=="pine"))
plotv <- plotiv %>% 
  filter(functional_group=="meso")
write.table(plotv, "clipboard", sep="\t", row.names=FALSE)


ggplot(plotv, aes(iv, fill = functional_group, color = functional_group))+
  geom_histogram()
