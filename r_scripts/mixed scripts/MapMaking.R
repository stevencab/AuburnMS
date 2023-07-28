library(ggplot2)
library(usmap)

plot_usmap(include = "AL")

state <- map_data("state")

ggplot(data=state, aes(x=long, y=lat, fill=region, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

alabama <- subset(state, region=="alabama")
counties <- map_data("county")
alabama_county <- subset(counties, region=="alabama")

#get county layers
lee_county <- subset(alabama_county, subregion=="lee")
tallapoosa_county <- subset(alabama_county, subregion=="tallapoosa")
cleburne_county <- subset(alabama_county, subregion=="cleburne")
calhoun_county <- subset(alabama_county, subregion=="calhoun")
macon_county <- subset(alabama_county, subregion=="macon")

#make these combined becasue they border eachother
leemacontallapoosa <- rbind(lee_county,macon_county,tallapoosa_county)

cleburncalhoun <- rbind(cleburne_county,calhoun_county)

al_map <- ggplot(data=alabama, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.5) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=alabama_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  #ggtitle('Alabama Map with Counties') + 
  geom_polygon(data = lee_county, fill = "red", color = "white") +
  geom_polygon(data = tallapoosa_county, fill = "red", color = "white") +
  geom_polygon(data = macon_county, fill = "red", color = "white") +
  geom_polygon(data = cleburne_county, fill = "red", color = "white") +
  geom_polygon(data = calhoun_county, fill = "red", color = "white") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

al_map

#load gps info
gpsinfo <- readr::read_csv("data/raw_data/Mixed Stands/MixedStand_PlotLocationsGPS.csv")

leec <- gpsinfo %>% 
  filter(county=="lee")
maconc <- gpsinfo %>% 
  filter(county=="macon")
tallapoosac <- gpsinfo %>% 
  filter(county=="tallapoosa")
cleburnec <- gpsinfo %>% 
  filter(county=="cleburne")

#map for lee/macon/tallapoosa counties
lmt_map <- ggplot(data=leemacontallapoosa, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=leemacontallapoosa, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_point(data=leec, aes(x=long,y=lat, group = NULL), color="red") +
  geom_point(data=maconc, aes(x=long,y=lat, group = NULL), color="red") +
  geom_point(data=tallapoosac, aes(x=long,y=lat, group = NULL), color="red") +
  #ggtitle('Tallapoosa, Macon, and Lee counties') + 
  annotate("text", x = -85.4, y = 32.71, label =  "KNP (n = 5)", size = 5, fontface = 2) +  
  annotate("text", x = -85.32, y = 32.62, label =  "MOTDF (n = 19)", size = 5, fontface = 2)  +
  annotate("text", x = -85.827, y = 32.75, label =  "CRATA (n = 5)", size = 5, fontface = 2) +  
  annotate("text", x = -85.65, y = 32.397, label =  "TUNF (n = 29)", size = 5, fontface = 2) +  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


calhouncleburn_map <- ggplot(data=cleburncalhoun, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=cleburncalhoun, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_point(data=cleburnec, aes(x=long,y=lat, group = NULL), color="red") +
  #ggtitle('Calhoun & Cleburne County') + 
  annotate("text", x = -85.502, y = 33.7802, label =  "TANF (n = 39)", size = 5, fontface = 2) +  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot_grid(calhouncleburn_map,lmt_map, al_map, ncol = 1.5)
ggsave(plot = al_map, "figures/maps/mixed stands/alabamacounty.png", width =2, height = 8)
ggsave(plot = calhouncleburn_map, "figures/maps/mixed stands/calhouncleburn.png", width =5.5, height = 4)
ggsave(plot = lmt_map, "figures/maps/mixed stands/leemacontallapoosa.png",  width =5.5, height = 4)



#old ignore
macon_map <- ggplot(data=macon_county, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=macon_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_point(data=maconc, aes(x=long,y=lat, group = NULL), color="red") +
  ggtitle('Macon County') + 
  annotate("text", x = -85.32, y = 32.68, label =  "Kreher Preserve & Nature Center (n = 5)", size = 3, fontface = 2) +  
  annotate("text", x = -85.25, y = 32.6, label =  "MOT Demonstration Forest (n = 19)", size = 3, fontface = 2)  +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

tallapoosa_map <- ggplot(data=tallapoosa_county, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=tallapoosa_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_point(data=tallapoosac, aes(x=long,y=lat, group = NULL), color="red") +
  ggtitle('Tallapoosa County') + 
  annotate("text", x = -85.827, y = 32.71, label =  "Cherokee Ridge Alpine Ridge (n = 5)", size = 3, fontface = 2) +  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

calhouncleburn_map <- ggplot(data=cleburncalhoun, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=cleburncalhoun, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_point(data=cleburnec, aes(x=long,y=lat, group = NULL), color="red") +
  ggtitle('Calhoun & Cleburne County') + 
  #annotate("text", x = -85.32, y = 32.68, label =  "Kreher Preserve & Nature Center (n = 5)", size = 3, fontface = 2) +  
  #annotate("text", x = -85.25, y = 32.6, label =  "MOT Demonstration Forest (n = 19)", size = 3, fontface = 2)  +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())




Master
gpsinfo
g1 <- filter(Master, Site=="Talladega")
g2 <- filter(gpsinfo, Site=="Talladega")
View(g1)
View(g2)
unique(gpsinfo$Site)
unique(Master$Site)
anti_join(g2, g1)
