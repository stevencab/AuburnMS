library(readr)
library(dplyr)
library(ggplot2)
library(stringi, stringr)

mixed_trees <- read_csv("data/raw_data/Mixed Stands/MixedStand_Trees.csv")



dbh_cm_in <- mixed_trees %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(dbh_cm = DBH,
            dbh_in = (DBH)*0.393701)

basal_area_m2_ft2 <- dbh_cm_in %>% 
  group_by(Stem_id, Plot, Site, Species, Genus) %>% 
  summarise(ba_m2 = dbh_cm^2*(0.00007854),
            ba_ft2 = dbh_in^2*(0.005454))

basal_area_m2_ft2_plot <- basal_area_m2_ft2 %>% 
  group_by(Plot,Site) %>% 
  summarise(ba_m2ha = sum(ba_m2)/0.02023428,
            ba_ft2a = sum(ba_ft2)/0.05)

basal_area_m2_ft2_plot_PINE <- basal_area_m2_ft2 %>% 
  filter(Genus == "Pinus") %>% 
  group_by(Plot, Site) %>% 
  summarise(pine_metric = sum(ba_m2)/0.02023428,
            pine_american = sum(ba_ft2)/0.05)

test <- merge(basal_area_m2_ft2_plot, basal_area_m2_ft2_plot_PINE)

test <- test %>% 
  group_by(Plot, Site) %>% 
  summarise(percent = pine_american/ba_ft2a*100)

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

percent_pine_fig <- ggplot(test, aes(x=percent)) + scale_fill_manual(values =cbPalette) +
  geom_histogram(aes(fill=Site),
                 binwidth = 10,
                 col="black", 
                 size=.1) +
  scale_x_continuous(breaks=c(0,10,30,50,70,90,100)) +
  labs(title="Percent Pine Basal Area Distribution",
       x="Pine Basal Area (%)",
       y="Number of plots") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "figures/percent_pine_ba.png", plot = percent_pine_fig)

n_distinct(test$Plot & Site=="Tuskegee")

cr <- test %>% 
  filter(Site=="Tuskegee")
