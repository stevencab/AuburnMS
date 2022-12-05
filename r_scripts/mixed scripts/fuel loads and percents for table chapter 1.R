#leaf litter load and percent table for chap 1
library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggsignif)
library(factoextra)
library(plotrix)

litter2 <- read_csv("data/raw_data/Mixed Stands/MixedStand_LitterSortingALL.csv")

litter2 <- litter2 %>% 
  group_by(Site, Plot, Location) %>% 
  summarise(Total_wt = Total_wt/0.09,
            Pine_wt = Pine_wt/0.09,
            UO_wt = UO_wt/0.09,
            Meso_wt = (SG_wt+WO_wt+MESO_wt)/0.09)
littergroup <- litter %>% 
  group_by(group, Little_type) %>% 
  summarise(Load_wt_mean = mean(Load_wt),
            Load_wt_std = std.error(Load_wt),
            Relative_mean = mean(Pct_wt),
            Relative_std = std.error(Pct_wt))

usloadsgroup <- usloads %>% 
  group_by(group, Biomass_Load_Source ) %>% 
  summarise(Load_wt_mean = mean(Load_kgm2*10),
            Load_wt_std = std.error(Load_kgm2*10))

usloadsgroup2 <- usloadsgroup %>% 
  group_by(group) %>% 
  summarise(Load_wt_mean = sum(Load_wt_mean),
            Load_wt_std = sum(Load_wt_std))
loadtest <- lm(data=usloadsgroup, Load_wt_mean~group)
summary(loadtest)

loads_sum <- usloads_short %>%
  group_by(group) %>% 
  summarise(mean_cwd = mean(CWD*10)/usloadsgroup2$Load_wt_mean,
            se_cwd = std.error(CWD*10)/sqrt(usloadsgroup2$Load_wt_std),
            mean_fwd = mean(FWD*10)/usloadsgroup2$Load_wt_mean,
            se_fwd = std.error(FWD*10)/sqrt(usloadsgroup2$Load_wt_std),
            mean_herb = mean(Herbs*10)/usloadsgroup2$Load_wt_mean,
            se_herb = std.error(Herbs*10)/sqrt(usloadsgroup2$Load_wt_std),
            mean_shrub = mean(Shrub*10)/usloadsgroup2$Load_wt_mean,
            se_shrub = std.error(Shrub*10)/sqrt(usloadsgroup2$Load_wt_std),
            mean_duff = mean(Duff*10)/usloadsgroup2$Load_wt_mean,
            se_duff = std.error(Duff*10)/sqrt(usloadsgroup2$Load_wt_std),
            mean_litter = mean(Litter*10)/usloadsgroup2$Load_wt_mean,
            se_litter = std.error(Litter*10)/sqrt(usloadsgroup2$Load_wt_std))

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

loads_sum2 <- loads_sum %>%
  group_by(group) %>% 
  summarise(total = 
              sum(mean_cwd+mean_fwd+mean_herb+mean_shrub+mean_duff+mean_litter),
            cwd_m = mean_cwd/total,
            cwd_se = sqrt(mean_cwd),
            fwd_m = mean_fwd/total,
            fwd_se = sqrt(mean_fwd),
            herb_m = mean_herb/total,
            herb_se = sqrt(mean_herb),
            shrub_m = mean_shrub/total,
            shrub_se = sqrt(mean_shrub),
            duff_m = mean_duff/total,
            duff_se = sqrt(mean_duff),
            litter_m = mean_litter/total,
            litter_se = sqrt(mean_litter)
