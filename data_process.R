library(rgdal)
library(dplyr)
library(rgeos)
setwd("~/DCHackday")

PAD <- readOGR(dsn = "test2", layer = "test")
HUC_list <- as.numeric(as.character(unique(PAD$HUC_12)))

write.csv(HUC_list, "science_hackday_conservation/huc_list.csv")
data <- data.frame(HUC_12 = HUC_list)
remove(PAD)
protection <- read.csv("EnviroAtlas/protection.csv")
# Use picun-all for protected identification
write.csv(protection, "science_hackday_conservation/protection.csv")

landcover <- read.csv("EnviroAtlas/landcover.csv")
write.csv(landcover, "science_hackday_conservation/landcover.csv")

agw <- read.csv("EnviroAtlas/agw_demand.csv")
write.csv(agw, "science_hackday_conservation/agw.csv")

avg_perc <- read.csv("EnviroAtlas/average_precipitation.csv")
write.csv(avg_perc, "science_hackday_conservation/avg_perc.csv")

erm <- read.csv("EnviroAtlas/ecosystem_rarity_metrics.csv")
write.csv(erm, "science_hackday_conservation/erm.csv")

ind_water <- read.csv("EnviroAtlas/industrial_water_demand.csv")
write.csv(ind_water, "science_hackday_conservation/ind_water.csv")

nature_reserve <- read.csv("EnviroAtlas/nature_reserve.csv")
write.csv(nature_reserve, "science_hackday_conservation/nature_reserve.csv")

usa_reptile <- read.csv("EnviroAtlas/usa_reptile.csv")
write.csv(usa_reptile, "science_hackday_conservation/usa_reptile.csv")
usa_reptile <- usa_reptile %>% rename(HUC_12 = HUC12)

data.final <- data %>% 
  left_join(landcover) %>%
  left_join(agw) %>%
  left_join(avg_perc) %>%
  left_join(erm) %>%
  left_join(ind_water) %>%
  left_join(protection)
  #left_join(nature_reserve %>% filter(HUC_12 %in% HUC_list) %>% filter(is.finite(HUC_12)))
  #left_join(usa_reptile %>% filter(HUC_12 %in% HUC_list, is.finite(HUC_12)))

#library(ggplot2)
#ggplot(protection)+geom_histogram(aes(x=PIUCN_ALL), color = "RED") +geom_histogram(aes(x=PGAPSTAT123), color = "BLUE")

data.model <- data.final %>% mutate(protected = ifelse(PIUCN_ALL > 0, 1, 0))
data.model$

write.csv(data.model, "datafinal.csv", row.names = FALSE)

