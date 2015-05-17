library(rgdal)
setwd("~/DCHackday/science_hackday_conservation")

library(maptools)
gpclibPermit()

maptools::readShapeSpatial(fn = "landcover.dbf")

setwd("~/DCHackday")
shape <- readOGR(dsn = "FESM", layer = "FESM_OPR")

ogrInfo(dsn = "EnviroAtlas", layer = "landcover")
rgdal::readGDAL

plot()

data <- read.csv("EnviroAtlas/landcov.csv")

setwd("~/DCHackday")
shape <- readOGR(dsn = "NATIONAL_METRICS_NOV2014/National_metrics_Nov2014//National_metrics.gdb", layer = "manure06")
undebug(ogrInfo)
ogrInfo(dsn = "NATIONAL_METRICS_NOV2014/National_metrics_Nov2014//National_metrics.gdb", layer = "landcover")

library(ggplot2)

PAD <- readOGR(dsn = "test2", layer = "test")
PAD_n <- PAD[1:10,]
HUC_list <- unique(PAD$HUC_12)


res <- 10000
BB <- bbox(PAD_n)
BB <- res*round(BB/res)
GT <- GridTopology(cellcentre.offset = BB[,1], 
                   cellsize = c(res, res),
                   cells.dim = (c(diff(BB[1,]), diff(BB[2,]))/res) + 1)

SP <- SpatialPoints(GT, proj4string = CRS(proj4string(PAD_n)))
vals <- over(SP, PAD_n)
res <- cbind(coordinates(SP), vals)
x <- res[!is.na(res$HUC_12),]
rbind(head(x,3), tail(x,3))

#layers
# Access
# Category
# GAP_sts
# IUCN_Cat
# Own_Name
# Own_Type
# P_des_tp
# State_Nm
# Status
# PADUS_1_3Fee
# PADUS_1_3MPA
# PADUS_1_3Combined
# PADUS_1_3Easements

shape <- readOGR(dsn = "FESM1", "FESM_1")
gpclibPermit()
gpclibPermitStatus()
shapef <- fortify(shape, region = "quadname")
mapf <- data.frame(lat =shape$lrlat , 
           long = shape$lrlon, 
           region = shape$quadname,
           elevation = shape$zmean)
ggplot(mapf) + geom_point(aes(x=x, y=y, color = elevation))
plot(shape)

summary(shape$quadname)
summary(shape$lrlat)
summary(shape$lrlon)
summary(shape$ullat)
summary(shape$ullon)
summary(shape$)


library(maps)
library(dplyr)
all_states = map_data("state")

ggplot() + geom_polygon(data = all_states,
                      aes(x=long, y=lat, group = group),
                      color="white",
                      fill="grey10") + geom_point(data = mapf %>% filter( long < 0) , aes(x = long, y = lat, color = elevation))

