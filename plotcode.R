library(rgdal)
library(dplyr)
library(rgeos)
library(ggplot2)
setwd("~/DCHackday")

PAD <- readOGR(dsn = "test2", layer = "test")
HUC_list <- as.numeric(as.character(unique(PAD$HUC_12)))
#centroids <- rgeos::gCentroid(PAD, byid = TRUE)


HUC_vec <- as.numeric(as.character(PAD$HUC_12))
centroids.df <- as.data.frame(coordinates(PAD))
names(centroids.df) <- c("Longitude", "Latitude")
pop.df <- data.frame(HUC_12 = HUC_vec, centroids.df)

# test <- over(x = PAD, y= centroids)
# test2 <- over(x = centroids, y =  PAD)
# test2 <- cbind(test2, test)
# test2 <- na.omit(test2)
# test2 <- cbind(test2, centroids[na.omit(test),])
# nrow(test2)
# check <- centroids[na.omit(test),]
# length(test)
# length(centroids)
ggplot(as.data.frame(centroids@coords))+
  geom_point(aes(x=x, y=y), color = "black") + theme_bw()
remove(PAD)

protected_map <- pop.df %>% left_join(protection)
protected_map$PIUCN_ALL <- ifelse(protected_map$PIUCN_ALL > .5, 1, 0)
protected_map <- na.omit(protected_map)
ggplot(na.omit(protected_map)) + 
  geom_point(aes(x=Longitude, y=Latitude)) + 
  facet_wrap(~PIUCN_ALL)

protected_plot <- ggplot() + 
  geom_point( data = protected_map[protected_map$PIUCN_ALL == 0,],
              aes(x=Longitude, y=Latitude), color = 'grey') +
  geom_point( data = protected_map[protected_map$PIUCN_ALL == 1,],
              aes(x=Longitude, y=Latitude), color = 'darkgreen', alpha = 3/4) +
  theme_bw(base_size = 10, ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     axis.line=element_blank(),axis.text.x=element_blank(),
                                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank()) +
  scale_colour_manual(name="Plot Colors",
                      values=c(Protect_Areas="darkgreen", Unprotect_Areas="grey")) + theme(legend.position="bottom")
ggsave(filename = 'protected_plots.png', plot = protected_plot, height = 5.3, width = 9.5)



data.model <- read.csv("datafinal.csv")

form = formula(protected~  Percent_of_HUC_Rare + MeanPrecip)

odds <- function(x) {
  exp(x)/(exp(x)+1)  
}

x <- na.omit(data.model)
x <- data.model
fit <- glm(form, family = binomial("logit"), data=x)
fcast <- odds(predict(fit, newdata = x))


data.model2 <- x
data.model2$fcast <- fcast


predicted_map <- pop.df %>% left_join(data.model2)
predicted_map$PIUCN_ALL <- ifelse(predicted_map$PIUCN_ALL > .5, 1, 0)
#predicted_map <- na.omit(predicted_map)
predicted_map$Forecasted <- predicted_map$fcast

predicted_plot <- ggplot() + 
  geom_point( data = predicted_map[predicted_map$Forecasted > 0,],
              aes(x=Longitude, y=Latitude, size = Forecasted, alpha = Forecasted), color = 'red') +
  geom_point( data = predicted_map[predicted_map$PIUCN_ALL == 0,],
              aes(x=Longitude, y=Latitude), color = 'grey') +
  geom_point( data = predicted_map[predicted_map$PIUCN_ALL == 1,],
              aes(x=Longitude, y=Latitude), color = 'darkgreen', alpha = 3/4) +
  theme_bw(base_size = 10, ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     axis.line=element_blank(),axis.text.x=element_blank(),
                                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(),
                                     legend.position = 'none') +
                                     scale_size(range = c(4, 10))
ggsave(filename = 'predicted_plots.png', plot = protected_plot, height = 5.3, width = 9.5)


