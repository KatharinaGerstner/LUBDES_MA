############################################################################
### Purpose of this skript module 08 is to:
###
### 08.1. Plot map of studies
###
### Authors: KG, MB, SK ...
############################################################################

### show current working directory and check if its local
getwd()

############################################################################
### 08.1. Plot map of studies
### 08.1.1 full map        
### 
############################################################################

world_map <- map_data("world")
p <- ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),fill="white",color="black",lwd=0.2) + 
  geom_point(data=ES.frame, aes(x=Longitude, y=Latitude, ymin=-55), color="blue", shape=4, size=1) +
  scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90° W", "0", "90° E")) +
  scale_y_continuous(breaks=c(-60, -30, 0, 30, 60), labels=c("60°S", "30°S","0°", "30°N","60°N")) +
  xlab("") + ylab("") 
p 
ggsave(paste(path2temp, "/CaseDistributionAll.png",sep=""), width=18, height=10, units="cm")

############################################################################
### 08.1.2 6 map by intensity classes
############################################################################

world_map <- map_data("world")
ES.frame <- within(ES.frame, LUI.range.level <- factor(LUI.range.level, levels = c("low-low","low-medium","low-high","medium-medium","medium-high","high-high"))) # resort levels for plotting
p <- ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),fill="white",color="black",lwd=0.3) + 
  geom_point(data=ES.frame, aes(x=Longitude, y=Latitude, ymin=-55), color="blue", shape=2, size=1) +
  scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90° W", "0", "90° E")) +
  scale_y_continuous(breaks=c(-60, -30, 0, 30, 60), labels=c("60°S", "30°S","0°", "30°N","60°N")) +
  xlab("") + ylab("") +
  facet_wrap(~LUI.range.level)   
p 
ggsave(paste(path2temp, "/CaseDistribution.png",sep=""), width=18, height=10, units="cm")

