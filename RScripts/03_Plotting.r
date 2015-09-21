############################################################################
### Purpose of this skript module 03 is to:
###
### 03.1. Specify output directory in the dropbox or other local folder - NOT in the git directory!
### 03.2. Plot map of studies
### 03.3. Plot cross-diagrams
### 03.4. 
###
### General comments:
### * 
###
### Authors: KG, MB, SK ...
############################################################################

############################################################################
### 03.1. Specify output directory in the dropbox or other local folder - NOT in the git directory!
### 
### all lines should stay commented out, only temporarily set your wd
############################################################################

#setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB
#setwd("/tmp/") #MB

### show current working directory and check if its local
getwd()

############################################################################
### 03.2. Plot map of studies
### 
############################################################################

### plot study locations using the "classic" way
library(rworldmap)
newmap <- getMap(resolution = "low")
pdf("map_of_studies.pdf",pointsize=8)
plot(newmap, main="Distribution of LUBDES studies included in the meta analysis")
points(data$lon, data$lat, col = "blue", cex = .6, pch=2)
dev.off()

# ### plot study locations using fancy ggplot - Does NOT work properly #MB

# world_map <- map_data("world")
# p <- ggplot(legend=FALSE) +
#   geom_polygon(data=world_map, aes(x=long, y=lat)) + 
#   geom_point(data=data, aes(x=longitude..E..W., y=latitude..N..S.), color="blue")
# p ## looks weird, the reason is the max latitude in data = 2011! - check!
# ggsave("CaseDistribution.pdf", width=8, height=8, units="cm")

############################################################################
### 03.3. Plot cross-diagrams
### 
############################################################################

### plot all in one rush
#TO DO: Plot for no moderators
for(choose.moderator in as.character(unique(MA.coeffs$Moderator))[-1]){
  ES.moderator.subset <- subset(MA.coeffs, Moderator %in% choose.moderator)
  ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
  ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
  
  if(nrow(ES.moderator.subset) >= 2){
    plot <- ggplot() + 
      geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), size=3.5) +
      geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness - (1.96*se.Richness), ymax=mean.Richness + (1.96*se.Richness),color=factor(levels)), size=1) +
      geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness, color=factor(levels)),size=1) +
      geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
      scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
      scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
      scale_colour_brewer(palette="Set1") +
      ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=choose.moderator)
    print(plot)
  }
  
  if(nrow(ES.moderator.subset) == 1){
   ggplot() + 
      geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR), color="grey", size=3.5) +
      geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness)), color="green", size=1) +
      geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness), color="green", size=1) +
      geom_hline(data=ES.frame, x=0, linetype="twodash") + 
      geom_vline(data=ES.frame, y=0, linetype="twodash") +
      scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
      scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
      ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE)
    print(plot)
  }

ggsave(plot, file = paste("Cross_diagram_",choose.moderator,".png",sep=""), width = 15, height = 8, type = "cairo-png")

}

