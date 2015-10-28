############################################################################
### Purpose of this skript module 03 is to:
###
### 03.1. Specify output directory in the dropbox or other local folder - NOT in the git directory!
### 03.2. Plot map of studies
### 03.3. Plot cross-diagrams
### 03.4. Forest plots for noLU vs low/medium/high LU
###
### General comments:
### * TO DO: change palette for scale_color_brewer() to get rid of the warning "In RColorBrewer::brewer.pal(n, pal) : n too large, allowed maximum for palette Set1 is 9 Returning the palette you asked for with that many colors"
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
#newmap <- getMap(resolution = "low")
#pdf("map_of_studies.pdf",pointsize=8)
#plot(newmap, main="Distribution of LUBDES studies included in the meta analysis")
#points(data$lon, data$lat, col = "blue", cex = .6, pch=2)
#dev.off()

# ### plot study locations using fancy ggplot - Does NOT work properly #MB - works again! # KG

world_map <- map_data("world")
p <- ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="white",color="black") + 
  geom_point(data=data, aes(x=longitude..E..W., y=latitude..N..S.), color="blue")
p ## looks weird, the reason is the max latitude in data = 2011! - check!
ggsave("CaseDistribution.png", width=8, height=8, units="cm")

############################################################################
### 03.3. Plot cross-diagrams
### 
############################################################################

### plot all in one rush
for(choose.moderator in as.character(unique(MA.coeffs$Moderator))){
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
      scale_colour_brewer(palette="Set1",labels=paste(levels(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)]))," (",table(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), ")", sep="")) +
      ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=choose.moderator)
    print(plot)
  }
  
  if(nrow(ES.moderator.subset) == 1){
   plot <- ggplot() + 
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

############################################################################
### 03.4. Forest plots for noLU vs low/medium/high LU
### 
############################################################################

### plot all in one rush
#TO DO: Plot for no moderators
for(choose.moderator in as.character(unique(MA.coeffs.noLU$Moderator))[-1]){
  ES.moderator.subset <- subset(MA.coeffs.noLU, Moderator %in% choose.moderator)
  ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
  ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
  
  if(nrow(ES.moderator.subset) >= 2){
    plot <- ggplot(ES.moderator.subset, aes(x=paste(levels(factor(ES.frame.noLU[,which(names(ES.frame.noLU) %in% choose.moderator)]))," (",table(factor(ES.frame.noLU[,which(names(ES.frame.noLU) %in% choose.moderator)])), ")", sep=""), y=mean.Richness, ymin=mean.Richness-1.96*se.Richness, ymax=mean.Richness+1.96*se.Richness)) + 
      geom_pointrange() + 
      coord_flip() +
      geom_hline(x=0, linetype="twodash") + # weird: draws the line at x=0!!
      scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
      ylab("Response Ratio") +
      xlab(ES.moderator.subset$Moderator)  #switch because of the coord_flip() above
    print(plot)
  }
  
  ggsave(plot, file = paste("ForestPlot",choose.moderator,".png",sep=""), width = 15, height = 8, type = "cairo-png")
  
}
