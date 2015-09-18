##################################
## set the stage
library(metafor)
library(ggplot2)
library(scales)

setwd("C:\\Users\\hoppek\\Documents\\GitHub\\LUBDES_MA") #KG

### load data
ES.frame <- read.csv("ES_table.csv")
MA.coeffs = read.csv("Output\\MA.coeffs.csv")
ES.frame = read.csv("Input/ES_table.csv")

ES.frame.posVar <- ES.frame[ES.frame$Richness.Log.RR.Var>0 & ES.frame$Yield.Log.RR.Var>0,] # restrict analysis to study cases with positive variances
ES.frame <- ES.frame.posVar
ES.frame$LUI.range <- factor(ES.frame$LUI.range)


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

ggsave(plot, file = paste("Output/Cross_diagram_",choose.moderator,".png",sep=""), width = 15, height = 8, type = "cairo-png")

}

