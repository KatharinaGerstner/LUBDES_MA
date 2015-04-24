##################################
## set the stage
library(metafor)
library(ggplot2)
library(scales)
#insert your wd here:
#SK:
setwd("c:/Users/kambach/Dropbox/sDiv_workshop/Meta-Analysis/DataAnalysis") #KG

#load data
ES.frame <- read.csv("ES_table.csv")
MA.coeffs = read.csv("Output\\MA.coeffs.csv")
ES.frame <- ES.frame[-which(ES.frame$Study.ID=="4788-Mosquera-Losada2009"),] 
#ES.frame$LUI.range = as.factor(paste(ES.frame$Low.LUI,ES.frame$LUI.range,sep="_"))




#choose.moderator = "Trophic.Level"
#ES.moderator.subset = subset(MA.coeffs, moderator %in% choose.moderator)

#plot all in one rush
for(choose.moderator in as.character(unique(MA.coeffs$Moderator))){
  ES.moderator.subset = subset(MA.coeffs, Moderator %in% choose.moderator)
  ES.moderator.subset$Moderator = factor(ES.moderator.subset$Moderator)
  ES.moderator.subset$levels = factor(ES.moderator.subset$levels)
  
  if(nrow(ES.moderator.subset) >= 2){
    pdf(paste("Cross_diagram_",choose.moderator,".pdf",sep=""),width=15,height=8)
plot =   ggplot() + 
      geom_blank(data=ES.frame,aes(x=Yield.Log.RR,y=Richness.Log.RR),) + 
      geom_point(data=ES.frame,aes(x=Yield.Log.RR,y=Richness.Log.RR,color=as.factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])),size=3.5) +
      geom_pointrange(data=ES.moderator.subset,aes(x=mean.Yield, y=mean.Richness	, ymin=mean.Richness	 - (1.96*se.Richness), ymax=mean.Richness	 + (1.96*se.Richness),color=factor(levels)),size=1) +
      geom_segment(data=ES.moderator.subset,aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield),y = mean.Richness	, yend = mean.Richness	,color=factor(levels)),size=1) +
      geom_hline(data=ES.frame,x=0,linetype="twodash") + geom_vline(data=ES.frame,y=0,linetype="twodash") +
      #    scale_fill_discrete(name="paste") +
      scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
      scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
      ylab("RR (Species Richness)") + xlab("RR (Yield)")  + scale_colour_brewer(palette="Set1")
    print(plot)
    dev.off()
  }
  
  if(nrow(ES.moderator.subset) == 1){
    
    pdf(paste("Cross_diagram_",choose.moderator,".pdf",sep=""),width=15,height=8)
   ggplot() + 
      geom_blank(data=ES.frame,aes(x=Yield.Log.RR,y=Richness.Log.RR)) + 
      geom_point(data=ES.frame,aes(x=Yield.Log.RR,y=Richness.Log.RR),color="grey",size=3.5) +
      geom_pointrange(data=ES.moderator.subset,aes(x=mean.Yield, y=mean.Richness  , ymin=mean.Richness	 - (1.96*se.Richness), ymax=mean.Richness	 + (1.96*se.Richness)),color="green",size=1) +
      geom_segment(data=ES.moderator.subset,aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield),y = mean.Richness	, yend = mean.Richness),color="green",size=1) +
      geom_hline(data=ES.frame,x=0,linetype="twodash") + geom_vline(data=ES.frame,y=0,linetype="twodash") +
      #    scale_fill_discrete(name="paste") +
      scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
      scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
      ylab("RR (Species Richness)") + xlab("RR (Yield)")  + scale_colour_brewer(palette="Set1")
    print(plot)
    dev.off()
  }
}

