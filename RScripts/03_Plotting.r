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
  geom_point(data=data, aes(x=as.numeric(longitude..E..W.), y=as.numeric(latitude..N..S.)), color="blue")
p ## looks weird, the reason is the max latitude in data = 2011! - check!
ggsave(path2temp %+% "/CaseDistribution.png", width=8, height=8, units="cm")

############################################################################
### 03.3. Plot cross-diagrams
### 
############################################################################

### plot all in one rush
for(choose.moderator in as.character(unique(MA.coeffs.cat$Moderator))){
  ES.moderator.subset <- subset(MA.coeffs.cat, Moderator %in% choose.moderator)
  ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
  ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
  
  if(nrow(ES.moderator.subset) >= 2){
    plot <- ggplot() + 
      geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), size=4) +
      geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness - (1.96*se.Richness), ymax=mean.Richness + (1.96*se.Richness),color=factor(levels)), size=1.5) +
      geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness, color=factor(levels)),size=1.5) +
      geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
      scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
      scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
      scale_colour_brewer(palette="Set1",labels=paste(levels(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)]))," (",table(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), ")", sep="")) +
      ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=choose.moderator) +
      theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
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
      ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
      theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))    
   
   print(plot)
  }

ggsave(plot, file = paste(path2temp %+% "/Cross_diagram_",gsub(".","",choose.moderator,fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")

}

for(i in 2:nrow(MA.coeffs.cont)){
  print(MA.coeffs.cont$Moderator[i])
  ES.moderator.subset <- MA.coeffs.cont[i,]
  if(all(is.na(ES.moderator.subset))) next
  
  ## Transform prediction list from rma to dataframe
  richness.preds.df <- data.frame(slab=preds.richness[[i]]$slab, pred.richness=preds.richness[[i]]$pred, cr.lb.richness=preds.richness[[i]]$cr.lb, cr.ub.richness=preds.richness[[i]]$cr.ub)
  yield.preds.df <- data.frame(slab=preds.yield[[i]]$slab, pred.yield=preds.yield[[i]]$pred, cr.lb.yield=preds.yield[[i]]$cr.lb, cr.ub.yield=preds.yield[[i]]$cr.ub)
  
  ### combine ES.frame with predictions dataframe
  pred.frame <- subset(ES.frame,Study.Case %in% sapply(as.character(richness.preds.df$slab),function(x) strsplit(x,"_")[[1]][1]))
  pred.frame$slab <- paste(pred.frame$Study.Case, pred.frame$Low.LUI, pred.frame$High.LUI,sep="_")
  pred.frame <- join_all(list(pred.frame,richness.preds.df,yield.preds.df),by="slab")
  
  ### Transform predictions dataframe so that there is one column Response (Richness, Yield) and in another the corresponding logRR
  pred.frame.trans <- rbind(pred.frame,pred.frame)
  pred.frame.trans$Response <- c(rep("Richness",nrow(pred.frame)),rep("Yield",nrow(pred.frame)))
  pred.frame.trans$Log.RR <- c(pred.frame$Richness.Log.RR,pred.frame$Yield.Log.RR)
  
  Richness.reg.line <- function(xvar){
    ES.moderator.subset$Richness.intercept+ES.moderator.subset$Richness.slope*xvar
  }
  Yield.reg.line <- function(xvar){
    ES.moderator.subset$Yield.intercept+ES.moderator.subset$Yield.slope*xvar
  }
  
  plot1 <- ggplot(data=pred.frame.trans, aes(x=GDP.pc.2000)) + 
    geom_point(aes(y=Log.RR, color=Response), size=3.5) +
    geom_abline(intercept=ES.moderator.subset$Richness.intercept, slope=ES.moderator.subset$Richness.slope,color="red") +
    geom_abline(intercept=ES.moderator.subset$Yield.intercept, slope=ES.moderator.subset$Yield.slope,color="blue") +
    geom_ribbon(aes(ymin=cr.lb.richness,ymax=cr.ub.richness),fill="red",alpha=0.2) +
    geom_ribbon(aes(ymin=cr.lb.yield,ymax=cr.ub.yield),fill="blue",alpha=0.2) +
    scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
    scale_colour_manual(values=c("red","blue"),labels=c("Richness","Yield")) +
    ylab("RR")  +
    theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
  
  print(plot1)
  ggsave(plot1, file = paste(path2temp %+% "/Scatterplot_",gsub(".","",MA.coeffs.cont$Moderator[i],fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
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
      geom_pointrange(size=1.2) + 
      coord_flip() +
      geom_hline(x=0, linetype="twodash") + # weird: draws the line at x=0!!
      scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
      ylab("Response Ratio") +
      xlab(ES.moderator.subset$Moderator)  + #switch because of the coord_flip() above 
      theme(axis.title = element_text(size = rel(1.8)), axis.text = element_text(size = rel(1.8)),legend.text=element_text(size = rel(1.8)),legend.title=element_text(size = rel(1.8)))
    print(plot)
  }
  
  ggsave(plot, file = paste(path2temp %+% "/ForestPlot",gsub(".","",choose.moderator,fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
  
}

