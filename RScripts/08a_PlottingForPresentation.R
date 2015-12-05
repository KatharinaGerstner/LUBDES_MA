############################################################################
### Purpose of this skript module 08a is to:
###
### 08a.1. Plot map of studies
### 08a.2. Plot cross-diagrams
### 08a.3. Forest plots for noLU vs low/medium/high LU
###
### General comments:
### * TO DO: change palette for scale_color_brewer() to get rid of the warning "In RColorBrewer::brewer.pal(n, pal) : n too large, allowed maximum for palette Set1 is 9 Returning the palette you asked for with that many colors"
###
### Authors: KG, MB, SK ...
############################################################################

### show current working directory and check if its local
getwd()

############################################################################
### 08a.1. Plot map of studies
### 
############################################################################

### plot study locations using the "classic" way
# newmap <- getMap(resolution = "low")
# png(paste(path2temp, "/CaseDistribution.png",sep=""),width=24,height=16,units="cm",res=200)
# plot(newmap)
# points(data$lon, data$lat, col = "blue", cex = .8, pch=17)
# dev.off()
# 
### plot study locations using fancy ggplot
world_map <- map_data("world")
ES.frame <- within(ES.frame, LUI.range.level <- factor(LUI.range.level, levels = c("low-low","low-medium","low-high","medium-medium","medium-high","high-high"))) # resort levels for plotting
p <- ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),fill="white",color="black",lwd=0.3) + 
  geom_point(data=ES.frame, aes(x=Longitude, y=Latitude, ymin=-55), color="blue", shape=2, size=1) +
  xlab("") + ylab("") +
  facet_wrap(~LUI.range.level)   
p 
ggsave(paste(path2temp, "/CaseDistribution.png",sep=""), width=18, height=10, units="cm")

############################################################################
### 08a.2. Plot cross-diagrams
### 
############################################################################

### plot cross diagrams for categorical moderators
for(choose.moderator in as.character(unique(MA.coeffs.cat$Moderator))){
  ES.moderator.subset <- subset(MA.coeffs.cat, Moderator %in% choose.moderator)
  ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
  ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
  
  ### plot cross diagrams
  if(nrow(ES.moderator.subset) >= 2){
    plot <- ggplot() + 
      geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), size=4, alpha=.5) +
      geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness - (1.96*se.Richness), ymax=mean.Richness + (1.96*se.Richness),color=factor(levels)), size=1.5) +
      geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness, color=factor(levels)),size=1.5) +
      geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
      scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
      scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
      xlim(-2,2) + ylim(-1,1) + # zoom into the image
      scale_colour_brewer(palette="Set1",labels=paste(levels(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)]))," (",table(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), ")", sep="")) +
      ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=choose.moderator) +
      theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
    print(plot)
  }
  
  if(nrow(ES.moderator.subset) == 1){
   plot <- ggplot() + 
      geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR), color="grey", size=3.5, alpha=.5) +
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

ggsave(plot, file = paste(path2temp,"/Cross_diagram_",gsub(".","",choose.moderator,fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")

}

### plot scatterplots for continuous moderators
for(i in 2:nrow(MA.coeffs.cont)){
  mods <- MA.coeffs.cont$Moderator[i]
  print(mods)
  ES.moderator.subset <- MA.coeffs.cont[i,]
  if(all(is.na(ES.moderator.subset[-1]))) next
  
  ## Transform prediction list from rma to dataframe
  richness.preds.df <- data.frame(slab=preds.richness[[mods]][[1]]$slab, pred.richness=preds.richness[[mods]][[1]]$pred, ci.lb.richness=preds.richness[[mods]][[1]]$ci.lb, ci.ub.richness=preds.richness[[mods]][[1]]$ci.ub)
  yield.preds.df <- data.frame(slab=preds.yield[[mods]][[1]]$slab, pred.yield=preds.yield[[mods]][[1]]$pred, ci.lb.yield=preds.yield[[mods]][[1]]$ci.lb, ci.ub.yield=preds.yield[[mods]][[1]]$ci.ub)
  
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
  
  plot1 <- ggplot(data=pred.frame.trans, aes(x=pred.frame.trans[,paste(mods)])) + 
    geom_point(aes(y=Log.RR, color=Response), size=3.5, alpha=.5) +
    geom_abline(intercept=ES.moderator.subset$Richness.intercept, slope=ES.moderator.subset$Richness.slope,color="red") +
    geom_abline(intercept=ES.moderator.subset$Yield.intercept, slope=ES.moderator.subset$Yield.slope,color="blue") +
    geom_ribbon(aes(ymin=ci.lb.richness,ymax=ci.ub.richness),fill="red",alpha=0.2) +
    geom_ribbon(aes(ymin=ci.lb.yield,ymax=ci.ub.yield),fill="blue",alpha=0.2) +
    geom_hline(x=0, linetype="twodash") + 
    scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
    scale_colour_manual(values=c("red","blue"),labels=c("Richness","Yield")) +
    ylab("RR")  + xlab(paste(mods)) +
    theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
  
  print(plot1)
  ggsave(plot1, file = paste(path2temp, "/Scatterplot_",gsub(".","",MA.coeffs.cont$Moderator[i],fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
}

############################################################################
### 08a.3. Forest plots for noLU vs low/medium/high LU
### 
############################################################################

### plot all in one rush
#TO DO: Plot for no moderators
for(choose.moderator in as.character(unique(MA.coeffs.noLU$Moderator))[-1]){
  ES.moderator.subset <- subset(MA.coeffs.noLU, Moderator %in% choose.moderator)
  ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
  ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
  
  if(nrow(ES.moderator.subset) >= 2){
    ifelse(choose.moderator=="Product:High.LUI",n.levels <- table(paste(ES.frame.noLU$Product,ES.frame.noLU$High.LUI,sep=":"))[length(n.levels):1], n.levels <- table(factor(ES.frame.noLU[,which(names(ES.frame.noLU) %in% choose.moderator)])))
    plot <- ggplot(ES.moderator.subset, aes(x=paste(levels," (",n.levels, ")", sep=""), y=mean.Richness, ymin=mean.Richness-1.96*se.Richness, ymax=mean.Richness+1.96*se.Richness)) + 
      geom_pointrange(size=1.2) + 
      coord_flip() +
      geom_hline(x=0, linetype="twodash") + # weird: draws the line at x=0!!
      scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
      ylab("Response Ratio") +
      xlab(ES.moderator.subset$Moderator)  + #switch because of the coord_flip() above 
      theme(axis.title = element_text(size = rel(1.8)), axis.text = element_text(size = rel(1.8)),legend.text=element_text(size = rel(1.8)),legend.title=element_text(size = rel(1.8)))
    print(plot)
  }
  
  ggsave(plot, file = paste(path2temp, "/ForestPlot",gsub(":","",gsub(".","",choose.moderator,fixed=T),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
  
}

##################
### RESTERAMPE ###
##################


# 