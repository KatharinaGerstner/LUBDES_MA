############################################################################
### Purpose of this skript module 08 is to:
###
### 08.1. Plot map of studies
### 08.2. Plot cross-diagrams
### 08.4. Forest plots for noLU vs low/medium/high LU
###
### General comments:
### * TO DO: change palette for scale_color_brewer() to get rid of the warning "In RColorBrewer::brewer.pal(n, pal) : n too large, allowed maximum for palette Set1 is 9 Returning the palette you asked for with that many colors"
###
### Authors: KG, MB, SK ...
############################################################################

### show current working directory and check if its local
getwd()

############################################################################
### 08.1. Plot map of studies
### 
############################################################################

### plot study locations using the "classic" way
# newmap <- getMap(resolution = "low")
# png(paste(path2temp, "/CaseDistribution.png",sep=""),width=24,height=16,units="cm",res=200)
# plot(newmap)
# points(data$lon, data$lat, col = "blue", cex = .8, pch=17)
# dev.off()
# 

world_map <- map_data("world")
p <- ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),fill="white",color="black",lwd=0.2) + 
  geom_point(data=ES.frame, aes(x=Longitude, y=Latitude, ymin=-55), color="blue", shape=4, size=1) +
  scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90° W", "0", "90° E")) +
  scale_y_continuous(breaks=c(-60, -30, 0, 30, 60), labels=c("60°S", "30°S","0°", "30°N","60°N")) +
  xlab("") + ylab("") 
p 
ggsave(paste(path2temp, "/CaseDistributionAll.png",sep=""), width=18, height=10, units="cm")

### plot study locations using fancy ggplot
world_map <- map_data("world")
ES.frame <- within(ES.frame, LUI.range.level <- factor(LUI.range.level, levels = c("low-low","low-medium","low-high","medium-medium","medium-high","high-high"))) # resort levels for plotting
p <- ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),fill="white",color="black",lwd=0.2) + 
  geom_point(data=ES.frame, aes(x=Longitude, y=Latitude, ymin=-55), color="blue", shape=2, size=1) +
  scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90° W", "0", "90° E")) +
  scale_y_continuous(breaks=c(-60, -30, 0, 30, 60), labels=c("60°S", "30°S","0°", "30°N","60°N")) +
  xlab("") + ylab("") +
  facet_wrap(~LUI.range.level)   
p 
ggsave(paste(path2temp, "/CaseDistribution.png",sep=""), width=18, height=10, units="cm")

############################################################################
### 08.2. Plot cross-diagrams
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
   
  Richness.reg.line <- function(xvar){
    ES.moderator.subset$Richness.intercept+ES.moderator.subset$Richness.slope*xvar
  }
  Yield.reg.line <- function(xvar){
    ES.moderator.subset$Yield.intercept+ES.moderator.subset$Yield.slope*xvar
  }
  
  plot1 <- ggplot() + 
    geom_point(aes(x=ES.frame.richness[,paste(mods)],y=ES.frame.richness$Richness.Log.RR),color="red", size=3.5, alpha=.5) +
    geom_point(aes(x=ES.frame.yield[,paste(mods)],y=ES.frame.yield$Yield.Log.RR),color="blue", size=3.5, alpha=.5) +
    geom_abline(x=ES.frame.richness[,paste(mods)],intercept=ES.moderator.subset$Richness.intercept, slope=ES.moderator.subset$Richness.slope,color="red") +
    geom_abline(aes(x=ES.frame.yield[,paste(mods)]),intercept=ES.moderator.subset$Yield.intercept, slope=ES.moderator.subset$Yield.slope,color="blue") +
    geom_ribbon(aes(x=ES.frame.richness[!is.na(ES.frame.richness[,paste(mods)]),paste(mods)],ymin=richness.preds.df$ci.lb.richness,ymax=richness.preds.df$ci.ub.richness),fill="red",alpha=0.2) +
    geom_ribbon(aes(x=ES.frame.yield[!is.na(ES.frame.yield[,paste(mods)]),paste(mods)],ymin=yield.preds.df$ci.lb.yield,ymax=yield.preds.df$ci.ub.yield),fill="blue",alpha=0.2) +
    geom_hline(y=0, linetype="twodash") + 
    scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
    scale_colour_manual(values=c("red","blue"),labels=c("Richness","Yield")) +
    ylab("RR")  + xlab(paste(mods)) +
    theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
  
  print(plot1)
  ggsave(plot1, file = paste(path2temp, "/Scatterplot_",gsub(".","",MA.coeffs.cont$Moderator[i],fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
}

############################################################################
### 08.3. Forest plots for noLU vs low/medium/high LU
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



############################
### SK Annapolis WS 2015 ###
### cross diagramm with only 3 groups: low-low, medium-medium, high-high
############################
### ------------------------

# select dataset and moderator grand means to plot
data.to.plot = ES.frame[which(ES.frame$Low.LUI == ES.frame$High.LUI),]
moderator.grand.mean.subset = MA.coeffs.cat[which(MA.coeffs.cat$Moderator %in% "LUI.range.level" & MA.coeffs.cat$levels %in% c("low-low","medium-medium","high-high")),]

# order levels of moderators so they are displayed in same colour
moderator.grand.mean.subset$levels = factor(moderator.grand.mean.subset$levels, levels=c("low-low","medium-medium","high-high"))
data.to.plot$LUI.range.level = factor(data.to.plot$LUI.range.level, levels=c("low-low","medium-medium","high-high"))

#set colour scheme here ! 3 for points and 3 for crosses
colours.to.plot = c("#33FF00","#FFCC00","#FF0000","#33FF00","#FFCC00","#FF0000")

# get axes length to center the plot 
if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
  y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
}else{
  y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
}

if(0 - min(data.to.plot$Yield.Log.RR) > max(data.to.plot$Yield.Log.RR)){
  x.range.for.plot = c(0 -max(data.to.plot$Yield.Log.RR) - 0.5,max(data.to.plot$Yield.Log.RR) + 0.5 )
}else{
  x.range.for.plot = c(min(data.to.plot$Yield.Log.RR)-0.5,0-min(data.to.plot$Yield.Log.RR) + 0.5)
}

plot.within.groups.cross.diagramm =  ggplot() + 

  # study cases as points
  geom_point(data= data.to.plot, aes(x=Yield.Log.RR, y=Richness.Log.RR,colour=LUI.range.level), size=3.5,alpha = 0.5) +
  
  #grand mean for moderators
  geom_pointrange(data=moderator.grand.mean.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness),colour=levels), size=1) +
  geom_segment(data=moderator.grand.mean.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness,colour=levels), size=1) +

  # colouring
  scale_colour_manual(values=colours.to.plot,
                      breaks=c("low-low","medium-medium","high-high"),
                      labels=c("low-low","medium-medium","high-high")) +
  
  # lines for zero response rations
  geom_hline(data=ES.frame, x=0, linetype="twodash",colour="grey") + 
  geom_vline(data=ES.frame, y=0, linetype="twodash",colour="grey") +
    
  #white background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
  
  scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +

  #legend
  theme(legend.position="top",legend.title=element_blank())   +

  # place the zero-zero lines in the middle of the plot
  coord_cartesian(ylim= y.range.for.plot, xlim=x.range.for.plot)

print(plot.within.groups.cross.diagramm)
ggsave(plot.within.groups.cross.diagramm, file = paste(path2temp, "/Cross_diagram_within_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")

############################
### SK Annapolis WS 2015 ###
### cross diagramm with only 3 groups: low-medium, low-high, medium-high
############################
### ------------------------



# select dataset and moderator grand means to plot
data.to.plot = ES.frame[which(!(ES.frame$Low.LUI == ES.frame$High.LUI)),]
moderator.grand.mean.subset = MA.coeffs.cat[which(MA.coeffs.cat$Moderator %in% "LUI.range.level" & MA.coeffs.cat$levels %in% c("low-medium","medium-high","low-high")),]

# order levels of moderators so they are displayed in same colour
moderator.grand.mean.subset$levels = factor(moderator.grand.mean.subset$levels, levels=c("low-medium","medium-high","low-high"))
data.to.plot$LUI.range.level = factor(data.to.plot$LUI.range.level, levels=c("low-medium","medium-high","low-high"))

#set colour scheme here ! 3 for points and 3 for crosses
colours.to.plot = c("#3399FF","#FFCC00","#FF0000","#3399FF","#FFCC00","#FF0000")

# get axes length to center the plot 
if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
  y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
}else{
  y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
}

if(0 - min(data.to.plot$Yield.Log.RR) > max(data.to.plot$Yield.Log.RR)){
  x.range.for.plot = c(0 -max(data.to.plot$Yield.Log.RR) - 0.5,max(data.to.plot$Yield.Log.RR) + 0.5 )
}else{
  x.range.for.plot = c(min(data.to.plot$Yield.Log.RR)-0.5,0-min(data.to.plot$Yield.Log.RR) + 0.5)
}

plot.across.groups.cross.diagramm =  ggplot() + 
  
  # study cases as points
  geom_point(data= data.to.plot, aes(x=Yield.Log.RR, y=Richness.Log.RR,colour=LUI.range.level), size=3.5,alpha = 0.5) +
  
  #grand mean for moderators
  geom_pointrange(data=moderator.grand.mean.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness),colour=levels), size=1) +
  geom_segment(data=moderator.grand.mean.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness,colour=levels), size=1) +
  
  # colouring
  scale_colour_manual(values=colours.to.plot,
                      breaks=c("low-medium","medium-high","low-high"),
                      labels=c("low-medium","medium-high","low-high")) +
  
  # lines for zero response rations
  geom_hline(data=ES.frame, x=0, linetype="twodash",colour="grey") + 
  geom_vline(data=ES.frame, y=0, linetype="twodash",colour="grey") +
  
  #white background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
  
  scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +
  
  #legend
  theme(legend.position="top",legend.title=element_blank())   +
  
  # place the zero-zero lines in the middle of the plot
  coord_cartesian(ylim= y.range.for.plot, xlim=x.range.for.plot)

print(plot.across.groups.cross.diagramm)
ggsave(plot.across.groups.cross.diagramm, file = paste(path2temp, "Cross_diagram_across_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")


############################
### SK Annapolis WS 2015 ###
### forest plots for each LUI-comparison class
############################
### ------------------------

LUI.range.level = c("low-low","medium-medium","high-high","low-medium","medium-high","low-high")


LUI.level.to.plot = LUI.range.level[1]

for(LUI.level.to.plot in LUI.range.level){
  data.to.plot = subset(ES.frame, LUI.range.level %in% LUI.level.to.plot)
  data.to.plot = data.to.plot[order(data.to.plot$Yield.Log.RR,decreasing=F),]
  data.to.plot$uniqueID = factor(paste(data.to.plot$Study.ID,data.to.plot$Case.ID),levels=paste(data.to.plot$Study.ID,data.to.plot$Case.ID))

  
  # get axes length to center the plot 
  if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
    y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
  }else{
    y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
  }
  
  
plot.forest = ggplot(data=data.to.plot) +
  
  geom_pointrange(aes(x=uniqueID, y=Yield.Log.RR, ymin=Yield.Log.RR	- (1.96*Yield.Log.RR.Var), ymax=Yield.Log.RR	+ (1.96*Yield.Log.RR.Var),colour="Yield"), size=1) +
  geom_pointrange(aes(x=uniqueID, y=Richness.Log.RR, ymin=Richness.Log.RR	- (1.96*Richness.Log.RR.Var), ymax=Richness.Log.RR	+ (1.96*Richness.Log.RR.Var),colour="Species Richness"), size=1) +
  geom_hline(x=0,linetype ="twodash")  +

  
  #scale manually to get the legend correct
  scale_colour_manual(values  =c("#00CC00","#FF6633")) +
  
  #white background + flip 90 degrees
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
  coord_flip(ylim=y.range.for.plot)

  print(plot.forest)
  ggsave(plot.forest, file = paste(c(path2temp, "Forest_plot_",LUI.level.to.plot,".png"), collapse=""), width = 20, height = 8, type = "cairo-png")

}

##################
### RESTERAMPE ###
##################
