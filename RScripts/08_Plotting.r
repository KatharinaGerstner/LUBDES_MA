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

### plot maps and histograms of covariates

### NPP

#convert the raster to points for plotting
npp.p <- rasterToPoints(npp)

#Make the points a dataframe for ggplot
df <- data.frame(npp.p)
#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "NPP")

#Call in point data, in this case a fake transect (csv file with lat and lon coordinates)
#sites <- data.frame(read.csv(“/your/path/to/pointfile.csv”))

#Now make the map
ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=NPP)) +
  #geom_point(data=sites, aes(x=x, y=y), color=”white”, size=3, shape=4) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient("NPP", limits=c(0,1500)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
#         scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90° W", "0", "90° E")) +
#         scale_y_continuous(breaks=c(-60, -30, 0, 30, 60), labels=c("60°S", "30°S","0°", "30°N","60°N")),
        legend.key = element_blank()
  )

### TODO: add second histogram in background showing distribution of all npp points, also check coordinate of the Zero case

ggplot(data=ES.frame) + 
  #geom_histogram(aes(x=df$MAP[df$MAP>0]), size=0.4) + 
  geom_histogram(aes(x=ES.frame$npp), size=0.4) + 
  xlab("NPP") +
  ggtitle("NPP") + 
  theme(axis.title = element_text(size = rel(2)), axis.text = element_text(size = rel(2)),plot.title=element_text(size = rel(2)) , axis.text.x=element_text(angle=45,vjust = 1, hjust=1),legend.text=element_text(size = rel(2)),legend.title=element_text(size = rel(2)))
#,axis.ticks.length=unit(.4,"cm")
# print(p)
# ggsave(p, file = paste(path2temp,"/PlotHist_ESframe_",gsub(".","",colnames(ES.frame[i]),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")

### Agricultural Stock

# world <- map_data("world")
# 
# #world <- spTransform(world, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# capitalstock_per_country <- capital_stock_in_agriculture
# map <- fortify(world, region="region")
# map$CountryCode <- countrycode(map$region,"country.name","iso3c")
# 
# gg <- ggplot()
# gg <- gg + 
#   geom_polygon(data=world_map, aes(x=long, y=lat, group=group),fill="white",color="black",lwd=0.2)
#   
# gg +
#   geom_map(data=map, map=map)#,
#                     aes(x=long, y=lat, map_id=id, group=group),
#                     fill="#ffffff", color=NA)
# gg <- gg + geom_map(data=capitalstock_per_country, map=map, color="white", size=0.15,
#                     aes(fill=CountryCode, group=CountryCode, map_id=CountryCode))
# #gg <- gg + geom_point(data=c_labs, aes(x=lon, y=lat), size=4)
# gg <- gg + scale_fill_gradient(low="#f7fcb9", high="#31a354", name="Capital Stock per Country and Agricultural Area")
# #gg <- gg + labs(title="2013 Population")
# gg <- gg + coord_equal(ratio=1)
# #gg <- gg + theme_map()
# gg <- gg + theme(legend.position="bottom")
# gg <- gg + theme(legend.key = element_blank())
# gg <- gg + theme(plot.title=element_text(size=16))
# gg

### Habitat Heterogeneity

#convert the raster to points for plotting
habitat_dissimilarity.p <- rasterToPoints(habitat_dissimilarity)

#Make the points a dataframe for ggplot
df <- data.frame(habitat_dissimilarity.p)

#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "Heterogeneity")
df$Heterogeneity[df$Heterogeneity==2147483647]<-NA
#Call in point data, in this case a fake transect (csv file with lat and lon coordinates)
#sites <- data.frame(read.csv(“/your/path/to/pointfile.csv”))

#Now make the map
ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Heterogeneity)) +
  #geom_point(data=sites, aes(x=x, y=y), color=”white”, size=3, shape=4) +
  theme_bw() +
  coord_equal() +
  scale_fill_gradient("Heterogeneity", limits=c(0,130000)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        #         scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90° W", "0", "90° E")) +
        #         scale_y_continuous(breaks=c(-60, -30, 0, 30, 60), labels=c("60°S", "30°S","0°", "30°N","60°N")),
        legend.key = element_blank()
  )

### TODO: add second histogram in background showing distribution of all npp points, also check coordinate of the Zero case
ES.frame$habitat_dissimilarity[ES.frame$habitat_dissimilarity==2147483647]<-NA
ggplot(data=ES.frame) + 
  #geom_histogram(aes(x=df$MAP[df$MAP>0]), size=0.4) + 
  geom_histogram(aes(x=ES.frame$habitat_dissimilarity), size=0.4) + 
  xlab("Habitat Dissimilarity") +
  ggtitle("Habitat Dissimilarity") + 
  theme(axis.title = element_text(size = rel(2)), axis.text = element_text(size = rel(2)),plot.title=element_text(size = rel(2)) , axis.text.x=element_text(angle=45,vjust = 1, hjust=1),legend.text=element_text(size = rel(2)),legend.title=element_text(size = rel(2)))
#,axis.ticks.length=unit(.4,"cm")
# print(p)
# ggsave(p, file = paste(path2temp,"/PlotHist_ESframe_",gsub(".","",colnames(ES.frame[i]),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")

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

##################
### RESTERAMPE ###
##################


# 