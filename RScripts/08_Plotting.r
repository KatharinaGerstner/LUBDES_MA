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

modelDataR <- ES.frame.richness[,c('Richness.Log.RR','Richness.Log.RR.Var','Species.Group','LUI.range.level','Product','BIOME',
                                  'rel_capital_stock_in_agriculture',
                                  'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]
modelDataR <- na.omit(modelDataR)

modelDataY <- ES.frame.yield[,c('Yield.Log.RR','Yield.Log.RR.Var','Species.Group','LUI.range.level','Product','BIOME',
                               'rel_capital_stock_in_agriculture',
                               'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]
modelDataY <- na.omit(modelDataY)

ES.frame$LUI.range.level <- factor(paste(ES.frame$LUI.range.level),levels=levels(modelDataY$LUI.range.level))

predFrame <- data.frame(Species.Group=factor(levels(modelDataR$Species.Group),levels = levels(modelDataR$Species.Group)),
                        LUI.range.level=factor("high-high",levels=levels(modelDataR$LUI.range.level)),
                        BIOME=factor("Tropical Forests",levels=levels(modelDataR$BIOME)))

newMods <- model.matrix(~Species.Group + LUI.range.level + BIOME,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsR <- predict.rma(RichnessModel$model,newmods = newMods)
predsR<-data.frame(pred=predsR$pred,se=predsR$se)
predsR$Species.Group <- factor(levels(modelDataR$Species.Group),levels = levels(modelDataR$Species.Group))

predsY <- data.frame(pred=rep(coefficients(Yield.MA.fit),dim(predsR)[1]),
                     se=rep(Yield.MA.fit$se,dim(predsR)[1]))

predsY$Species.Group <- factor(levels(modelDataR$Species.Group),levels = levels(modelDataR$Species.Group))

plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,'Species.Group'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                  ymax=pred + (1.96*se),color=Species.Group), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=Species.Group),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$Species.Group)," (",table(factor(modelDataR[,'Species.Group'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='Species.Group') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_Species.Group.png",sep=""), width = 20, height = 8, type = "cairo-png")

predFrame <- data.frame(Species.Group=factor("invertebrates",levels=levels(modelDataR$Species.Group)),
                        LUI.range.level=factor(levels(modelDataR$LUI.range.level),levels=levels(modelDataR$LUI.range.level)),
                        BIOME=factor("Tropical Forests",levels=levels(modelDataR$BIOME)))

newMods <- model.matrix(~Species.Group + LUI.range.level + BIOME,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsR <- predict.rma(RichnessModel$model,newmods = newMods)
predsR<-data.frame(pred=predsR$pred,se=predsR$se)
predsR$LUI.range.level <- factor(levels(modelDataR$LUI.range.level),levels = levels(modelDataR$LUI.range.level))

predFrame <- data.frame(LUI.range.level=factor(levels(modelDataR$LUI.range.level),levels=levels(modelDataR$LUI.range.level)),
                        Product=factor("crop",levels=levels(modelDataR$Product)))

newMods <- model.matrix(~LUI.range.level + Product,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsY <- predict.rma(YieldModel$model,newmods = newMods)
predsY<-data.frame(pred=predsY$pred,se=predsY$se)
predsY$LUI.range.level <- factor(levels(modelDataY$LUI.range.level),levels = levels(modelDataY$LUI.range.level))

plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=factor(ES.frame[,'LUI.range.level'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                   ymax=pred + (1.96*se),color=LUI.range.level), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=LUI.range.level),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$LUI.range.level)," (",table(factor(modelDataR[,'LUI.range.level'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='LUI.range.level') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_LUI.range.level.png",sep=""), width = 20, height = 8, type = "cairo-png")


predFrame <- data.frame(LUI.range.level=factor("medium-medium",levels=levels(modelDataR$LUI.range.level)),
                        Product=factor(levels(modelDataR$Product),levels=levels(modelDataR$Product)))

newMods <- model.matrix(~LUI.range.level + Product,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsY <- predict.rma(YieldModel$model,newmods = newMods)
predsY<-data.frame(pred=predsY$pred,se=predsY$se)
predsY$Product <- factor(levels(modelDataY$Product),levels = levels(modelDataY$Product))

predsR <- data.frame(pred=rep(coefficients(Richness.MA.fit),dim(predsY)[1]),
                     se=rep(Richness.MA.fit$se,dim(predsY)[1]))
predsR$Product <- factor(levels(modelDataR$Product),levels = levels(modelDataR$Product))


plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,'Product'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                   ymax=pred + (1.96*se),color=Product), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=Product),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$Product)," (",table(factor(modelDataR[,'Product'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='Product') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_Product.level.png",sep=""), width = 20, height = 8, type = "cairo-png")

predFrame <- data.frame(Species.Group=factor("invertebrates",levels = levels(modelDataR$Species.Group)),
                        LUI.range.level=factor("high-high",levels=levels(modelDataR$LUI.range.level)),
                        BIOME=factor(levels(modelDataR$BIOME),levels=levels(modelDataR$BIOME)))

newMods <- model.matrix(~Species.Group + LUI.range.level + BIOME,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsR <- predict.rma(RichnessModel$model,newmods = newMods)
predsR<-data.frame(pred=predsR$pred,se=predsR$se)
predsR$BIOME <- factor(levels(modelDataR$BIOME),levels = levels(modelDataR$BIOME))

predsY <- data.frame(pred=rep(coefficients(Yield.MA.fit),dim(predsR)[1]),
                     se=rep(Yield.MA.fit$se,dim(predsR)[1]))

predsY$BIOME <- factor(levels(modelDataR$BIOME),levels = levels(modelDataR$BIOME))

plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,'BIOME'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                   ymax=pred + (1.96*se),color=BIOME), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=BIOME),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$BIOME)," (",table(factor(modelDataR[,'BIOME'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='BIOME') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),
        legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_BIOME.png",sep=""), width = 20, height = 8, type = "cairo-png")

# 
# 
# ### plot cross diagrams for categorical moderators
# for(choose.moderator in as.character(unique(MA.coeffs.cat$Moderator))){
#   ES.moderator.subset <- subset(MA.coeffs.cat, Moderator %in% choose.moderator)
#   ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
#   ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
#   
#   ### plot cross diagrams
#   if(nrow(ES.moderator.subset) >= 2){
#     plot <- ggplot() + 
#       geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), size=4, alpha=.5) +
#       geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness - (1.96*se.Richness), ymax=mean.Richness + (1.96*se.Richness),color=factor(levels)), size=1.5) +
#       geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness, color=factor(levels)),size=1.5) +
#       geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
#       scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#       scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
#       scale_colour_brewer(palette="Set1",labels=paste(levels(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)]))," (",table(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), ")", sep="")) +
#       ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=choose.moderator) +
#       theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
#     print(plot)
#   }
#   
#   if(nrow(ES.moderator.subset) == 1){
#    plot <- ggplot() + 
#       geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR), color="grey", size=3.5, alpha=.5) +
#       geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness)), color="green", size=1) +
#       geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness), color="green", size=1) +
#       geom_hline(data=ES.frame, x=0, linetype="twodash") + 
#       geom_vline(data=ES.frame, y=0, linetype="twodash") +
#       scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#       scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
#       ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
#       theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))    
#    
#    print(plot)
#   }
# 
# ggsave(plot, file = paste(path2temp,"/Cross_diagram_",gsub(".","",choose.moderator,fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
# 
# }
# 
# ### plot scatterplots for continuous moderators
# for(i in 2:nrow(MA.coeffs.cont)){
#   mods <- MA.coeffs.cont$Moderator[i]
#   print(mods)
#   ES.moderator.subset <- MA.coeffs.cont[i,]
#   if(all(is.na(ES.moderator.subset[-1]))) next
#   
#   ## Transform prediction list from rma to dataframe
#   richness.preds.df <- data.frame(slab=preds.richness[[mods]][[1]]$slab, pred.richness=preds.richness[[mods]][[1]]$pred, ci.lb.richness=preds.richness[[mods]][[1]]$ci.lb, ci.ub.richness=preds.richness[[mods]][[1]]$ci.ub)
#   yield.preds.df <- data.frame(slab=preds.yield[[mods]][[1]]$slab, pred.yield=preds.yield[[mods]][[1]]$pred, ci.lb.yield=preds.yield[[mods]][[1]]$ci.lb, ci.ub.yield=preds.yield[[mods]][[1]]$ci.ub)
#    
#   Richness.reg.line <- function(xvar){
#     ES.moderator.subset$Richness.intercept+ES.moderator.subset$Richness.slope*xvar
#   }
#   Yield.reg.line <- function(xvar){
#     ES.moderator.subset$Yield.intercept+ES.moderator.subset$Yield.slope*xvar
#   }
#   
#   plot1 <- ggplot() + 
#     geom_point(aes(x=ES.frame.richness[,paste(mods)],y=ES.frame.richness$Richness.Log.RR),color="red", size=3.5, alpha=.5) +
#     geom_point(aes(x=ES.frame.yield[,paste(mods)],y=ES.frame.yield$Yield.Log.RR),color="blue", size=3.5, alpha=.5) +
#     geom_abline(x=ES.frame.richness[,paste(mods)],intercept=ES.moderator.subset$Richness.intercept, slope=ES.moderator.subset$Richness.slope,color="red") +
#     geom_abline(aes(x=ES.frame.yield[,paste(mods)]),intercept=ES.moderator.subset$Yield.intercept, slope=ES.moderator.subset$Yield.slope,color="blue") +
#     geom_ribbon(aes(x=ES.frame.richness[!is.na(ES.frame.richness[,paste(mods)]),paste(mods)],ymin=richness.preds.df$ci.lb.richness,ymax=richness.preds.df$ci.ub.richness),fill="red",alpha=0.2) +
#     geom_ribbon(aes(x=ES.frame.yield[!is.na(ES.frame.yield[,paste(mods)]),paste(mods)],ymin=yield.preds.df$ci.lb.yield,ymax=yield.preds.df$ci.ub.yield),fill="blue",alpha=0.2) +
#     geom_hline(y=0, linetype="twodash") + 
#     scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#     scale_colour_manual(values=c("red","blue"),labels=c("Richness","Yield")) +
#     ylab("RR")  + xlab(paste(mods)) +
#     theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
#   
#   print(plot1)
#   ggsave(plot1, file = paste(path2temp, "/Scatterplot_",gsub(".","",MA.coeffs.cont$Moderator[i],fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
# }
# 
# ############################################################################
# ### 08.3. Forest plots for noLU vs low/medium/high LU
# ### 
# ############################################################################
# 
# ### plot all in one rush
# #TO DO: Plot for no moderators
# for(choose.moderator in as.character(unique(MA.coeffs.noLU$Moderator))[-1]){
#   ES.moderator.subset <- subset(MA.coeffs.noLU, Moderator %in% choose.moderator)
#   ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
#   ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
#   
#   if(nrow(ES.moderator.subset) >= 2){
#     ifelse(choose.moderator=="Product:High.LUI",n.levels <- table(paste(ES.frame.noLU$Product,ES.frame.noLU$High.LUI,sep=":"))[length(n.levels):1], n.levels <- table(factor(ES.frame.noLU[,which(names(ES.frame.noLU) %in% choose.moderator)])))
#     plot <- ggplot(ES.moderator.subset, aes(x=paste(levels," (",n.levels, ")", sep=""), y=mean.Richness, ymin=mean.Richness-1.96*se.Richness, ymax=mean.Richness+1.96*se.Richness)) + 
#       geom_pointrange(size=1.2) + 
#       coord_flip() +
#       geom_hline(x=0, linetype="twodash") + # weird: draws the line at x=0!!
#       scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#       ylab("Response Ratio") +
#       xlab(ES.moderator.subset$Moderator)  + #switch because of the coord_flip() above 
#       theme(axis.title = element_text(size = rel(1.8)), axis.text = element_text(size = rel(1.8)),legend.text=element_text(size = rel(1.8)),legend.title=element_text(size = rel(1.8)))
#     print(plot)
#   }
#   
#   ggsave(plot, file = paste(path2temp, "/ForestPlot",gsub(":","",gsub(".","",choose.moderator,fixed=T),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
#   
# }
# 
# 
# 
# ############################
# ### SK Annapolis WS 2015 ###
# ### cross diagramm with only 3 groups: low-low, medium-medium, high-high
# ############################
# ### ------------------------
# 
# # select dataset and moderator grand means to plot
# data.to.plot = ES.frame[which(ES.frame$Low.LUI == ES.frame$High.LUI),]
# moderator.grand.mean.subset = MA.coeffs.cat[which(MA.coeffs.cat$Moderator %in% "LUI.range.level" & MA.coeffs.cat$levels %in% c("low-low","medium-medium","high-high")),]
# 
# # order levels of moderators so they are displayed in same colour
# moderator.grand.mean.subset$levels = factor(moderator.grand.mean.subset$levels, levels=c("low-low","medium-medium","high-high"))
# data.to.plot$LUI.range.level = factor(data.to.plot$LUI.range.level, levels=c("low-low","medium-medium","high-high"))
# 
# #set colour scheme here ! 3 for points and 3 for crosses
# colours.to.plot = c("#33FF00","#FFCC00","#FF0000","#33FF00","#FFCC00","#FF0000")
# 
# # get axes length to center the plot 
# if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
#   y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
# }else{
#   y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
# }
# 
# if(0 - min(data.to.plot$Yield.Log.RR) > max(data.to.plot$Yield.Log.RR)){
#   x.range.for.plot = c(0 -max(data.to.plot$Yield.Log.RR) - 0.5,max(data.to.plot$Yield.Log.RR) + 0.5 )
# }else{
#   x.range.for.plot = c(min(data.to.plot$Yield.Log.RR)-0.5,0-min(data.to.plot$Yield.Log.RR) + 0.5)
# }
# 
# plot.within.groups.cross.diagramm =  ggplot() + 
# 
#   # study cases as points
#   geom_point(data= data.to.plot, aes(x=Yield.Log.RR, y=Richness.Log.RR,colour=LUI.range.level), size=3.5,alpha = 0.5) +
#   
#   #grand mean for moderators
#   geom_pointrange(data=moderator.grand.mean.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness),colour=levels), size=1) +
#   geom_segment(data=moderator.grand.mean.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness,colour=levels), size=1) +
# 
#   # colouring
#   scale_colour_manual(values=colours.to.plot,
#                       breaks=c("low-low","medium-medium","high-high"),
#                       labels=c("low-low","medium-medium","high-high")) +
#   
#   # lines for zero response rations
#   geom_hline(data=ES.frame, x=0, linetype="twodash",colour="grey") + 
#   geom_vline(data=ES.frame, y=0, linetype="twodash",colour="grey") +
#     
#   #white background
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
#   
#   scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#   scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
#   ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
#   theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +
# 
#   #legend
#   theme(legend.position="top",legend.title=element_blank())   +
# 
#   # place the zero-zero lines in the middle of the plot
#   coord_cartesian(ylim= y.range.for.plot, xlim=x.range.for.plot)
# 
# print(plot.within.groups.cross.diagramm)
# ggsave(plot.within.groups.cross.diagramm, file = paste(path2temp, "/Cross_diagram_within_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")
# 
# ############################
# ### SK Annapolis WS 2015 ###
# ### cross diagramm with only 3 groups: low-medium, low-high, medium-high
# ############################
# ### ------------------------
# 
# 
# 
# # select dataset and moderator grand means to plot
# data.to.plot = ES.frame[which(!(ES.frame$Low.LUI == ES.frame$High.LUI)),]
# moderator.grand.mean.subset = MA.coeffs.cat[which(MA.coeffs.cat$Moderator %in% "LUI.range.level" & MA.coeffs.cat$levels %in% c("low-medium","medium-high","low-high")),]
# 
# # order levels of moderators so they are displayed in same colour
# moderator.grand.mean.subset$levels = factor(moderator.grand.mean.subset$levels, levels=c("low-medium","medium-high","low-high"))
# data.to.plot$LUI.range.level = factor(data.to.plot$LUI.range.level, levels=c("low-medium","medium-high","low-high"))
# 
# #set colour scheme here ! 3 for points and 3 for crosses
# colours.to.plot = c("#3399FF","#FFCC00","#FF0000","#3399FF","#FFCC00","#FF0000")
# 
# # get axes length to center the plot 
# if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
#   y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
# }else{
#   y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
# }
# 
# if(0 - min(data.to.plot$Yield.Log.RR) > max(data.to.plot$Yield.Log.RR)){
#   x.range.for.plot = c(0 -max(data.to.plot$Yield.Log.RR) - 0.5,max(data.to.plot$Yield.Log.RR) + 0.5 )
# }else{
#   x.range.for.plot = c(min(data.to.plot$Yield.Log.RR)-0.5,0-min(data.to.plot$Yield.Log.RR) + 0.5)
# }
# 
# plot.across.groups.cross.diagramm =  ggplot() + 
#   
#   # study cases as points
#   geom_point(data= data.to.plot, aes(x=Yield.Log.RR, y=Richness.Log.RR,colour=LUI.range.level), size=3.5,alpha = 0.5) +
#   
#   #grand mean for moderators
#   geom_pointrange(data=moderator.grand.mean.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness),colour=levels), size=1) +
#   geom_segment(data=moderator.grand.mean.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness,colour=levels), size=1) +
#   
#   # colouring
#   scale_colour_manual(values=colours.to.plot,
#                       breaks=c("low-medium","medium-high","low-high"),
#                       labels=c("low-medium","medium-high","low-high")) +
#   
#   # lines for zero response rations
#   geom_hline(data=ES.frame, x=0, linetype="twodash",colour="grey") + 
#   geom_vline(data=ES.frame, y=0, linetype="twodash",colour="grey") +
#   
#   #white background
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
#   
#   scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#   scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
#   ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
#   theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +
#   
#   #legend
#   theme(legend.position="top",legend.title=element_blank())   +
#   
#   # place the zero-zero lines in the middle of the plot
#   coord_cartesian(ylim= y.range.for.plot, xlim=x.range.for.plot)
# 
# print(plot.across.groups.cross.diagramm)
# ggsave(plot.across.groups.cross.diagramm, file = paste(path2temp, "Cross_diagram_across_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")
# 
# 
# ############################
# ### SK Annapolis WS 2015 ###
# ### forest plots for each LUI-comparison class
# ############################
# ### ------------------------
# 
# LUI.range.level = c("low-low","medium-medium","high-high","low-medium","medium-high","low-high")
# 
# 
# LUI.level.to.plot = LUI.range.level[1]
# 
# for(LUI.level.to.plot in LUI.range.level){
#   data.to.plot = subset(ES.frame, LUI.range.level %in% LUI.level.to.plot)
#   data.to.plot = data.to.plot[order(data.to.plot$Yield.Log.RR,decreasing=T),]
#   data.to.plot$uniqueID = factor(paste(data.to.plot$Study.ID,data.to.plot$Case.ID),levels=paste(data.to.plot$Study.ID,data.to.plot$Case.ID))
# 
#   
#   # get axes length to center the plot 
#   min.values = sqrt(min(c(data.to.plot$Yield.Log.RR,data.to.plot$Richness.Log.RR))^2)
#   max.values = sqrt(max(c(data.to.plot$Yield.Log.RR,data.to.plot$Richness.Log.RR))^2)
#   
#   if(min.values > max.values){
#     y.range.for.plot = c(0 - min.values -0.5,min.values + 0.5)
#   }else{
#     y.range.for.plot = c(0 - max.values -0.5,max.values + 0.5)
#   }
#   
#   
# plot.forest = ggplot(data=data.to.plot) +
#   
#   geom_pointrange(aes(x=uniqueID, y=Yield.Log.RR, ymin=Yield.Log.RR	- (1.96*Yield.Log.RR.Var), ymax=Yield.Log.RR	+ (1.96*Yield.Log.RR.Var),colour="Yield"), size=1) +
#   geom_pointrange(aes(x=uniqueID, y=Richness.Log.RR, ymin=Richness.Log.RR	- (1.96*Richness.Log.RR.Var), ymax=Richness.Log.RR	+ (1.96*Richness.Log.RR.Var),colour="Species Richness"), size=1) +
#   geom_hline(x=0,linetype ="twodash")  +
# 
#   
#   #scale manually to get the legend correct
#   scale_colour_manual(values  =c("#00CC00","#FF6633")) +
#   
#   #white background + flip 90 degrees
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(), 
#       panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
#   coord_flip(ylim=y.range.for.plot)
# 
#   print(plot.forest)
#   ggsave(plot.forest, file = paste(c(path2temp, "Forest_plot_",LUI.level.to.plot,".png"), collapse=""), width = 20, height = 8, type = "cairo-png")
# 
# }

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
  data.to.plot = melt(data.to.plot[,c("Study.ID","Case.ID","LUI.range.level","Richness.Log.RR","Richness.Log.RR.Var","Yield.Log.RR","Yield.Log.RR.Var",
                                      "Yield.SD.is.imputed.low","Yield.SD.is.imputed.high", "Richness.SD.is.imputed.low","Richness.SD.is.imputed.high")],
                      id.vars=c("Study.ID","Case.ID","LUI.range.level","Richness.Log.RR.Var","Yield.Log.RR.Var","Yield.SD.is.imputed.low","Yield.SD.is.imputed.high", "Richness.SD.is.imputed.low","Richness.SD.is.imputed.high"),
                      measure.vars=c("Richness.Log.RR", "Yield.Log.RR"))
  data.to.plot = data.to.plot[order(data.to.plot$value),]
  data.to.plot$Richness.Log.RR.Var[which(data.to.plot$variable %in% "Yield.Log.RR")] =   data.to.plot$Yield.Log.RR.Var[which(data.to.plot$variable %in% "Yield.Log.RR")] 
  data.to.plot = data.to.plot[,-(which(names(data.to.plot) %in% "Yield.Log.RR.Var"))]
  names(data.to.plot)[which(names(data.to.plot) %in% "Richness.Log.RR.Var")] = "Log.RR.Var"
  names(data.to.plot)[which(names(data.to.plot) %in% "variable")] = "RR.ID"
  names(data.to.plot)[which(names(data.to.plot) %in% "value")] = "RR.value"
  
  data.to.plot$uniqueID = paste(as.character(data.to.plot$Study.ID),as.character((data.to.plot$Case.ID)))
  data.to.plot$uniqueID = paste(as.character(data.to.plot$uniqueID),as.character((data.to.plot$RR.ID)))
  
  # sort the table so that Yield and Richness of the same study are together
  data.to.plot.ordered = subset(data.to.plot, RR.ID %in% "Yield.Log.RR")
  data.to.fill.with.ordered = data.to.plot.ordered[0,]
  for(sort.run in 1:nrow(data.to.plot.ordered)){
    data.to.fill.with.ordered = rbind(data.to.fill.with.ordered,data.to.plot.ordered[sort.run,])
    data.to.fill.with.ordered = rbind(data.to.fill.with.ordered,
                                      data.to.plot[which(data.to.plot$Study.ID %in% data.to.plot.ordered$Study.ID[sort.run] &
                                                           data.to.plot$Case.ID %in% data.to.plot.ordered$Case.ID[sort.run] &  
                                                           data.to.plot$RR.ID %in% "Richness.Log.RR"),])
    
  }
  data.to.plot = data.to.fill.with.ordered
  data.to.plot$uniqueID = factor(data.to.plot$uniqueID,levels= rev(data.to.plot$uniqueID))
  
  
  #add a column with the study name and case id + remove every second to make axis better readable
  #data.to.plot$axes.naming = paste(data.to.plot$Study.ID,data.to.plot$Case.ID)
  data.to.plot$axes.naming = as.character(data.to.plot$uniqueID)
  data.to.plot$axes.naming[which(seq(1:nrow(data.to.plot)) %% 2 == 0)] = " "
  
  #rename Richness.Log.RR and Yield.Log.RR
  data.to.plot$RR.ID = as.character(data.to.plot$RR.ID)
  data.to.plot$RR.ID[which(data.to.plot$RR.ID %in% "Yield.Log.RR")] = "Yield"
  data.to.plot$RR.ID[which(data.to.plot$RR.ID %in% "Richness.Log.RR")] = "Species Richness"
  data.to.plot$RR.ID = factor(data.to.plot$RR.ID, levels =c("Yield", "Species Richness"))
  
  
  # get axes length to center the plot 
  max.values = sqrt(max(data.to.plot$RR.value^2,na.rm=TRUE)) + 0.5
  
  # create a data is imputed column
  data.to.plot$is.SD.imputed = "no"
  data.to.plot$is.SD.imputed[which(data.to.plot$Yield.SD.is.imputed.low %in% "yes" & data.to.plot$RR.ID %in% "Yield")] = "yes"
  data.to.plot$is.SD.imputed[which(data.to.plot$Yield.SD.is.imputed.high %in% "yes" & data.to.plot$RR.ID %in% "Yield")] = "yes"
  data.to.plot$is.SD.imputed[which(data.to.plot$Richness.SD.is.imputed.low %in% "yes"& data.to.plot$RR.ID %in% "Species Richness")] = "yes"
  data.to.plot$is.SD.imputed[which(data.to.plot$Richness.SD.is.imputed.high %in% "yes" & data.to.plot$RR.ID %in% "Species Richness")] = "yes"
  
  #find exact replicated that come from a single imputation
  data.to.plot$is.duplicate = duplicated(data.to.plot[,c("Study.ID","RR.value","Log.RR.Var","RR.ID")])
  data.to.plot$is.duplicate[which(data.to.plot$is.duplicate)] = "duplicate"
  data.to.plot$is.duplicate[which(data.to.plot$is.duplicate %in% "FALSE")] = "value"
  data.to.plot$is.duplicate = factor(data.to.plot$is.duplicate,levels=c("value","duplicate"))
  

#### if ervery runs smooth - cut away this plot  
  #cut away the information of Yield.Log.RR from the unique Study ID to have them not displayed in the graph
#  data.to.plot$uniqueID.for.axis.lables = seq(nrow(data.to.plot):1)
#  data.to.plot$uniqueID.for.axis.lables = paste(paste(paste(data.to.plot$uniqueID.for.axis.lables,". ",sep=""),data.to.plot$Study.ID,sep=""),data.to.plot$Case.ID,sep="")
#  data.to.plot$uniqueID.for.axis.lables = factor(data.to.plot$uniqueID.for.axis.lables,levels=(data.to.plot$uniqueID.for.axis.lables))
#####

  data.to.plot$uniqueID = factor(data.to.plot$uniqueID,levels=rev(data.to.plot$uniqueID))
  data.to.plot$axes.naming = gsub(" Yield.Log.RR","",data.to.plot$axes.naming)
  data.to.plot$colouring = paste(data.to.plot$RR.ID,data.to.plot$is.duplicate)
  data.to.plot$colouring = factor(data.to.plot$colouring,levels = c("Yield value","Yield duplicate","Species Richness value","Species Richness duplicate"))
  
  
  plot.forest =
      ggplot(data=data.to.plot) +
      
      geom_pointrange(aes(x=uniqueID, y=RR.value, ymin=RR.value	- (1.96*Log.RR.Var), ymax=RR.value	+ (1.96*Log.RR.Var),colour=colouring,alpha=is.SD.imputed,linetype=is.SD.imputed), size=1) +
      geom_hline(x=0,linetype ="twodash")  +

      #scale manually to get the legend correct
      scale_colour_manual(values=c("#FF6633","grey","#00CC00","grey")) +
      scale_x_discrete("Study ID",breaks= as.character(data.to.plot$uniqueID),labels=data.to.plot$axes.naming)  +
      scale_alpha_discrete(range = c(1,0.4)) + 
      scale_linetype_discrete(c("solid","twodash"),guide="none") +
    
      #white background + flip 90 degrees
      theme(axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black"),
            axis.text.y = element_text(vjust=1)) +
      coord_flip(ylim=c(0 - max.values,max.values)) +
      geom_vline(xintercept=seq(from=0.5, to=nrow(data.to.plot)-0.5,by=2),linetype="solid",colour="grey") +
      
      #axes labels
      xlab("Study ID") +
      ylab("Log Response Ration")+
      ggtitle(paste("Forest Plot of study case effect sizes\n- ",LUI.level.to.plot))
      
    print(plot.forest)
    ggsave(plot.forest, file = paste(c(path2temp, "Forest_plot_",LUI.level.to.plot,".png"), collapse=""), width = 15, height = nrow(data.to.plot) / 5, type = "cairo-png")
    
  }
##################
### RESTERAMPE ###
##################
