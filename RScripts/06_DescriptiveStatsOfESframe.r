############################################################################
### Purpose of this skript module 06 is to:
###
### 06.1 Protocol structure and summary of variables in the ES.frame
### 06.2 Plot Histograms of all variables in the ES.frame 
### 06.3 Protocol structure and summary of variables in the ES.frame.noLU
### 06.4 Plot Histograms of all variables in the ES.frame.noLU 
###
### General comments:
###
### Authors: KG ...
############################################################################

setwd(path2temp %+% "/")

############################################################################
### 06.1 Protocol structure and summary of variables in the ES.frame
############################################################################

sink('str_summary_ESframe.txt')
str(ES.frame)
summary(ES.frame)

### explore the issue of dependence
hist(table(ES.frame$Study.Case),breaks=0.5:6.5)
ES.frame[duplicated(ES.frame$Study.Case),c("Study.Case", "LUI.range.level")]

### distribution of LUI ranges among covariables
with(ES.frame,table(Land.use...land.cover,LUI.range.level))
with(ES.frame,table(Species.Group,LUI.range.level))
with(ES.frame,table(Product,LUI.range.level))
sink()

############################################################################
### 06.2 Plot Histograms of all variables in the ES.frame 
############################################################################

#pdf("PlotHist_ESframe.pdf", width = 15)
for(i in 1:ncol(ES.frame)){
  p <- ggplot(data=ES.frame) + 
    geom_histogram(aes(x=ES.frame[,i]), size=0.4) + 
    xlab(names(ES.frame)[i]) +
    ggtitle(paste(names(ES.frame)[i])) + 
    theme(axis.title = element_text(size = rel(2)), axis.text = element_text(size = rel(2)),plot.title=element_text(size = rel(2)) , axis.text.x=element_text(angle=45,vjust = 1, hjust=1),legend.text=element_text(size = rel(2)),legend.title=element_text(size = rel(2)))
  #,axis.ticks.length=unit(.4,"cm")
  print(p)
  ggsave(p, file = paste(path2temp,"/PlotHist_ESframe_",gsub(".","",colnames(ES.frame[i]),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
  
}  
#dev.off()

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
### 06.3 Protocol structure and summary of variables in the ES.frame.noLU
############################################################################

sink('str_summary_ES.frame.noLU.txt')
str(ES.frame.noLU)
summary(ES.frame.noLU)
sink()

############################################################################
### 06.4 Plot Histograms of all variables in the ES.frame.noLU 
############################################################################

#pdf("PlotHist_ESframenoLU.pdf", width = 15)
for(i in 1:ncol(ES.frame.noLU)){
  p <- ggplot(data=ES.frame.noLU) + 
    geom_histogram(aes(x=ES.frame.noLU[,i]), size=0.4) + 
    xlab(names(ES.frame.noLU)[i]) +
    ggtitle(paste(names(ES.frame.noLU)[i])) + 
    theme(axis.title = element_text(size = rel(2)), axis.text = element_text(size = rel(2)),plot.title=element_text(size = rel(2)), axis.text.x=element_text(angle=45,vjust = 1, hjust=1),legend.text=element_text(size = rel(2)),legend.title=element_text(size = rel(2)))
  #,axis.ticks.length=unit(.4,"cm")
  print(p)
  ggsave(p, file = paste(path2temp,"/PlotHist_ESframenoLU_",gsub(".","",colnames(ES.frame.noLU[i]),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
}  
#dev.off()

setwd(path2wd)
