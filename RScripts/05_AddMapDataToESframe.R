############################################################################
### Purpose of this skript module 05 is to:
###
### 05.1. Intersect studies with main climate zones
### 05.2. Intersect studies with coarse classes of land-use history
### General comments:
### * 
###
### Authors: MB, KG, RS ...
############################################################################

# set a wd suitable for downloading and extracting Ecoregions shapefile
setwd(path2temp %+% "/") 

# remove unneeded information for extraction, the extract command requests this form of data
ES.frame <- ES.frame[!is.na(ES.frame$Longitude+ES.frame$Latitude),] # remove NA lonlat
lonlat <- cbind(ES.frame$Longitude,ES.frame$Latitude)

# ES.frame.noLU <- ES.frame.noLU[!is.na(ES.frame.noLU$Longitude+ES.frame.noLU$Latitude),] # remove NA lonlat
# lonlat.noLU <- cbind(ES.frame.noLU$Longitude,ES.frame.noLU$Latitude)

############################################################################
### 05.1. Intersect studies with main climate zones
############################################################################
print("Intersect studies with climate zones")
if (file.exists("1976-2000_GIS.zip")==FALSE){
  download.file("http://koeppen-geiger.vu-wien.ac.at/data/1976-2000_GIS.zip","1976-2000_GIS.zip", mode="wb")
  unzip("1976-2000_GIS.zip")
} else {unzip("1976-2000_GIS.zip")}
climate_zone <- readOGR(dsn=".",layer="1976-2000")

climate_zone@data$main_climate <- cut(climate_zone@data$GRIDCODE, breaks=c(10,20,30,40,60,70), labels=c("Tropical","Arid","Temperate","Cold (Continental)","Polar"))
save(climate_zone, file=path2temp %+% "climate_mapdata.Rdata")
# Legend(GRIDCODE)
# 11 ... Af
# 12 ... Am
# 13 ... As
# 14 ... Aw
# 21 ... BWk
# 22 ... BWh
# 26 ... BSk
# 27 ... BSh
# 31 ... Cfa
# 32 ... Cfb
# 33 ... Cfc
# 34 ... Csa
# 35 ... Csb
# 36 ... Csc
# 37 ... Cwa
# 38 ... Cwb
# 39 ... Cwc
# 41 ... Dfa
# 42 ... Dfb
# 43 ... Dfc
# 44 ... Dfd
# 45 ... Dsa
# 46 ... Dsb
# 47 ... Dsc
# 48 ... Dsd
# 49 ... Dwa
# 50 ... Dwb
# 51 ... Dwc
# 52 ... Dwd
# 61 ... EF
# 62 ... ET

# extract ecoregions
climate_extract <- extract(climate_zone,lonlat)
climate_extract <- climate_extract[!duplicated(climate_extract$point.ID),]
ES.frame$main_climate <- climate_extract$main_climate

############################################################################
### 05.2. Intersect studies with coarse classes of land-use history
############################################################################
print("Intersect studies with Land-use history")

setwd(path2temp %+% "/")

if (file.exists("rmsrkk11.zip")==FALSE){
  download.file("https://www.dropbox.com/s/s1wdaq1ui2o0tnu/rmsrkk11.zip?dl=1", "rmsrkk11.zip", mode="wb")
  unzip("rmsrkk11.zip")
} else {unzip("rmsrkk11.zip")}

rmrsrkk11<-raster("rmsrkk11.grd")

save(rmrsrkk11, file=path2temp %+% "landuse_history_mapdata.Rdata")

# extract year since 20% use
years_extract <- extract(rmrsrkk11,lonlat)
ES.frame <- cbind(ES.frame,years_extract)
colnames(ES.frame)[which(names(ES.frame) == "years_extract")]<-"landuse_history"

# recode NAs to 11000
ES.frame$landuse_history[is.na(ES.frame$landuse_history)] <- 11000

# transform landuse_history to factor
ES.frame$landuse_history <- factor(ES.frame$landuse_history)
levels(ES.frame$landuse_history) <- c("5950 BC","50 BC", "1450","1950", "after 1950")

summary(ES.frame$landuse_history)

setwd(path2wd)


##### reclassification of Jed Kaplans 100 years based data to create rmsrkk11.grd
# 
# setwd(path2temp %+% "/")
# 
# if (file.exists("KK11_05m_8k_merge_100yr.nc.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/oat650i7ervc5oh/KK11_05m_8k_merge_100yr.nc.zip?dl=1", "KK11_05m_8k_merge_100yr.nc.zip", mode="wb")
#   unzip("KK11_05m_8k_merge_100yr.nc.zip")
# } else {unzip("KK11_05m_8k_merge_100yr.nc.zip")}
# 
# #kk11<-nc_open("KK11_05m_8k_merge_100yr.nc")
# #rkk11<-raster("KK11_05m_8k_merge_100yr.nc")
# srkk11<-stack("KK11_05m_8k_merge_100yr.nc") # load data as stack
# rsrkk11<-srkk11 # create raster object to fill
# 
# gc() # clear memory
# 
# # reclassify stack based on a 20% land used threshold
# for (i in 1:nlayers(srkk11)){
#   m <- c(-1, 0.2, -9999,  0.2, 1.1, as.numeric((strsplit(names(srkk11[[i]]),"X"))[[1]][2]))  
#   rclmat<-matrix(m,ncol=3,byrow=TRUE)
#   rsrkk11[[i]]<-reclassify(srkk11[[i]], rclmat)
# }
# 
# # merge stack to get oldest time since 20% used
# mrsrkk11<-max(rsrkk11)
# 
# mrsrkk11[mrsrkk11==-9999]<-NA #set -99999 to NA
# #plot(mrsrkk11)
# 
# # reclassify to 5 age classes (until 50BC, 50BC-1450, 1450-1650,1650-1850,1850-1950)
# m <- c(-1,501,1950,502,2001,1450,2002,7801,-50, 7802,8001,-5950)
# rclmat<-matrix(m,ncol=3,byrow=TRUE)
# rmrsrkk11<-reclassify(mrsrkk11, rclmat)#,include.lowest=TRUE)
# #rmrsrkk11[rmrsrkk11==50]<--50 #reset to -50
# 
# my.at <- c(-7000,-5900,-10, 1500,1800, 2000,12000)
# levelplot(rmrsrkk11,par.settings=YlOrRdTheme,at=my.at)
# 
# 
# #################### create a 1% threshold layer 
# 
# rsrkk11_1<-srkk11 # create raster object to fill
# 
# rm(mrsrkk11,rsrkk11)
# gc() # clear memory
# 
# # reclassify stack based on a 1% land used threshold
# for (i in 1:nlayers(srkk11)){
#   m <- c(-1, 0.01, -9999,  0.01, 1.1, as.numeric((strsplit(names(srkk11[[i]]),"X"))[[1]][2]))  
#   rclmat<-matrix(m,ncol=3,byrow=TRUE)
#   rsrkk11_1[[i]]<-reclassify(srkk11[[i]], rclmat)
# }
# 
# # merge stack to get oldest time since 1% used
# mrsrkk11_1<-max(rsrkk11_1)
# 
# mrsrkk11_1[mrsrkk11_1==-9999]<-NA #set -9999 to NA
# #plot(mrsrkk11)
# 
# # reclassify to 1 age class
# m <- c(-1,8001,10000)
# rclmat<-matrix(m,ncol=3,byrow=TRUE)
# rmrsrkk11_1<-reclassify(mrsrkk11_1, rclmat)#,include.lowest=TRUE)
# #rmrsrkk11[rmrsrkk11==50]<--50 #reset to -50
# 
# rm(mrsrkk11_1,rsrkk11_1)
# gc() # clear memory
# 
# ## cells that are below 20% threshold are reclassified to the value 11000
# rmrsrkk11[is.na(rmrsrkk11_1)]<-10000
# rmrsrkk11[is.na(rmrsrkk11)]<-11000
# rmrsrkk11[rmrsrkk11==10000]<-NA
# 
# levelplot(rmrsrkk11,par.settings=YlOrRdTheme,at=my.at)
# writeRaster(rmrsrkk11,"rmsrkk11.grd")
# 
# # extract year since 20% use
# years_extract <- extract(rmrsrkk11,lonlat)
# ES.frame <- cbind(ES.frame,years_extract)
# colnames(ES.frame)[which(names(ES.frame) == "years_extract")]<-"landuse_history"
# 
# summary(years_extract)
# table(years_extract)
# 
# setwd(path2wd)

# ############################################################################
# ### 05.1. Intersect studies with global maps of WWF_REALMs Ecoregions, reduce them to five main regions
# ############################################################################
# print("Intersect studies with BIOMES")
# 
# if (file.exists("terr-ecoregions-TNC.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/ihivf2ie98wxvee/terr-ecoregions-TNC.zip?dl=1", "terr-ecoregions-TNC.zip", mode="wb")
#   unzip("terr-ecoregions-TNC.zip")
# } else {unzip("terr-ecoregions-TNC.zip")}
# 
# ecoregions <- readOGR(".","tnc_terr_ecoregions",verbose = FALSE)
# 
# ### reclassify Biomes in coarse classes
# ecoregions@data$BIOME <- ecoregions@data$WWF_MHTNAM
# ecoregions@data$BIOME <- paste(ecoregions@data$BIOME)
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Tropical and Subtropical Moist Broadleaf Forests",
#                                                     "Tropical and Subtropical Dry Broadleaf Forests",
#                                                     "Tropical and Subtropical Coniferous Forests",
#                                                     "Tropical and Subtropical Coniferous Forests"))] <- "Tropical_Forests"
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Temperate Broadleaf and Mixed Forests",
#                                                     "Temperate Conifer Forests",
#                                                     "Boreal Forests/Taiga"))] <- "Temperate_Boreal_Forests"
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Mediterranean Forests, Woodlands and Scrub"))] <- "Drylands"
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Tropical and Subtropical Grasslands, Savannas and Shrublands",
#                                                     "Flooded Grasslands and Savannas"))] <- "Tropical_Grasslands"
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Temperate Grasslands, Savannas and Shrublands",
#                                                     "Montane Grasslands and Shrublands"))]<-"Temperate_Montane_Grasslands"
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Deserts and Xeric Shrublands"))]<-"Deserts"
# ecoregions@data$BIOME[(ecoregions@data$BIOME %in% c("Inland Water","NA"))]<-NA
# ecoregions@data$BIOME <- factor(ecoregions@data$BIOME)
# biome_mapdata <- fortify(ecoregions, region="BIOME") #  convert shapefile to a data frame, extract biomes 
# names(biome_mapdata)[grep("id",names(biome_mapdata))] <- "BIOME"
# biome_mapdata$BIOME <- factor(biome_mapdata$BIOME)
# 
# ## reduce size of the data.frame
# biome_mapdata <- subset(biome_mapdata, hole==FALSE & lat>=-55) # remove points equal to holes (not plotted in ggplot cf https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles) and cut out Antarctica
# biome_mapdata$lon <- round(biome_mapdata$lon, digits=2) # reduce resolution
# biome_mapdata$lat <- round(biome_mapdata$lat, digits=2) # reduce resolution
# biome_mapdata <- biome_mapdata[!duplicated(biome_mapdata[,c("long","lat","BIOME")]),] # remove duplicates not needed for plotting
# biome_mapdata <- biome_mapdata[,c("long","lat","BIOME","group")] # remove columns not needed for plotting
# 
# save(biome_mapdata, file=path2temp %+% "biome_mapdata.Rdata")
# 
# # extract ecoregions
# realms_extract <- extract(ecoregions,lonlat)
# ES.frame <- cbind(ES.frame,realms_extract$BIOME)
# colnames(ES.frame)[which(names(ES.frame) == "realms_extract$BIOME")]<-"BIOME"
# 
# ES.frame$BIOME <- factor(ES.frame$BIOME)
# 
# setwd(path2wd)
# rm(realms_extract,ecoregions)
# 
# ### for ES.frame.noLU
# # extract ecoregions
# # if(nrow(ES.frame.noLU) >0){
# #   realms_extract <- extract(ecoregions,lonlat.noLU)
# #   ES.frame.noLU <- cbind(ES.frame.noLU,realms_extract$WWF_MHTNAM)
# #   colnames(ES.frame.noLU)[which(names(ES.frame.noLU) == "realms_extract$WWF_MHTNAM")]<-"BIOME"
# # }



############################################################################
### 05.2. Intersect studies with NPP
############################################################################
# print("Intersect studies with NPP")
# 
# if (file.exists("tn0_all_gcm.asc")==FALSE){
#   download.file("https://www.dropbox.com/s/689m9pc5bnejbg9/tn0_all_gcm.asc?dl=1","tn0_all_gcm.asc")
# }
# npp <- raster("tn0_all_gcm.asc",crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# ES.frame$npp<-extract(npp,lonlat,buffer=100000,fun=mean)

# if(nrow(ES.frame.noLU) >0){
#   ES.frame.noLU$npp<-extract(npp,lonlat.noLU, buffer=100000, fun=mean)
# }

############################################################################
### 05.3. Intersect studies with Land-use history
############################################################################
# print("Intersect studies with LAND-USE HISTORY")
# 
# if (file.exists("ellis_etal_2013_dataset.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/6qkr58sjimclpst/ellis_etal_2013_dataset.zip?dl=1", "ellis_etal_2013_dataset.zip", mode="wb")
#   unzip("ellis_etal_2013_dataset.zip")
# } else {
#   unzip ("ellis_etal_2013_dataset.zip")
# }
# 
# timeseries.hyde <- list("sus_hbc6000","sus_hbc3000","sus_hbc1000","sus_had0","sus_had1000","sus_had1500","sus_had1750","sus_had1900","sus_had1950","sus_had2000")
# timeseries.kk10 <- list("sus_kbc6000","sus_kbc3000","sus_kbc1000","sus_kad0","sus_kad1000","sus_kad1500","sus_kad1750","sus_kad1900","sus_kad1950","sus_kad2000")
# 
# hyde.LUhist.stack <- stack(lapply(timeseries.hyde,function(x) raster("hyde/sus_use/" %+% x)))
# kk10.LUhist.stack <- stack(lapply(timeseries.kk10,function(x) raster("kk10/sus_use/" %+% x)))
# 
# ## ES.frame
# hyde.extract.year.of.first.use <- extract(hyde.LUhist.stack,lonlat) 
# names(hyde.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
# hyde.year.of.first.use <- apply(hyde.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(hyde.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
# kk10.extract.year.of.first.use <- extract(kk10.LUhist.stack,lonlat) 
# names(kk10.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
# kk10.year.of.first.use <- apply(kk10.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(kk10.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
# ES.frame$time.since.first.use <- log10(2000-apply(cbind(hyde.year.of.first.use,kk10.year.of.first.use),1,function(x){ifelse(all(is.na(x)),2000,min(x,na.rm=T))})+1) # set not yet used land to log10(1)=0
# ES.frame$start.agr.use <- ifelse(ES.frame$time.since.first.use >= 500,"old","young")
# ES.frame$start.agr.use[is.na(ES.frame$start.agr.use)] <- "not yet used"
# 
## ES.frame.noLU
# if(nrow(ES.frame.noLU) >0){
#   hyde.extract.year.of.first.use <- extract(hyde.LUhist.stack,lonlat.noLU) 
#   names(hyde.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
#   hyde.year.of.first.use <- apply(hyde.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(hyde.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
#   kk10.extract.year.of.first.use <- extract(kk10.LUhist.stack,lonlat.noLU) 
#   names(kk10.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
#   kk10.year.of.first.use <- apply(kk10.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(kk10.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
#   ES.frame.noLU$time.since.first.use <- log10(2000-apply(cbind(hyde.year.of.first.use,kk10.year.of.first.use),1,function(x){ifelse(all(is.na(x)),2000,min(x,na.rm=T))})+1) # set not yet used land to log10(1)=0
#   ES.frame.noLU$start.agr.use <- ifelse(ES.frame.noLU$time.since.first.use >= 500,"old","young")
#   ES.frame.noLU$start.agr.use[is.na(ES.frame.noLU$start.agr.use)] <- "not yet used"
# }


############################################################################
### Resterampe
############################################################################
############################################################################
### 05.3. Intersect studies with cropland hybrid IIASA
############################################################################

# if (file.exists("Hybrid.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/rr3qx2ogomegqxo/Hybrid.zip?dl=1", "Hybrid.zip", mode="wb")
#   unzip("Hybrid.zip")
# } else {unzip("Hybrid.zip")}
# 
# cropland.hybrid<-raster("Hybrid_10042015v9.img")
# ES.frame$cropcover<-extract(cropland.hybrid,lonlat, buffer=50000, fun=mean) # consider a buffer of radius=50km² around each dot)

############################################################################
### 05.6. Intersect studies with human pressure index
############################################################################
# TO DO

############################################################################
### 05.6. Intersect studies with GLOBCOVER
############################################################################
# 
# if (file.exists("Globcover_V2.2_Global.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/ks3sm60er8mgasd/Globcover_V2.2_Global.zip?dl=1", "Globcover_V2.2_Global.zip", mode="wb")
#   unzip("Globcover_V2.2_Global.zip")
# } else {
#   unzip ("Globcover_V2.2_Global.zip")
# }
# 
# globcover <- raster("GLOBCOVER_200412_200606_V2.2_Global_CLA.tif") 
# 
# globcover[globcover>30]<- 0
# globcover[globcover<15]<- 1 # reclassify 11 and 14 to 100%
# globcover[globcover==20]<- 0.6 # reclassify 20 to 60%
# globcover[globcover==30]<- 0.35 # reclassify 30 to 35%
# 
# writeraster(globcover, paste(path2temp,"/globcover_reclassified.tif",sep=""))
# 
# # points<-SpatialPoints(lonlat)
# # 
# # for (i in length(points)){
# #   
# #   pbuf <- gBuffer(points[i], widt=50000)
# #   buf <- mask(globcover, pbuf)
# #   buf[buf>30]<- 0
# #   buf[buf<15]<- 1 # reclassify 11 and 14 to 100%
# #   buf[buf==20]<- 0.6 # reclassify 20 to 60%
# #   buf[buf==30]<- 0.35 # reclassify 30 to 35%
# #   mean(buf)
# # }
# # 
# # pbuf <- gBuffer(points, widt=50000)
# # gobcover.extract$humanfootprint <- extract(globcover,lonlat, buffer=50000, fun=mean) # consider a buffer of radius=100km² around each dot)


############################################################################
### 05.9. Human Footprint
############################################################################
# if (file.exists("hfp_global_geo_grid.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/aedwhxiu6mygudo/hfp_global_geo_grid.zip?dl=1", "hfp_global_geo_grid.zip", mode="wb")
#   unzip("hfp_global_geo_grid.zip")
# } else {
#   unzip ("hfp_global_geo_grid.zip")
# }
# 
# humanfootprint <- raster("hfp_global_geo_grid/hf_v2geo")
# 
# ES.frame$humanfootprint <- log10(extract(humanfootprint,lonlat, buffer=100000, fun=mean)) # consider a buffer of radius=100km² around each dot)
# 
# ES.frame.noLU$humanfootprint <- log10(extract(humanfootprint,lonlat.noLU, buffer=100000, fun=mean)) # consider a buffer of radius=100km² around each dot)
# 
# 
# 

############################################################################
### 05.4. Intersect studies with Global Habitat Heterogeneity, Dissimilarity
############################################################################

### data from http://www.earthenv.org/texture.html
# 
# if (file.exists("habitat_dissimilarity.tif")==FALSE){
#   download.file("https://www.dropbox.com/s/bwpzna0y4e1t77e/Dissimilarity_01_05_25km_uint32.tif?dl=1", "habitat_dissimilarity.tif", mode="wb")
#   habitat_dissimilarity <- raster("habitat_dissimilarity.tif")
# } else {
#   habitat_dissimilarity <- raster("habitat_dissimilarity.tif")
# }
#   
# habitat_dissimilarity <- raster("habitat_dissimilarity.tif")
# 
# ES.frame$habitat_dissimilarity<-extract(habitat_dissimilarity,lonlat, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot)
# 
# ES.frame.noLU$habitat_dissimilarity<-extract(habitat_dissimilarity,lonlat.noLU, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot)
# 

############################################################################
### 05.3. Intersect studies with gross capital stock in agriculture
############################################################################
# 
# if (file.exists("Investment_CapitalStock_E_All_Data.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/xgqrmyiqx2lqvyl/Investment_CapitalStock_E_All_Data.zip?dl=1", "Investment_CapitalStock_E_All_Data.zip", mode="wb")
#   unzip("Investment_CapitalStock_E_All_Data.zip")
# } else {
#   unzip("Investment_CapitalStock_E_All_Data.zip")
# }
# 
# if (file.exists("agricultural_area.csv") == FALSE){
# 	download.file("https://www.dropbox.com/s/fscg314f6kkvbhv/Data_Extract_From_World_Development_Indicators_Data.csv?dl=1", "agricultural_area.csv")}
# agricultural_area <- read.csv("agricultural_area.csv")
# agricultural_area$X2007..YR2007. <- as.numeric(agricultural_area$X2007..YR2007.)
# agricultural_area$CountryCode <- countrycode(agricultural_area$Country.Code,"wb","iso3c")
# agricultural_area <- data.frame(Country.Code = agricultural_area$CountryCode,
#                                 agricultural_area = agricultural_area$X2007..YR2007.)
# ES.frame$agricultural_area <- agricultural_area$agricultural_area[(match(ES.frame$Country.Code,agricultural_area$Country.Code))]
# 
# capital_stock_in_agriculture <- read.csv("Investment_CapitalStock_E_All_Data.csv")
# capital_stock_in_agriculture <- subset(capital_stock_in_agriculture, Item == "Capital Stock + (Total)" & Element == "Gross Capital Stock (constant 2005 prices)" & Year == 2007)
# 
# # convert fao codes into iso3c codes
# capital_stock_in_agriculture$CountryCode <- countrycode(capital_stock_in_agriculture$CountryCode,"fao","iso3c")
# capital_stock_in_agriculture <- data.frame(Country.Code = capital_stock_in_agriculture$CountryCode,
#                                            capital_millionUSD = capital_stock_in_agriculture$Value)
# capital_stock_in_agriculture$agricultural_area <- agricultural_area$agricultural_area[(match(capital_stock_in_agriculture$Country.Code,agricultural_area$Country.Code))]
# capital_stock_in_agriculture$rel_capital_stock_in_agriculture <- capital_stock_in_agriculture$capital_millionUSD/capital_stock_in_agriculture$agricultural_area
# capital_stock_in_agriculture <- capital_stock_in_agriculture[!is.na(capital_stock_in_agriculture$Country.Code),] ## exclude Country.Code==NA from merging as this produces duplicates
# 
# ES.frame <- join(ES.frame,capital_stock_in_agriculture[,c("Country.Code","rel_capital_stock_in_agriculture")],by="Country.Code")
# ES.frame$rel_capital_stock_in_agriculture <- log10(ES.frame$rel_capital_stock_in_agriculture)
# 
# ES.frame.noLU <- join(ES.frame.noLU,capital_stock_in_agriculture[,c("Country.Code","rel_capital_stock_in_agriculture")],by="Country.Code")
# ES.frame.noLU$rel_capital_stock_in_agriculture <- log10(ES.frame.noLU$rel_capital_stock_in_agriculture)

############################################################################
### 05.4. Intersect studies with GLOBCOVER
############################################################################

# if (file.exists("Globcover_V2.2_Global.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/ks3sm60er8mgasd/Globcover_V2.2_Global.zip?dl=1", "Globcover_V2.2_Global.zip", mode="wb")
#   unzip("Globcover_V2.2_Global.zip")
# } else {
#   unzip("Globcover_V2.2_Global.zip")
# }
# 
# globcover<-raster("GLOBCOVER_200412_200606_V2.2_Global_CLA.tif")
# 
# ### reclassify everything except croplands to 0
# 
# m<-c(11,14,100,20,20,60,30,30,35,40,230,0)
# rclmat<-matrix(m,ncol=3,byrow=TRUE)
# 
# beginCluster()
# rc1 <- clusterR(globcover, reclassify, args=list(rcl=rclmat,right=NA))
# endCluster()
# writeRaster(rc1, "globcover_rc.tif",format="GTiff",overwrite=TRUE)
# 
# globcover.rc<-raster("globcover_rc.tif")
# 
# ES.frame$globcover<-extract(globcover.rc,lonlat, buffer=50000, fun=mean) # consider a buffer of radius=50km² around each dot)
# 

# 
# ### for ES.frame.noLU
# climate_extract <- extract(climate_zone,lonlat.noLU)
# ES.frame.noLU$main_climate <- cut(climate_extract$GRIDCODE, breaks=c(10,20,30,40,50,60), labels=c("equatorial","arid","warm temperature","snow","polar"))

## solar radiation
# if (file.exists("CM10_1975H_Bio_ASCII_V1.2.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/me92v7ozmixy75a/CM10_1975H_Bio_ASCII_V1.2.zip?dl=1", "CM10_1975H_Bio_ASCII_V1.2.zip", mode="wb")
#   unzip("CM10_1975H_Bio_ASCII_V1.2.zip")
# } else {
#   unzip("CM10_1975H_Bio_ASCII_V1.2.zip")
# }
# 
# if (file.exists("tn0_all_gcm.asc")==FALSE){
#   download.file("https://www.dropbox.com/s/689m9pc5bnejbg9/tn0_all_gcm.asc?dl=1","tn0_all_gcm.asc")
# }
# 
# annual_mean_radiation <- raster("CM10_1975H_Bio_V1.2/CM10_1975H_Bio20_V1.2.txt",crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# ES.frame$annual_mean_radiation<-extract(annual_mean_radiation,lonlat, buffer=100000, fun=mean) # consider a buffer of radius=100km around each dot
# 
# ES.frame.noLU$annual_mean_radiation<-extract(annual_mean_radiation,lonlat.noLU, buffer=100000, fun=mean) # consider a buffer of radius=100km around each dot

## GDP
# if (file.exists("Data_Extract_From_World_Development_Indicators.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/v00kxpmll1pb5fm/Data_Extract_From_World_Development_Indicators.zip?dl=1", "Data_Extract_From_World_Development_Indicators.zip", mode="wb")
#   unzip("Data_Extract_From_World_Development_Indicators.zip")
# } else {
#   unzip("Data_Extract_From_World_Development_Indicators.zip")
# }
# 
# GDP.pc <- read.csv("Data_Extract_From_World_Development_Indicators_Data.csv",na.strings="..")
# 
# GDP.pc.2000 <- data.frame(Country.Code=GDP.pc$Country.Code,GDP.pc.2000=GDP.pc$X2000)
# 
# ES.frame <- join(ES.frame,GDP.pc.2000,by="Country.Code")
# ES.frame.noLU <- join(ES.frame.noLU,GDP.pc.2000,by="Country.Code")
# 

## Population density
# if (file.exists("Population_density.zip")==FALSE){
#   download.file("https://www.dropbox.com/s/1pgyh8jyvynjlca/Population_density.zip?dl=1", "Population_density.zip", mode="wb")
#   unzip("Population_density.zip")
# } else {
#   unzip ("Population_density.zip")
# }
# 
# pop.data <- raster("gldens00/glds00ag")
# 
# ES.frame$pop.dens.2000 <- log10(extract(pop.data,lonlat, buffer=100000, fun=mean)) # consider a buffer of radius=100km² around each dot)
# 
# ES.frame.noLU$pop.dens.2000 <- log10(extract(pop.data,lonlat.noLU, buffer=100000, fun=mean)) # consider a buffer of radius=100km² around each dot)
