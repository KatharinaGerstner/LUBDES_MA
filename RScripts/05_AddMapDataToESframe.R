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

### reset path2wd
setwd(path2wd)
