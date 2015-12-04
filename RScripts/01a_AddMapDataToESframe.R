############################################################################
### Purpose of this skript module 01a is to:
###
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
### 01a.2. Intersect studies with global maps of climate zones (Köppen-Geiger)
### 01a.2. Intersect studies with global maps of GDP per capita
### 01a.3. Intersect studies with annual mean radiation (Climond)
### 01a.4. Intersect studies with gross capital stock in agriculture
### 01a.5. Intersect studies with Agricultural intensity (efficiency) in the neighborhood
### 01a.6. Intersect studies with Global Habitat Heterogeneity, Dissimilarity
###
### General comments:
### * TO DO: download maps and store them, link global data using coordinates and countries
###   for continuous data such as land use history, NPP
###
### Authors: MB ...
############################################################################

# set a wd suitable for downloading and extracting Ecoregions shapefile
setwd(path2temp %+% "/") 

# remove unneeded information for extraction, the extract command requests this form of data
ES.frame <- ES.frame[!is.na(ES.frame$Longitude+ES.frame$Latitude),] # remove NA lonlat
lonlat <- cbind(ES.frame$Longitude,ES.frame$Latitude)

ES.frame.noLU <- ES.frame.noLU[!is.na(ES.frame.noLU$Longitude+ES.frame.noLU$Latitude),] # remove NA lonlat
lonlat.noLU <- cbind(ES.frame.noLU$Longitude,ES.frame.noLU$Latitude)

############################################################################
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
############################################################################

if (file.exists("terr-ecoregions-TNC.zip")==FALSE){
  download.file("https://www.dropbox.com/s/ihivf2ie98wxvee/terr-ecoregions-TNC.zip?dl=1", "terr-ecoregions-TNC.zip", mode="wb")
  unzip("terr-ecoregions-TNC.zip")
  } else {unzip("terr-ecoregions-TNC.zip")}

ecoregions <- readOGR(path2temp,"tnc_terr_ecoregions")

# extract ecoregions
realms_extract <- extract(ecoregions,lonlat)
ES.frame <- cbind(ES.frame,realms_extract$WWF_MHTNAM)
colnames(ES.frame)[which(names(ES.frame) == "realms_extract$WWF_MHTNAM")]<-"BIOME"

### for ES.frame.noLU
# extract ecoregions
realms_extract <- extract(ecoregions,lonlat.noLU)
ES.frame.noLU <- cbind(ES.frame.noLU,realms_extract$WWF_MHTNAM)
colnames(ES.frame.noLU)[which(names(ES.frame.noLU) == "realms_extract$WWF_MHTNAM")]<-"BIOME"

############################################################################
### 01a.2. Intersect studies with global maps of climate zones (Köppen-Geiger)
############################################################################
if (file.exists("1976-2000_GIS.zip")==FALSE){
  download.file("http://koeppen-geiger.vu-wien.ac.at/data/1976-2000_GIS.zip","1976-2000_GIS.zip", mode="wb")
  unzip("1976-2000_GIS.zip")
} else {unzip("1976-2000_GIS.zip")}
climate_zone <- readOGR(path2temp,"1976-2000")
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
ES.frame$main_climate <- cut(climate_extract$GRIDCODE, breaks=c(10,20,30,40,50,60), labels=c("equatorial","arid","warm temperature","snow","polar"))

### for ES.frame.noLU
climate_extract <- extract(climate_zone,lonlat.noLU)
ES.frame.noLU$main_climate <- cut(climate_extract$GRIDCODE, breaks=c(10,20,30,40,50,60), labels=c("equatorial","arid","warm temperature","snow","polar"))

# 

############################################################################
### 01a.2. Intersect studies with global maps of GDP per capita
############################################################################

if (file.exists("Data_Extract_From_World_Development_Indicators.zip")==FALSE){
  download.file("https://www.dropbox.com/s/v00kxpmll1pb5fm/Data_Extract_From_World_Development_Indicators.zip?dl=1", "Data_Extract_From_World_Development_Indicators.zip", mode="wb")
  unzip("Data_Extract_From_World_Development_Indicators.zip")
} else {
  unzip("Data_Extract_From_World_Development_Indicators.zip")
}

GDP.pc <- read.csv("Data_Extract_From_World_Development_Indicators_Data.csv",na.strings="..")

GDP.pc.2000 <- data.frame(Country.Code=GDP.pc$Country.Code,GDP.pc.2000=GDP.pc$X2000)

ES.frame <- join(ES.frame,GDP.pc.2000,by="Country.Code")
ES.frame.noLU <- join(ES.frame.noLU,GDP.pc.2000,by="Country.Code")

############################################################################
### 01a.3. Intersect studies with annual mean radiation (Climond)
############################################################################

if (file.exists("CM10_1975H_Bio_ASCII_V1.2.zip")==FALSE){
  download.file("https://www.dropbox.com/s/me92v7ozmixy75a/CM10_1975H_Bio_ASCII_V1.2.zip?dl=1", "CM10_1975H_Bio_ASCII_V1.2.zip", mode="wb")
  unzip("CM10_1975H_Bio_ASCII_V1.2.zip")
} else {
  unzip("CM10_1975H_Bio_ASCII_V1.2.zip")
}

annual_mean_radiation <- raster("CM10_1975H_Bio_V1.2/CM10_1975H_Bio20_V1.2.txt",crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ES.frame$annual_mean_radiation<-extract(annual_mean_radiation,lonlat, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot

ES.frame.noLU$annual_mean_radiation<-extract(annual_mean_radiation,lonlat.noLU, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot

############################################################################
### 01a.4. Intersect studies with gross capital stock in agriculture
############################################################################

if (file.exists("Investment_CapitalStock_E_All_Data.zip")==FALSE){
  download.file("https://www.dropbox.com/s/xgqrmyiqx2lqvyl/Investment_CapitalStock_E_All_Data.zip?dl=1", "Investment_CapitalStock_E_All_Data.zip", mode="wb")
  unzip("Investment_CapitalStock_E_All_Data.zip")
} else {
  unzip("Investment_CapitalStock_E_All_Data.zip")
}

capital_stock_in_agriculture <- read.csv("Investment_CapitalStock_E_All_Data.csv")
capital_stock_in_agriculture <- subset(capital_stock_in_agriculture, Item == "Capital Stock + (Total)" & Element == "Gross Capital Stock (constant 2005 prices)")

# convert fao codes into iso3c codes
capital_stock_in_agriculture$CountryCode <- countrycode(capital_stock_in_agriculture$CountryCode,"fao","iso3c")
capital_stock_in_agriculture <- data.frame(Country.Code = capital_stock_in_agriculture$CountryCode,
                                           capital_millionUSD = capital_stock_in_agriculture$Value,
                                           Date.End4CS = capital_stock_in_agriculture$Year)
capital_stock_in_agriculture <- capital_stock_in_agriculture[!is.na(capital_stock_in_agriculture$Country.Code),] ## exclude Country.Code==NA from merging as this produces duplicates

###
ES.frame$Date.End4CS <- ES.frame$Date.End
ES.frame$Date.End4CS[ES.frame$Date.End4CS>2007] <- 2007 ## use capital stock data from 2007 if study ends after

ES.frame <- join(ES.frame,capital_stock_in_agriculture,by=c("Country.Code","Date.End4CS"))
ES.frame <- ES.frame[,-which(names(ES.frame)=="Date.End4CS")] ## remove previously added column

###
ES.frame.noLU$Date.End4CS <- ES.frame.noLU$Date.End
ES.frame.noLU$Date.End4CS[ES.frame.noLU$Date.End4CS>2007] <- 2007 ## use capital stock data from 2007 if study ends after

ES.frame.noLU <- join(ES.frame.noLU,capital_stock_in_agriculture,by=c("Country.Code","Date.End4CS"))
ES.frame.noLU <- ES.frame.noLU[,-which(names(ES.frame.noLU)=="Date.End4CS")] ## remove previously added column

############################################################################
### 01a.5. Intersect studies with Agricultural intensity (efficiency) in the neighborhood
############################################################################

### not yet working

############################################################################
### 01a.6. Intersect studies with Global Habitat Heterogeneity, Dissimilarity
############################################################################

### data from http://www.earthenv.org/texture.html

if (file.exists("habitat_dissimilarity.tif")==FALSE){
  download.file("https://www.dropbox.com/s/bwpzna0y4e1t77e/Dissimilarity_01_05_25km_uint32.tif?dl=1", "habitat_dissimilarity.tif", mode="wb")
  habitat_dissimilarity <- raster("habitat_dissimilarity.tif")
} else {
  habitat_dissimilarity <- raster("habitat_dissimilarity.tif")
}
  
habitat_dissimilarity <- raster("habitat_dissimilarity.tif")

ES.frame$habitat_dissimilarity<-extract(habitat_dissimilarity,lonlat, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot)

ES.frame.noLU$habitat_dissimilarity<-extract(habitat_dissimilarity,lonlat.noLU, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot)

############################################################################
### 01a.7. Intersect studies with Land-use history
############################################################################

if (file.exists("ellis_etal_2013_dataset.zip")==FALSE){
  download.file("https://www.dropbox.com/s/6qkr58sjimclpst/ellis_etal_2013_dataset.zip?dl=1", "ellis_etal_2013_dataset.zip", mode="wb")
  unzip("ellis_etal_2013_dataset.zip")
} else {
  unzip ("ellis_etal_2013_dataset.zip")
}

timeseries.hyde <- list("sus_hbc6000","sus_hbc3000","sus_hbc1000","sus_had0","sus_had1000","sus_had1500","sus_had1750","sus_had1900","sus_had1950","sus_had2000")
timeseries.kk10 <- list("sus_kbc6000","sus_kbc3000","sus_kbc1000","sus_kad0","sus_kad1000","sus_kad1500","sus_kad1750","sus_kad1900","sus_kad1950","sus_kad2000")

hyde.LUhist.stack <- stack(lapply(timeseries.hyde,function(x) raster("hyde/sus_use/" %+% x)))
kk10.LUhist.stack <- stack(lapply(timeseries.kk10,function(x) raster("kk10/sus_use/" %+% x)))

## ES.frame
hyde.extract.year.of.first.use <- extract(hyde.LUhist.stack,lonlat) 
names(hyde.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
hyde.year.of.first.use <- apply(hyde.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(hyde.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
kk10.extract.year.of.first.use <- extract(kk10.LUhist.stack,lonlat) 
names(kk10.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
kk10.year.of.first.use <- apply(kk10.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(kk10.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
ES.frame$year.of.first.use <- apply(cbind(hyde.year.of.first.use,kk10.year.of.first.use),1,function(x){ifelse(all(is.na(x)),NA,min(x,na.rm=T))})
ES.frame$start.agr.use <- ifelse(ES.frame$year.of.first.use < 1500,"old","young")
ES.frame$start.agr.use[is.na(ES.frame$start.agr.use)] <- "not yet used"

## ES.frame.noLU
hyde.extract.year.of.first.use <- extract(hyde.LUhist.stack,lonlat.noLU) 
names(hyde.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
hyde.year.of.first.use <- apply(hyde.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(hyde.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
kk10.extract.year.of.first.use <- extract(kk10.LUhist.stack,lonlat.noLU) 
names(kk10.extract.year.of.first.use) <- c("-6000","-3000","-1000","0","1000","1500","1750","1900","1950","2000")
kk10.year.of.first.use <- apply(kk10.extract.year.of.first.use,1,function(x){ifelse(sum(x)>0,as.numeric(names(kk10.extract.year.of.first.use)[min(which(x==1))]),NA)}) # NA if no significant use were detectable
ES.frame.noLU$year.of.first.use <- apply(cbind(hyde.year.of.first.use,kk10.year.of.first.use),1,function(x){ifelse(all(is.na(x)),NA,min(x,na.rm=T))})
ES.frame.noLU$start.agr.use <- ifelse(ES.frame.noLU$year.of.first.use < 1500,"old","young")
ES.frame.noLU$start.agr.use[is.na(ES.frame.noLU$start.agr.use)] <- "not yet used"

############################################################################
### 01a.8. Intersect studies with Population density
############################################################################

if (file.exists("Population_density.zip")==FALSE){
  download.file("https://www.dropbox.com/s/1pgyh8jyvynjlca/Population_density.zip?dl=1", "Population_density.zip", mode="wb")
  unzip("Population_density.zip")
} else {
  unzip ("Population_density.zip")
}

pop.data <- raster("gldens00/glds00ag")

ES.frame$pop.dens.2000 <- extract(pop.data,lonlat, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot)

ES.frame.noLU$pop.dens.2000 <- extract(pop.data,lonlat.noLU, buffer=10000, fun=mean) # consider a buffer of radius=10km² around each dot)

############################################################################
### 01a.9. Combine LUI classifiers
############################################################################

# not yet working

############################################################################
### remove objectes to save workspace
############################################################################
rm(lonlat, lonlat.noLU,ecoregions,climate_extract,realms_extract,GDP.pc,GDP.pc.2000,annual_mean_radiation,capital_stock_in_agriculture,habitat_dissimilarity, timeseries.hyde,timeseries.kk10,hyde.LUhist.stack,kk10.LUhist.stack,hyde.extract.year.of.first.use,kk10.extract.year.of.first.use,hyde.year.of.first.use,kk10.year.of.first.use)

setwd(path2wd)
