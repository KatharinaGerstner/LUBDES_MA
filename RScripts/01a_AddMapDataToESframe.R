############################################################################
### Purpose of this skript module 01a is to:
###
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
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

############################################################################
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
### 
### 
############################################################################

download.file("https://www.dropbox.com/s/ihivf2ie98wxvee/terr-ecoregions-TNC.zip?dl=1", "terr-ecoregions-TNC.zip", mode="wb")
unzip("terr-ecoregions-TNC.zip")

ecoregions <- readOGR(path2temp,"tnc_terr_ecoregions")

# remove unneeded information for extraction, the extract command requests this form of data
ES.frame <- ES.frame[!is.na(ES.frame$Longitude+ES.frame$Latitude),] # remove NA lonlat
lonlat <- cbind(ES.frame$Longitude,ES.frame$Latitude)

# extract ecoregions
realms_extract <- extract(ecoregions,lonlat)
ES.frame <- cbind(ES.frame,realms_extract$WWF_MHTNAM)
colnames(ES.frame)[which(names(ES.frame) == "realms_extract$WWF_MHTNAM")]<-"BIOME"

### for ES.frame.noLU
# remove unneeded information for extraction, the extract command requests this form of data
ES.frame.noLU <- ES.frame.noLU[!is.na(ES.frame.noLU$Longitude+ES.frame.noLU$Latitude),] # remove NA lonlat
lonlat.noLU <- cbind(ES.frame.noLU$Longitude,ES.frame.noLU$Latitude)

# extract ecoregions
realms_extract <- extract(ecoregions,lonlat.noLU)
ES.frame.noLU <- cbind(ES.frame.noLU,realms_extract$WWF_MHTNAM)
colnames(ES.frame.noLU)[which(names(ES.frame.noLU) == "realms_extract$WWF_MHTNAM")]<-"BIOME"

############################################################################
### 01a.2. Intersect studies with global maps of GDP per capita
############################################################################


download.file("https://www.dropbox.com/s/v00kxpmll1pb5fm/Data_Extract_From_World_Development_Indicators.zip?dl=1", "Data_Extract_From_World_Development_Indicators.zip", mode="wb")
unzip("Data_Extract_From_World_Development_Indicators.zip")

GDP.pc <- read.csv("Data_Extract_From_World_Development_Indicators_Data.csv",na.strings="..")

GDP.pc.2000 <- data.frame(Country.Code=GDP.pc$Country.Code,GDP.pc.2000=GDP.pc$X2000)

ES.frame <- join(ES.frame,GDP.pc.2000,by="Country.Code")
ES.frame.noLU <- join(ES.frame.noLU,GDP.pc.2000,by="Country.Code")

############################################################################
### 01a.3. Intersect studies with annual mean radiation (Climond)
############################################################################

download.file("https://www.dropbox.com/s/me92v7ozmixy75a/CM10_1975H_Bio_ASCII_V1.2.zip?dl=1", "CM10_1975H_Bio_ASCII_V1.2.zip", mode="wb")
unzip("CM10_1975H_Bio_ASCII_V1.2.zip")

annual_mean_radiation <- raster("CM10_1975H_Bio_V1.2/CM10_1975H_Bio20_V1.2.txt")

lonlat<-cbind(ES.frame$Longitude,ES.frame$Latitude)
ES.frame$annual_mean_radiation<-extract(annual_mean_radiation,lonlat)

lonlat<-cbind(ES.frame.noLU$Longitude,ES.frame.noLU$Latitude)
ES.frame.noLU$annual_mean_radiation<-extract(annual_mean_radiation,lonlat)

############################################################################
### 01a.4. Intersect studies with gross capital stock in agriculture
############################################################################

download.file("https://www.dropbox.com/s/xgqrmyiqx2lqvyl/Investment_CapitalStock_E_All_Data.zip?dl=1", "Investment_CapitalStock_E_All_Data.zip", mode="wb")
unzip("Investment_CapitalStock_E_All_Data.zip")

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

download.file("https://www.dropbox.com/s/bwpzna0y4e1t77e/Dissimilarity_01_05_25km_uint32.tif?dl=1", "habitat_dissimilarity.tif", mode="wb")

habitat_dissimilarity <- raster("habitat_dissimilarity.tif")

lonlat<-cbind(ES.frame$Longitude,ES.frame$Latitude)
ES.frame$habitat_dissimilarity<-extract(habitat_dissimilarity,lonlat)

lonlat<-cbind(ES.frame.noLU$Longitude,ES.frame.noLU$Latitude)
ES.frame.noLU$habitat_dissimilarity<-extract(habitat_dissimilarity,lonlat)


### remove objectes to save workspace
rm(ecoregions,lonlat,realms_extract,lonlat.noLU,GDP.pc,GDP.pc.2000,annual_mean_radiation,capital_stock_in_agriculture,habitat_dissimilarity)

setwd(path2wd)