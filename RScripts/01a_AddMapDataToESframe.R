############################################################################
### Purpose of this skript module 01a is to:
###
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
###
### General comments:
### * TO DO: download maps and store them, link global data using coordinates and countries
###   for continuous data such as land use history, NPP
###
### Authors: MB ...
############################################################################

# set a wd suitable for downloading and extracting Ecoregions shapefile
setwd("C:/Users/hoppek/Documents/temp") # KG
#setwd("/tmp") #MB

############################################################################
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
### 
### 
############################################################################

#download.file("http://maps.tnc.org/files/shp/terr-ecoregions-TNC.zip", "terr-ecoregions-TNC.zip")
#unzip("terr-ecoregions-TNC.zip")

ecoregions <- readOGR(".","tnc_terr_ecoregions")

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

#download.file("http://databank.worldbank.org/data/reports.aspx?source=2&type=metadata&series=NY.GDP.MKTP.CD", destfile="GDP_2000")
unzip("GDP_per_capita.zip")
GDP.pc <- read.csv("ny.gdp.pcap.cd_Indicator_en_csv_v2.csv",skip=4)
GDP.pc.2000 <- data.frame(Country.Code=GDP.pc$Country.Code,GDP.pc.2000=GDP.pc$X2000)

ES.frame <- join(ES.frame,GDP.pc.2000,by="Country.Code")
ES.frame.noLU <- join(ES.frame.noLU,GDP.pc.2000,by="Country.Code")

### remove objectes to save workspace
rm(ecoregions,lonlat,realms_extract,lonlat.noLU,GDP.pc,GDP.pc.2000)
