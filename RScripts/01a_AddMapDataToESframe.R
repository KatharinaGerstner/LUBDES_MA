############################################################################
### Purpose of this skript module 01a is to:
###
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
###
### General comments:
### * TO DO: link global data using coordinates and countries
###   biomes, land use history, NPP
###
### Authors: MB ...
############################################################################

############################################################################
### 01a.1. Intersect studies with global maps of WWF_REALMs Ecoregions
### 
### 
############################################################################

# set a wd suitable for downloading and extracting Ecoregions shapefile
setwd("/tmp") #MB

download.file("http://maps.tnc.org/files/shp/terr-ecoregions-TNC.zip", "terr-ecoregions-TNC.zip")
unzip("terr-ecoregions-TNC.zip")

ecoregions<-readOGR(".","tnc_terr_ecoregions")

# remove unneeded information for extraction, the extract command requests this form of data
lonlat<-cbind(ES.frame$Longitude,ES.frame$Latitude)

# extract ecoregions
realms_extract<-extract(ecoregions,lonlat)
ES.frame<-cbind(ES.frame,realms_extract$WWF_REALM)
colnames(ES.frame)[which(names(ES.frame) == "realms_extract$WWF_REALM")]<-"WWF_REALM"

### WWF realms:
# AA	Australasia
# AN	Antarctic
# AT	Afrotropic
# IM	Indo-Malay
# NA	Nearctic
# NT	Neotropic
# OC	Oceania
# PA	Palearctic





