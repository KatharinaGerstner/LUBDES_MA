############################################################################
### Purpose of this skript module 01 is to:
### 
### 01.1. load all libraries needed for subsequent analysis
###
### Authors: KG, MB ...
############################################################################

############################################################################
### 01.1. load all libraries needed for subsequent analysis
############################################################################

### first timers should run:
# install.packages("devtools","mice","mi","metafor","ggplot2","scales")
# library(devtools)
# devtools::install_github("jennybc/googlesheets") # documentation at:http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html and https://github.com/jennybc/googlesheets

library(devtools) # needed for library googlesheets
library(googlesheets) # for loading data directly from google
library(mice) # for imputation
#library(mi) # for imputation
library(metafor) # for meta analysis
library(ggplot2) # For plotting
library(scales) # ?
library(maps) # still needed? #For map data
library(rworldmap) # still needed?
#library(geosphere)
library(raster) # for adding map data
#library(shapefiles)
#library(sp)
require(rgdal) # for loading map data
library(rgeos) # dependency for rgdal
library(RColorBrewer) # fancy color schemes for plotting
library(plyr) # for joining datasets
library(countrycode) # convert FAO country IDs to ISO3
library(VennDiagram)
# library(venneuler)
library(reshape2)

source(path2wd %+% "RMASelect.R")
