############################################################################
### Purpose of this skript module 02 is to:
###
### 02.1. load data directly from google docs
### 02.2. adapt data structure
###
### General comments:
###
### Authors: MB, KG
############################################################################

############################################################################
### 02.1. load data directly from google docs
###
### 
############################################################################

### authorize with google docs, the first time or in a new session:
### follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
gs_ls() #once authorized, this will list the files you have in GS

### in case there are authorization problems reset the authorization token
#gs_auth(new_user = TRUE)

### load LUBDES  coding table
LUBDES_gsheet<- gs_title("LUBDES coding table v2") #this crashes sometimes but seems to work as of April 22 2015
data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2",check.names=TRUE) #consume data from sheet 1
data<-as.data.frame(data) #some functions don't like the tbl.df data type

############################################################################
### 02.2. adapt data structure
###
### 
############################################################################

### check variable types 
str(data)
data$X..of.samples.for.BD.measure <- as.integer(data$X..of.samples.for.BD.measure)

## Standardise units
data <- convertAreaUnits(data, type = "bd")

data$Yield.Unit.Type <- NA
data$Yield.Unit.Type[data$yield.unit %in% c("% conifer", "% fruit set", "% of trees of original volume removed","fruit/sq.m.", "treecover (%)", "trees/ha", "trees/year")] <- "Count/area"
data$Yield.Unit.Type[data$yield.unit %in% c("g", "kg / animal / day", "kg/tree", "g/m²", "kg/m²", "kg/ha","kg/hm²a","kg/ha/year","t/ha", "Mg/ha")] <- "Mass/area"
data$Yield.Unit.Type[data$yield.unit %in% c("m³/ha","m³/0.01 ha","m³/ha/year")] <- "Volume/area"
data$Yield.Unit.Type[data$yield.unit %in% c("cm","cm grass height","m", "m²/ha" )] <- "Area/area"
data$Yield.Unit.Type[is.na(data$yield.unit)] <- "Others"

### dissmiss studies with missing mean for BD or yield
data <- data[!is.na(data$richness.mean),]
data <- data[!is.na(data$yield.mean),]

### create study-case identifier
data$study.case <- factor(paste(data$Study.ID,data$Case.ID,sep="_"))


