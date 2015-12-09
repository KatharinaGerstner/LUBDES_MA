############################################################################
### Purpose of this skript module 02 is to:
###
### 02.1. load data directly from google docs
### 02.2. adapt data structure
###
### Authors: MB, KG
############################################################################

############################################################################
##
## Additional Functions for standardisation
##
############################################################################
convertYieldUnits <- function(data){
	if(!("yield.unit" %in% names(data))){stop("There must be a column called 'yield.unit'")}
	if(!("yield.mean" %in% names(data))){stop("There must be a column called 'yield.mean'")}
	
	new_units <- data$yield.unit	
	new_means <- data$yield.mean
	
	## getting rid of inconsistencies with names
	new_units <- ifelse(!is.na(new_units) & new_units == "cm grass height", "cm", new_units)
	new_units <- ifelse(!is.na(new_units) & new_units == "Mg/ha", "t/ha", new_units)
	new_units <- ifelse(!is.na(new_units) & new_units == "m³/ha/year", "m³/ha", new_units)


	new_means  <- ifelse(!is.na(new_units) & new_units == "cm", new_means/100, new_means)
	new_units <- ifelse(!is.na(new_units) & new_units == "cm", "m", new_units)	
	
	new_means  <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", new_means*10000, new_means)
	new_units <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", "kg/m²", new_units)
	
	new_means  <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", new_means*100, new_means)
	new_units <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", "m³/ha", new_units)
	
	new_means  <- ifelse(!is.na(new_units) & new_units == "g/m²", new_means/1000, new_means)
	new_units <- ifelse(!is.na(new_units) & new_units == "g/m²", "kg/m²", new_units)
	new_means  <- ifelse(!is.na(new_units) & new_units == "kg/m²", new_means*10, new_means)
	new_units <- ifelse(!is.na(new_units) & new_units == "kg/m²", "t/ha", new_units)
	
	data$yield.unit <- new_units
	data$yield.mean <- new_means
	return(data)
}

convertAreaUnits <- function(data, type=c("bd", "yield")){
	if(type == "yield"){
		if(!("sampled.size.area" %in% names(data))){stop("There must be a column called 'sampled.size.area'")}
		if(!("sampled.size.unit.1" %in% names(data))){stop("There must be a column called 'sampled.size.unit.1'")}
		new_units <- data$sampled.size.unit.1
		new_area <- as.numeric(data$sampled.size.area)
		}
	
	if(type == "bd"){
		if(!("sampled.area" %in% names(data))){stop("There must be a column called 'sampled.area'")}
		if(!("sampled.size.unit" %in% names(data))){stop("There must be a column called 'sampled.size.unit'")}
		new_units <- data$sampled.size.unit
		new_area <- as.numeric(data$sampled.area)
		}
	new_area  <- ifelse(!is.na(new_units) & new_units == "mÂ²", "m²", new_area)
	
	new_area  <- ifelse(!is.na(new_units) & new_units == "ha", new_area * 10000, new_area)
	new_units <- ifelse(!is.na(new_units) & new_units == "ha", "m²", new_units)	
	
	new_area  <- ifelse(!is.na(new_units) & new_units == "cm²", new_area * 0.0001, new_area)
	new_units <- ifelse(!is.na(new_units) & new_units == "cm²", "m²", new_units)	
	
	new_area  <- ifelse(!is.na(new_units) & new_units == "mm²", new_area * 1e-6, new_area)
	new_units <- ifelse(!is.na(new_units) & new_units == "mm²", "m²", new_units)	
	
	new_area  <- ifelse(!is.na(new_units) & new_units == "km²", new_area * 1000000, new_area)
	new_units <- ifelse(!is.na(new_units) & new_units == "km²", "m²", new_units)	
	
	if(type == "yield"){
		data$sampled.size.unit.1 <- new_units
		data$sampled.size.area <- as.numeric(new_area)
		}
	if(type == "bd"){
		data$sampled.size.unit <- new_units
		data$sampled.area <- as.numeric(new_area)
		}
	return(data)
}

SortTransectsTraps <- function(data){
	if(!("sampled.size.unit" %in% names(data))){stop("There must be a column called 'sampled.size.unit'")}
	
	new_units <- data$sampled.size.unit
	new_area <- as.numeric(data$sampled.area)
	
	transects <- c("points/transect", "transect", "transect (km)","transect (m)")
	new_units <- ifelse(new_units %in% transects, "transects", new_units)	
	new_area <- ifelse(new_units %in% transects, NA, new_area)
	
	traps <- c("traps", "traps (mistnets)")
	new_units <- ifelse(new_units %in% traps, "traps", new_units)	
	new_area <- ifelse(new_units %in% traps, NA, new_area)
	
	data$sampled.size.unit <- new_units
	data$sampled.area <- new_area

	return(data)
}

############################################################################
### 02.1. load data directly from google docs
###
### 
############################################################################

### authorize with google docs, the first time or in a new session:
### follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
gs_ls() 
gs_ls() #once authorized, this will list the files you have in GS

### in case there are authorization problems reset the authorization token
#gs_auth(new_user = TRUE)

### load LUBDES  coding table
LUBDES_gsheet<- gs_title("LUBDES coding table v2") #this crashes sometimes but seems to work as of April 22 2015
data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2") #consume data from sheet 1
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
data <- convertYieldUnits(data)
data <- convertAreaUnits(data, type = "yield")
data <- convertAreaUnits(data, type = "bd")
data <- SortTransectsTraps(data)

### dissmiss studies with missing mean for BD or yield
data <- data[-(which(is.na(data$richness.mean))),]
data <- data[-(which(is.na(data$yield.mean))),]

### create study-case identifier
data$study.case <- factor(paste(data$Study.ID,data$Case.ID,sep="_"))




###########################################################################
### Resterampe

# ### OBSOLETE since empty rows have been deleted
# #try again with copy of first few rows without empty rows
# list_sheets()
# LUBDES_gsheet<-register_ss("LUBDES_coding_copy")
# data <- get_via_lf(LUBDES_gsheet, ws = "Sheet1") #seems ok
# 
# #try again with copy of entire sheet without without empty rows
# gs_ls()
# LUBDES_gsheet<-register_ss("LUBDES_coding_copy")
# data <- get_via_lf(LUBDES_gsheet, ws = "Sheet2") #seems ok
# 
# # so the problem is in the first few empty rows ...

# write.csv(data, "Input/LUBDES coding table v2 - 1. Coding Table version 2.csv")

