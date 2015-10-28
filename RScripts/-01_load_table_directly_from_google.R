############################################################################
### Purpose of this skript module -01 is to:
###
### -01.1. set the working directory
### -01.2. load all libraries needed for subsequent analysis
### -01.3. load data directly from google docs
### -01.4. adapt data structure
###
### Some general comments on the structure of the LUBDES_MA scripts:
### * Each module (-01, 00, 01 ...) builds on the previous ones and does NOT work standalone - this avoids redundancies.
### * Exceptions to that rule are -02 and -03 which should not be run by anyone other than MB.
### * Data, functions etc. will be carried over from one module to another. Saving and loading of interim outputs is unwanted. 
### * In case interim data needs to be saved it must NOT be saved in the git directory. This will avoid that accidentally our whole dataset is visible to everyone on the internet. 
### * All needed output files (tables, plots etc.) must NOT be saved in the git directory but in the LUBDES_MA dropbox folders instead.
### * Hidden files like .rhistory .oauth etc must NOT be commited to github. This avoids errors. Also, uploading the authorization token for google docs to github might allow access to google accounts by hackers (presumably, #MB)
### * The data input comes directly from google docs. loading .csv is deprecated.
### * created functions should be own sub-sections (i.e. 01.03 blabla function)
###
### Overall structure of the modules is:
### [-03.1. To extract zotero citation data into excel.]
### [-02.1. set working directory to create folders in]
### [-02.2. create directory structure based on study IDs and place an empty note file within]
### -01.1. set the working directory
### -01.2. load all libraries needed for subsequent analysis
### -01.3. load data directly from google docs
### -01.4. adapt data structure
### -01a.1. load screening data directly from google docs
### -01a.2. plot pie charts about statistics
### 00.1. impute missing data using mice package
### 00.2. impute missing data using mi package 
### 01.1. table.sort function
### 01.2. Compile ES frame
### 01.3. Calculate response ratio effect sizes
### 01a.1. Intersect studies with global maps
### 02.1. Prepare data analysis
### 02.2. LMM.MA.fit function
### 02.3. Analysis without moderators
### 02.4. Analysis with moderators
### 03.1. Specify output directory in the dropbox or other local folder - NOT in the git directory!
### 03.2. Plot map of studies
### 03.3. Plot cross-diagrams
###
### Authors: MB, KG, ...
############################################################################

############################################################################
### -01.1. set the working directory
###
### all lines should stay commented out, only temporarily set your wd
############################################################################

#setwd("C:\\Users\\hoppek\\Documents\\GitHub\\LUBDES_MA") #KG
#setwd("C:\\Users\\kambach\\Desktop\\aktuelle Arbeiten\\SESYNC\\myAnalysis") #SK
#setwd("~/git/LUBDES_MA") #MB

############################################################################
### -01.2. load all libraries needed for subsequent analysis
###
### 
############################################################################

### first timers should run:
# install.packages("devtools","mice","mi","metafor","ggplot2","scales")
# library(devtools)
# devtools::install_github("jennybc/googlesheets") # documentation at:http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html and https://github.com/jennybc/googlesheets

library(devtools)
library(googlesheets)
library(mice)
#library(mi)
library(metafor)
library(ggplot2) # For plotting
library(scales)
library(maps) # For map data
library(rworldmap)
#library(geosphere)
library(raster)
#library(shapefiles)
#library(sp)
require(rgdal)
library(rgeos)

############################################################################
### -01.3. load data directly from google docs
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

###########################################################################
### DEPRECATED
# ### read .csv file directly downloaded from google docs without any changes
# data <- read.csv("Input/LUBDES coding table v2 - 1. Coding Table version 2.csv",na.strings=c("NA",""))


############################################################################
### -01.4. adapt data structure
###
### 
############################################################################

### check variable types 
str(data)
data$X..of.samples.for.BD.measure <- as.integer(data$X..of.samples.for.BD.measure)

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

