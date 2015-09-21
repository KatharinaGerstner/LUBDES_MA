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
### * Data, functions etc. will be carried over from one module to another. Saving and loading of interim outputs is unwanted. 
### * In case interim data needs to be saved it must NOT be saved in the git directory. This will avoid that accidentally our whole dataset is visible to everyone on the internet. 
### * All needed output files (tables, plots etc.) must NOT be saved in the git directory but in the LUBDES_MA dropbox folders instead.
### * Hidden files like .rhistory .oauth etc must NOT be commited to github. This avoids errors. Also, uploading the authorization token for google docs to github might allow access to google accounts by hackers (presumably, #MB)
### * The data input comes directly from google docs. loading .csv is deprecated.
### * created functions should be own sub-sections (i.e. 01.03 blabla function)
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

### dissmiss studies with missing mean for BD or yield
data <- data[-(which(is.na(data$richness.mean))),]
data <- data[-(which(is.na(data$yield.mean))),]




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

# ### plot study locations
# library(maps) # For map data
# library(ggplot2)
# world_map <- map_data("world")
# p <- ggplot(legend=FALSE) +
#   geom_polygon(data=world_map, aes(x=long, y=lat)) + 
#   geom_point(data=data, aes(x=longitude..E..W., y=latitude..N..S.), color="blue")
# p ## looks weird, the reason is the max latitude in data = 2011! - check!
# ggsave("Output/CaseDistribution.pdf", width=8, height=8, units="cm")