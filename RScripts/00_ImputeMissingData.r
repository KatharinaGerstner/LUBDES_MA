############################################################################
### load LUBDES coding table
# TO DO: include query to be able to authorize
#source("RScripts/-01_load_table_directly_from_google.R")

### set working directory
setwd("C:\\Users\\hoppek\\Documents\\GitHub\\LUBDES_MA") #KG
#setwd("C:\\Users\\kambach\\Desktop\\aktuelle Arbeiten\\SESYNC\\myAnalysis") #SK
#setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB

### read .csv file directly downloaded from google docs without any changes
data <- read.csv("Input/LUBDES coding table v2 - 1. Coding Table version 2.csv",na.strings=c("NA",""))

### alternatively (and more elegant) read data DIRECTLY from google docs as described in script -01
library(devtools)
devtools::install_github("jennybc/googlesheets") # documentation at:http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html and https://github.com/jennybc/googlesheets
library(googlesheets)
gs_ls() # once authorized, this will list the files you have in GS
LUBDES_gsheet<- gs_title("LUBDES coding table v2") # load LUBDES  coding table, this crashes sometimes but seems to work as of April 22 2015
data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2") # consume data from sheet 1, # NOTE: data loaded way is of classes 'tbl_df' and 'data.frame', rather than only 'data.frame' which is needed for imputation.
data<-as.data.frame(data) 

#names(data)
str(data) # check variable types 
data$X..of.samples.for.BD.measure <- as.integer(data$X..of.samples.for.BD.measure)

#dissmiss studies with missing mean for BD or yield
data <- data[-(which(is.na(data$richness.mean))),]
data <- data[-(which(is.na(data$yield.mean))),]

################################
### create study-case identifier
data$study.case <- factor(paste(data$Study.ID,data$Case.ID,sep="_"))

#######################################################################
### apply imputation methods, impute BD and yield sd and then calculate se
library(mi)

### specify data frame for imputation
# Adding "latitude..N..S.", "longitude..E..W.", "Land.cover", "PES.category" cause error
# TO Do: standardize sampled.size.unit for BD and yield, so that sampled.area has the same unit overall, then sampled.size.unit becomes obsolete 
#data2impute <- data[,c("study.type", "Country", "Land.use...land.cover", "Intensity.broad", "Fertilization", "Irrigation", "Pesticides", "Grazing", "Mowing", "Clear.Cut.y.n.", "Selective.Logging.y.n.", "Partial.Logging.y.n.", "species.group", "trophic.level..species.guild", "richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "sampled.area", "sampled.size.unit", "product", "yield.unit", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure", "sampled.size.area", "sampled.size.unit.1")]
data2impute <- data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]

### convert the data.frame to a missing_data.frame, which is an enhanced version of a data.frame that includes metadata about the variables that is essential in a missing data context
mi.df <- missing_data.frame(data2impute) 

### check whether the missing_data.frame constructor function initially guessed the appropriate class for each missing_variable, if not use change() 
show(mi.df)
mi.df <- change_type(mi.df, c("X..of.samples.for.BD.measure","X..of.samples.for.YD.measure"), to="count")
# mi.df <- change_link(mi.df, c("richness.SD","yield.SD"), to="log") # to ensure SD is estimated as positive number, however this does not work and cause errror when calling the mi-function: Error in checkForRemoteErrors(val) : 2 nodes produced errors; first error: cannot find valid starting values: please specify some

### get a sense of the raw data and their missingness patterns
image(mi.df) 

### use the mi function to do the actual imputation, specify how many independent chains to utilize, how many iterations to conduct, and the maximum amount of time the user is willing to wait for all the iterations of all the chains to finish
imputations <- mi(mi.df, n.iter = 20, n.chains = 4, max.minutes = 20) 

### check convergence by calculating the mean over imputation chains
(data.imp <- round(mipply(imputations, mean, to.matrix = TRUE), 3))

###  Complete the missing data frame, take results fo the mth imputation chain
data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] <- complete(imputations, m=1)[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]
summary(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")])

### calculate SE for richness and yield mean
data$richness.SE <- data$richness.SD/sqrt(data$X..of.samples.for.BD.measure)
data$yield.SE <- data$yield.SD/sqrt(data$X..of.samples.for.YD.measure)

##########################################################
### Alternatively we use the mice-package

data <- read.csv("Input/LUBDES coding table v2 - 1. Coding Table version 2.csv",na.strings=c("NA",""))

# alternatively:
data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2") # consume data from sheet 1, # NOTE: data loaded way is of classes 'tbl_df' and 'data.frame', rather than only 'data.frame' which is needed for imputation.
data<-as.data.frame(data) 

#names(data)
str(data) # check variable types 
data$X..of.samples.for.BD.measure <- as.integer(data$X..of.samples.for.BD.measure)

################################
### create study-case identifier
data$study.case <- factor(paste(data$Study.ID,data$Case.ID,sep="_"))

#dissmiss studies with missing mean for BD or yield
data <- data[-(which(is.na(data$richness.mean))),]
data <- data[-(which(is.na(data$yield.mean))),]
detach("package:mi", unload=TRUE)
library(mice)

imp <- mice(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")])
data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] <- complete(imp)

### calculate SE for richness and yield mean
data$richness.SE <- data$richness.SD/sqrt(data$X..of.samples.for.BD.measure)
data$yield.SE <- data$yield.SD/sqrt(data$X..of.samples.for.YD.measure)

### check results, e.g. SD must be positive or SE will be NaN
summary(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]) # does not produce negative SDs

##################################
### save imputed data as Rdata
save(data, file="Input/data.Rdata")
