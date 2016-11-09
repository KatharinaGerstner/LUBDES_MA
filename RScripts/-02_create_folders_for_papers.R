############################################################################
### Purpose of this skript module -02 is to:
###
### -02.1. set working directory to create folders in
### -02.2. create directory structure based on study IDs and place an empty note file within
###
### General comments:
### * This skript is NOT needed to run when doing the analysis. It should work standalone
### * should be only used by MB
###
### Authors: MB, ...
############################################################################

############################################################################
### -02.1. set working directory to create folders in
### 
### all lines should stay commented out, only temporarily set your wd
############################################################################

############################################################################
### -02.2. create directory structure based on study IDs and place an empty note file within
### 
### this will not work on non-linux systems
############################################################################

### load LUBDES  coding table
LUBDES_gsheet<- gs_title("LUBDES coding table v2") #this crashes sometimes but seems to work as of April 22 2015
data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2") #consume data from sheet 1
data<-as.data.frame(data) #some functions don't like the tbl.df data type

### create Study.ID based folders
for (i in 1:(length(unique(data$Study.ID))))  {
  dir.create(paste("",unique(data$Study.ID)[i],sep=""))
}

### create study.ID based note files
setwd(paste("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/papers_for_coding",sep=""))
for (i in 1:(length(list.dirs())))  {
  x<-list.dirs()[i]
  setwd(paste(list.dirs()[i],sep=""))
  file.create(paste(x,"_notes.txt",sep=""))
  setwd(paste("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/papers_for_coding",sep=""))
}

