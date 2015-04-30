

setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB

############################################################################
### read .csv file directly downloaded from google docs without any changes
data <- read.csv("LUBDES coding table v2 - 1. Coding Table version 2.csv",skip=2,na.strings=c("NA",""))
#names(data)
data <- data[-1,] # remove empty row

setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/papers_for_coding") #MB

## create Study.ID based folders
for (i in 1:(length(unique(data$Study.ID))))  {
  dir.create(paste("",unique(data$Study.ID)[i],sep=""))
}

## create study.ID based not files
setwd(paste("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/papers_for_coding",sep=""))
for (i in 1:(length(list.dirs())))  {
  x<-list.dirs()[i]
  setwd(paste(list.dirs()[i],sep=""))
  file.create(paste(x,"_notes.txt",sep=""))
  setwd(paste("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/papers_for_coding",sep=""))
}

