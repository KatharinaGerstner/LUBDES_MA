############################################################################
### Purpose of this skript module -01a is to:
###
### -01.1. load screening data directly from google docs
### -01.2. plot pie charts about statistics
###
### Authors: MB, KG, ...
############################################################################

############################################################################
### -01.1. load data directly from google docs
############################################################################

### authorize with google docs, the first time or in a new session:
### follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
gs_ls() 
gs_ls() #once authorized, this will list the files you have in GS

### in case there are authorization problems reset the authorization token
#gs_auth(new_user = TRUE)

### load LUBDES  screening table
LUBDES_screening<- gs_title("LUBDES meta analysis master worksheet_screening") #this crashes sometimes but seems to work as of April 22 2015
screening.data <- gs_read(LUBDES_screening, ws = "1. Master Screening Table, all 9910 records") #consume data from sheet 1
screening.data <- as.data.frame(screening.data) #some functions don't like the tbl.df data type

### check variable types 
str(screening.data)

############################################################################
### -01.2. plot pie charts
############################################################################
### remove duplicates
duplicates <- screening.data[duplicated(screening.data$TITLE),] ## TO DO: Check duplicates, as some duplicates are so far not recognized and screened twice
screening.data <- screening.data[!duplicated(screening.data$TITLE),]

screening.data$Coding_Decision_final <- screening.data$Coding_Decision..1
screening.data$Coding_Decision_final[!is.na(screening.data$Coding_Decision..2)] <- screening.data$Coding_Decision..2[!is.na(screening.data$Coding_Decision..2)]

pdf(paste(path2temp,"piecharts.pdf",sep="/"), width=10)

### testing threshold setting for machine learning score, aim: acceptance rate for ML is approximately the same than for manual screening
stats.screen1 <- table(screening.data$Use_for_Study)[c(1,2,5,3,4,6)]
sum(stats.screen1[c("Definitely","Maybe-High")])/sum(stats.screen1[c("Definitely","Maybe-High","Maybe-Low", "No")])
stats.screen1["ML-Yes"]/sum(stats.screen1[c("ML-Yes","ML-No")])
pie(stats.screen1,labels=paste(names(stats.screen1)," (",stats.screen1, ")",sep=""),col=c(brewer.pal(3, "Greens"),brewer.pal(3, "Reds")), main="Results of abstract screening",cex=2)

### Progress in screening (#studies screened vs not screened)
screen2 <- subset(screening.data, Use_for_Study %in% c("Definitely","Maybe-High","ML-Yes"))
stats1.screen2 <- table(screen2$Coding_Decision_final,exclude=NULL)
pie(c(sum(stats1.screen2[1:(length(stats1.screen2)-1)]),stats1.screen2[length(stats1.screen2)]), labels=paste(c("Screened","Not yet screened")," (",c(sum(stats1.screen2[1:(length(stats1.screen2)-1)]),stats1.screen2[length(stats1.screen2)]),")",sep=""), col=c("green","red"), main="Progress in full-text-screening",cex=2)

temp <- c(stats1.screen2["Proceed and Code!"],sum(stats1.screen2[c("Authors contacted","Contact authors and request data","Need data from figures","Needing clarification")]),sum(stats1.screen2[c("Got access to publication","paper in spanish","Re-evaluate use for study")]),sum(stats1.screen2[c("Code-ability Rejected","Ignored","Rejection confirmed")]),sum(stats1.screen2[c("no access to publication","not found")]))
pie(temp,labels=paste(c("Proceed and Code", "Pending discussion or\n requested data", "Pending screening","Rejected", "No access")," (",temp, ")",sep=""), col=c(brewer.pal(3,"Greens"),"red","white"),main="Preliminary results of full-text screening",cex=2)

### Progress in screened studies
stats2.screen2 <- table(screen2$Status)
temp <- data.frame(accepted=sum(stats2.screen2["coding complete"]), ToDo=sum(stats2.screen2)-sum(stats2.screen2[c("coding complete","study removed during coding", "study removed after evaluation")]), rejected=sum(stats2.screen2[c("coding complete","study removed during coding", "study removed after evaluation")]))
pie(as.numeric(temp),labels=paste(names(temp)," (",temp, ")",sep=""),col=c("green","white","red"),main="Progress in studies accepted in full-text screening",cex=2)

rm(duplicates,stats.screen1,screen2,stats1.screen2,temp,stats2.screen2)
dev.off()


### Venn Diagram

options(java.parameters = "-Xmx8000m") # I needed to increase memory rJava is allowed to use

# setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Bibliometric Analysis")
# setwd("C:/Users/winter/Dropbox/sDiv_workshop (1)/Papers/Conceptual Paper/Figures")
# venn<-read.csv("venn_concept.csv", header=T, sep=";")

# excel.file <- file.path("LUBDES meta analysis master worksheet_screening.xlsx")
# screening.data <- readWorksheetFromFile(excel.file, sheet=1)

subset.lu <- subset(screening.data, LU ==1, select = c(UNIQUE_WOS_ID, LU))
subset.lu$LU = rep("LU", length(subset.lu$LU))
print(length(subset.lu$LU))

subset.es <- subset(screening.data, ES == 1, select = c(UNIQUE_WOS_ID, LU))               
subset.es$LU = rep("ES", length(subset.es$LU))
print(length(subset.es$LU))

subset.bd <- subset(screening.data, BD==1, select = c(UNIQUE_WOS_ID, LU))               
subset.bd$LU = rep("BD", length(subset.bd$LU))
print(length(subset.bd$LU))

subset.lu.bd <- subset(screening.data, BD == 1 & LU ==1, select = c(UNIQUE_WOS_ID, LU))
subset.lu.bd$LU = rep("LU-BD", length(subset.lu.bd$LU))
print(length(subset.lu.bd$LU))

subset.lu.es <- subset(screening.data, ES == 1 & LU ==1, select = c(UNIQUE_WOS_ID, LU))               
subset.lu.es$LU = rep("LU-ES", length(subset.lu.es$LU))
print(length(subset.lu.es$LU))

subset.bd.es <- subset(screening.data, ES == 1 & BD ==1, select = c(UNIQUE_WOS_ID, LU))               
subset.bd.es$LU = rep("BD-ES", length(subset.bd.es$LU))
print(length(subset.bd.es$LU))

subset.lu.bd.es <- subset(screening.data, ES == 1 & LU ==1 &BD==1, select = c(UNIQUE_WOS_ID, LU))               
subset.lu.bd.es$LU = rep("LU-BD-ES", length(subset.lu.bd.es$LU))
print(length(subset.lu.es$LU))

# Test und nun noch mal schauen, was sonst noch so mit LU zu tun hat, Gurndgesamtheit
# Netter Versuch, nur geht das mit venneuler nicht der kann nur 2 Mengen und deren überlapp.

# subset.all <- subset(screening.data, select = c(UNIQUE_WOS_ID, LU))               
# subset.all$LU = rep("All", length(subset.all$LU))
# print(length(subset.all$LU))

venn.studies <- merge(subset.lu,subset.es, all=T)
venn.studies <- merge(venn.studies,subset.bd, all=T)

#str(venn.studies)
# venn.all <- merge(venn.studies, subset.all, all=T)

# Generate Datstruture for plotting
venn.studies<-venneuler(venn.studies)

# Delete Labels
venn.studies$labels<-rep("", length(venn.studies$labels)) 

# Manually attach labels and add size number of studies.
# use center coordinates and diameter to locate text-boxes
# surprisingly, venneuler seems to mix up diameters of categories, anyhow.


png(file = "venn_screening_paper_MB.png", bg = "transparent",width=1000,height=600)
par(mar=c(0,0,0,0), cex=3)

plot(venn.studies)
text(venn.studies$centers[1], 0.75, paste("BD\nn=", length(subset.bd$LU)),        cex = 0.8)
text(venn.studies$centers[2]+0.1, 0.5, paste("Y\nn=", length(subset.es$LU)),        cex = 0.8)
text(venn.studies$centers[3], 0.25, paste("LU\nn=", length(subset.lu$LU)),        cex = 0.8)

text(venn.studies$centers[2]-0.07, 0.5, paste("LU-BD-Y\nn=", length(subset.lu.bd.es$LU)),        cex = 0.8)
text(venn.studies$centers[3]-0.06, 0.45, paste("LU-BD\nn=", length(subset.lu.bd$LU)),        cex = 0.8)
text(venn.studies$centers[2], 0.4, paste("BD-Y\nn=", length(subset.bd.es$LU)),        cex = 0.8)
text(venn.studies$centers[2], 0.6, paste("LU-Y\nn=", length(subset.lu.es$LU)),        cex = 0.8)

dev.off()
