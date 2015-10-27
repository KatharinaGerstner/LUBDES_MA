############################################################################
### Purpose of this skript module -01a is to:
###
### -01a.1. load screening data directly from google docs
### -01a.2. plot pie charts about statistics
###
### Authors: MB, KG, ...
############################################################################

############################################################################
### -01a.1. load data directly from google docs
############################################################################

### authorize with google docs, the first time or in a new session:
### follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
gs_ls() 
gs_ls() #once authorized, this will list the files you have in GS

### in case there are authorization problems reset the authorization token
#gs_auth(new_user = TRUE)

### load LUBDES  coding table
LUBDES_screening<- gs_title("LUBDES meta analysis master worksheet_screening") #this crashes sometimes but seems to work as of April 22 2015
screening.data <- gs_read(LUBDES_screening, ws = "1. Master Screening Table, all 9910 records") #consume data from sheet 1
screening.data <- as.data.frame(screening.data) #some functions don't like the tbl.df data type

### check variable types 
str(screening.data)

############################################################################
### -01a.2. plot pie charts
############################################################################
### remove duplicates
duplicates <- screening.data[duplicated(screening.data$TITLE),] ## TO DO: Check duplicates, as some duplicates are so far not recognized and screened twice
screening.data <- screening.data[!duplicated(screening.data$TITLE),]

screening.data$Coding_Decision_final <- screening.data$Coding_Decision..1
screening.data$Coding_Decision_final[which(screening.data$Coding_Decision..1=="Re-evaluate use for study")] <- screening.data$Coding_Decision..2[which(screening.data$Coding_Decision..1=="Re-evaluate use for study")]
stats.todo <- 
stats <- table(screening.data$Coding_Decision_final, exclude=NULL) # not found==no access to publication?

png(file=paste(getwd(),"/piechart_screen_total.png",sep=""))
pie(c(sum(stats[1:9]),stats[10]), labels=paste(c("Screened","Not yet screened")," (",c(sum(stats[1:9]),stats[10]),")",sep=""), col=rainbow(2))
dev.off()
png(file=paste(getwd(),"/piechart_screen_subset.png",sep=""))
pie(stats[1:9], labels=paste(names(stats)[1:9]," (",stats[1:9],")",sep=""), col=rainbow(9))
dev.off()