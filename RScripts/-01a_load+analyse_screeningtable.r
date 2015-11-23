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
screening.data$Coding_Decision_final[!is.na(screening.data$Coding_Decision..2)] <- screening.data$Coding_Decision..2[!is.na(screening.data$Coding_Decision..2)]

pdf(paste(path2temp,"piecharts.pdf",sep="/"), width=10)

### testing threshold setting for machine learning score, aim: acceptance rate for ML is approximately the same than for manual screening
stats.screen1 <- table(screening.data$Use_for_Study)[c(1,2,5,3,4,6)]
sum(stats.screen1[c("Definitely","Maybe-High")])/sum(stats.screen1[c("Definitely","Maybe-High","Maybe-Low", "No")])
stats.screen1["ML-Yes"]/sum(stats.screen1[c("ML-Yes","ML-No")])
pie(stats.screen1,labels=paste(names(stats.screen1)," (",stats.screen1, ")",sep=""),col=c(brewer.pal(3, "Greens"),brewer.pal(3, "Reds")), main="Results of abstract screening",cex=1.5)

### Progress in screening (#studies screened vs not screened)
screen2 <- subset(screening.data, Use_for_Study %in% c("Definitely","Maybe-High","ML-Yes"))
stats1.screen2 <- table(screen2$Coding_Decision_final,exclude=NULL)
pie(c(sum(stats1.screen2[1:(length(stats1.screen2)-1)]),stats1.screen2[length(stats1.screen2)]), labels=paste(c("Screened","Not yet screened")," (",c(sum(stats1.screen2[1:(length(stats1.screen2)-1)]),stats1.screen2[length(stats1.screen2)]),")",sep=""), col=c("green","red"), main="Progress in full-text-screening",cex=1.5)

temp <- c(stats1.screen2["Proceed and Code!"],sum(stats1.screen2[c("Authors contacted","Contact authors and request data","Need data from figures","Needing clarification")]),sum(stats1.screen2[c("Got access to publication","paper in spanish","Re-evaluate use for study")]),sum(stats1.screen2[c("Code-ability Rejected","Ignored","Rejection confirmed")]),sum(stats1.screen2[c("no access to publication","not found")]))
pie(temp,labels=paste(c("Proceed and Code", "Pending discussion or\n requested data", "Pending screening","Rejected", "No access")," (",temp, ")",sep=""), col=c(brewer.pal(3,"Greens"),"red","white"),main="Preliminary results of full-text screening",cex=1.5)

### Progress in screened studies
stats2.screen2 <- table(screen2$Status)
temp <- data.frame(accepted=sum(stats2.screen2["coding complete"]), ToDo=sum(stats2.screen2)-sum(stats2.screen2[c("coding complete","study removed during coding", "study removed after evaluation")]), rejected=sum(stats2.screen2[c("coding complete","study removed during coding", "study removed after evaluation")]))
pie(as.numeric(temp),labels=paste(names(temp)," (",temp, ")",sep=""),col=c("green","white","red"),main="Progress in studies accepted in full-text screening",cex=1.5)

dev.off()