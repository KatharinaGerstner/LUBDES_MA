############################################################################
### Purpose of this skript module 03 is to:
###
### 03.1. crude-impute based on average SD/mean ratio
### 03.2. impute missing data using mice package
### 03.3. impute missing data using mi package 
###
### General comments:
### * 03.2 is currently not used due to random problems, TO DO: combine chains, include yield unit for imputation
### * 03.3 is currently not working!
###
### Authors: KG, MB, SK ...
############################################################################

dataimp <- data
### impute also zero SD's as we can't work with that in the analysis
dataimp$richness.SD[data$richness.SD==0] <- NA
dataimp$yield.SD[data$yield.SD==0] <- NA


###########################################################################
## 03.1. crude-impute based on average SD/mean ratio
## 
###########################################################################
# 
##########################################
# SK: save which results were imputed
dataimp$Yield.SD.is.imputed = "no"
dataimp$Yield.SD.is.imputed[which(is.na(dataimp$yield.SD))] = "yes"
dataimp$Richness.SD.is.imputed = "no"
dataimp$Richness.SD.is.imputed[which(is.na(dataimp$richness.SD))] = "yes"
# 
############################################################################
### 03.1. crude-impute based on average SD/mean ratio
### 
############################################################################

# dataimp$richness_sd_of_mean<-apply(subset(dataimp, select=c(richness.mean,richness.SD)),1,function(x) (x[2]/x[1]))
# dataimp$yield_sd_of_mean<-apply(subset(dataimp, select=c(yield.mean,yield.SD)),1,function(x) (x[2]/x[1]))
# 
# dataimp$richness_sd_of_mean[dataimp$richness_sd_of_mean==Inf]<-NA
# dataimp$yield_sd_of_mean[dataimp$yield_sd_of_mean==Inf]<-NA
# 
# mean_richness_sd_of_mean<-mean(dataimp$richness_sd_of_mean,na.rm=TRUE)
# mean_yield_sd_of_mean<-mean(dataimp$yield_sd_of_mean,na.rm=TRUE)
# 
# sd_richness_sd_of_mean<-sd(dataimp$richness_sd_of_mean,na.rm=TRUE)
# sd_yield_sd_of_mean<-sd(dataimp$yield_sd_of_mean,na.rm=TRUE)
# 
# ### choose one of three options
# 
# ### without SD
# # dataimp$richness.SD[is.na(dataimp$richness.SD)]<-(dataimp$richness.mean[is.na(dataimp$richness.SD)]*mean_richness_sd_of_mean)
# # dataimp$yield.SD[is.na(dataimp$yield.SD)]<-(dataimp$yield.mean[is.na(dataimp$yield.SD)]*mean_yield_sd_of_mean)
# 
# ### + 1sd
# dataimp$richness.SD[is.na(dataimp$richness.SD)]<-(dataimp$richness.mean[is.na(dataimp$richness.SD)]*(mean_richness_sd_of_mean+sd_richness_sd_of_mean))
# dataimp$yield.SD[is.na(dataimp$yield.SD)]<-(dataimp$yield.mean[is.na(dataimp$yield.SD)]*(mean_yield_sd_of_mean+sd_yield_sd_of_mean))
# 
# ### -1sd
# #dataimp$richness.SD[is.na(dataimp$richness.SD)]<-(dataimp$richness.mean[is.na(dataimp$richness.SD)]*(mean_richness_sd_of_mean-sd_richness_sd_of_mean))
# #dataimp$yield.SD[is.na(dataimp$yield.SD)]<-(dataimp$yield.mean[is.na(dataimp$yield.SD)]*(mean_yield_sd_of_mean-sd_yield_sd_of_mean))
# 
# dataimp$richness_sd_of_mean<-apply(subset(dataimp, select=c(richness.mean,richness.SD)),1,function(x) (x[2]/x[1]))
# dataimp$yield_sd_of_mean<-apply(subset(dataimp, select=c(yield.mean,yield.SD)),1,function(x) (x[2]/x[1]))
# 
# dataimp$richness_sd_of_mean[dataimp$richness_sd_of_mean==Inf]<-NA
# dataimp$yield_sd_of_mean[dataimp$yield_sd_of_mean==Inf]<-NA
# 
# mean_richness_sd_of_mean<-mean(dataimp$richness_sd_of_mean,na.rm=TRUE)
# mean_yield_sd_of_mean<-mean(dataimp$yield_sd_of_mean,na.rm=TRUE)
# 
# sd_richness_sd_of_mean<-sd(dataimp$richness_sd_of_mean,na.rm=TRUE)
# sd_yield_sd_of_mean<-sd(dataimp$yield_sd_of_mean,na.rm=TRUE)
# 
# ### choose one of three options
# 
# ### without SD
# # dataimp$richness.SD[is.na(dataimp$richness.SD)]<-(dataimp$richness.mean[is.na(dataimp$richness.SD)]*mean_richness_sd_of_mean)
# # dataimp$yield.SD[is.na(dataimp$yield.SD)]<-(dataimp$yield.mean[is.na(dataimp$yield.SD)]*mean_yield_sd_of_mean)
# 
# ### + 1sd
# dataimp$richness.SD[is.na(dataimp$richness.SD)]<-(dataimp$richness.mean[is.na(dataimp$richness.SD)]*(mean_richness_sd_of_mean+sd_richness_sd_of_mean))
# dataimp$yield.SD[is.na(dataimp$yield.SD)]<-(dataimp$yield.mean[is.na(dataimp$yield.SD)]*(mean_yield_sd_of_mean+sd_yield_sd_of_mean))

### -1sd
#dataimp$richness.SD[is.na(dataimp$richness.SD)]<-(dataimp$richness.mean[is.na(dataimp$richness.SD)]*(mean_richness_sd_of_mean-sd_richness_sd_of_mean))
#dataimp$yield.SD[is.na(dataimp$yield.SD)]<-(dataimp$yield.mean[is.na(dataimp$yield.SD)]*(mean_yield_sd_of_mean-sd_yield_sd_of_mean))


############################################################################
### 03.2. impute missing data using mice package
### 
############################################################################

dataimp$richnessID <- paste(dataimp$Study.ID,dataimp$richness.mean,dataimp$X..of.samples.for.BD.measure)
dataimp$yieldID <- paste(dataimp$Study.ID,dataimp$yield.mean,dataimp$X..of.samples.for.YD.measure)

### specify columns necessary for imputation
data2imp.richness <- dataimp[,c("richnessID","richness.mean", "richness.SD", "X..of.samples.for.BD.measure")]
data2imp.yield <- dataimp[dataimp$Intensity.broad!="no LU",c("yieldID","yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] 

### reduce dataframe, remove duplicates in Study.ID_meanRRs
data2imp.richness <- data2imp.richness[!duplicated(data2imp.richness[,"richnessID"]),]
data2imp.yield <- data2imp.yield[!duplicated(data2imp.yield[,"yieldID"]),]

### specify columns used for prediction
### only impute SDs using the corresponding means and sample.size
predictorMatrix1 <- matrix(c(rep(0,4),rep(0,4),c(0,1,0,1),rep(0,4)),
                           ncol=4,byrow=T)

nchains <- 10

### impute
temp <- complete(mice(data2imp.richness, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =20, printFlag = FALSE), 
                 "long")
data2imp.richness$richness.SD <- rowMeans(matrix(temp$richness.SD, ncol=nchains, byrow=F))

temp <- complete(mice(data2imp.yield, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =20, printFlag = FALSE), 
                 "long")
data2imp.yield$yield.SD <- rowMeans(matrix(temp$yield.SD, ncol=nchains, byrow=F))

dataimp$richness.SD[is.na(dataimp$richness.SD)]<-data2imp.richness$richness.SD[match(dataimp$richnessID[is.na(dataimp$richness.SD)],data2imp.richness$richnessID)]
dataimp$yield.SD[is.na(dataimp$yield.SD)]<-data2imp.yield$yield.SD[match(dataimp$yieldID[is.na(dataimp$yield.SD)],data2imp.yield$yieldID)]

rm(data2imp.richness, data2imp.yield, temp, predictorMatrix1, nchains)
# ############################################################################
### 03.3. impute missing data using mi package
###
### Currently not working!
############################################################################

# ### apply imputation methods, impute BD and yield sd and then calculate se
# ### specify data frame for imputation
# ### Adding "latitude..N..S.", "longitude..E..W.", "Land.cover", "PES.category" cause error
# ### TO DO: standardize sampled.size.unit for BD and yield, so that sampled.area has the same unit overall, then sampled.size.unit becomes obsolete 
# #data2impute <- data[,c("study.type", "Country", "Land.use...land.cover", "Intensity.broad", "Fertilization", "Irrigation", "Pesticides", "Grazing", "Mowing", "Clear.Cut.y.n.", "Selective.Logging.y.n.", "Partial.Logging.y.n.", "species.group", "trophic.level..species.guild", "richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "sampled.area", "sampled.size.unit", "product", "yield.unit", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure", "sampled.size.area", "sampled.size.unit.1")]
# data2impute <- data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]
# 
# ### convert the data.frame to a missing_data.frame, which is an enhanced version of a data.frame that includes metadata about the variables that is essential in a missing data context
# mi.df <- missing_data.frame(data2impute) 
# 
# ### check whether the missing_data.frame constructor function initially guessed the appropriate class for each missing_variable, if not use change() 
# show(mi.df)
# mi.df <- change_type(mi.df, c("X..of.samples.for.BD.measure","X..of.samples.for.YD.measure"), to="count")
# # mi.df <- change_link(mi.df, c("richness.SD","yield.SD"), to="log") # to ensure SD is estimated as positive number, however this does not work and cause errror when calling the mi-function: Error in checkForRemoteErrors(val) : 2 nodes produced errors; first error: cannot find valid starting values: please specify some
# 
# ### get a sense of the raw data and their missingness patterns
# image(mi.df) 
# 
# ### use the mi function to do the actual imputation, specify how many independent chains to utilize, how many iterations to conduct, and the maximum amount of time the user is willing to wait for all the iterations of all the chains to finish
# imputations <- mi(mi.df, n.iter = 20, n.chains = 4, max.minutes = 20) 
# 
# ### check convergence by calculating the mean over imputation chains
# (data.imp <- round(mipply(imputations, mean, to.matrix = TRUE), 3))
# 
# ###  Complete the missing data frame, take results fo the mth imputation chain
# data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] <- complete(imputations, m=1)[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]
# summary(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")])
# 
# ### calculate SE for richness and yield mean
# data$richness.SE <- data$richness.SD/sqrt(data$X..of.samples.for.BD.measure)
# data$yield.SE <- data$yield.SD/sqrt(data$X..of.samples.for.YD.measure)
