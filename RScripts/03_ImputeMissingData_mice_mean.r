###########################################################################
### 03.1. impute missing data using the mice package and the mean of 50 imputation chains
### 03.2. plot imputed missing data
### 
### Authors: KG, MB...
###########################################################################

###########################################################################
### 03.1. impute missing data using the mice package and the mean of 50 imputation chains
###########################################################################

### prepare dataframe
dataimp <- data

# save which results were imputed
dataimp$yield.SD.is.imputed <- ifelse(is.na(dataimp$yield.SD),"yes","no")
dataimp$richness.SD.is.imputed <- ifelse(is.na(dataimp$richness.SD),"yes","no")

# ensure that same mean and n combinations are imputed identically
dataimp$richnessID <- paste(dataimp$Study.ID,dataimp$richness.mean,dataimp$X..of.samples.for.BD.measure)
dataimp$yieldID <- paste(dataimp$Study.ID,dataimp$yield.mean,dataimp$X..of.samples.for.YD.measure)

### reduce dataframe, remove duplicates in Study.ID_meanRRs and restrict imputation to non-zero mean and SD cases
data4imp.richness <- subset(dataimp,!duplicated(dataimp[,"richnessID"]) & dataimp$X..of.samples.for.BD.measure > 1)
data4imp.yield <- subset(dataimp,!duplicated(dataimp[,"yieldID"]) & dataimp$X..of.samples.for.YD.measure > 1)

## specify columns necessary for imputation
data2imp.richness <- data4imp.richness[,c("richnessID","richness.mean", "richness.SD", "X..of.samples.for.BD.measure")]
data2imp.yield <- data4imp.yield[c("yieldID","yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] 

### specify columns used for prediction
## only impute SDs using the corresponding means and sample.size
predictorMatrix1 <- matrix(c(rep(0,4),rep(0,4),c(0,1,0,1),rep(0,4)),
                            ncol=4,byrow=T)

nchains <- 50

### impute richness
temp <- complete(mice(data2imp.richness, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =20, printFlag = FALSE), 
                 "long")
data2imp.richness$richness.SD <- rowMeans(matrix(temp$richness.SD, ncol=nchains, byrow=F))

dataimp$richness.SD[is.na(dataimp$richness.SD)]<-data2imp.richness$richness.SD[match(dataimp$richnessID[is.na(dataimp$richness.SD)],data2imp.richness$richnessID)]

temp$is.imputed <- rep(data4imp.richness$richness.SD.is.imputed,10)

richness.mean.imp <- data.frame(.id=unique(temp$.id[temp$is.imputed=="yes"]),mean.imp=rowMeans(matrix(temp$richness.SD[temp$is.imputed=="yes"], ncol=nchains, byrow=F)))

temp.richness <- data.frame(matrix(temp$richness.SD, ncol=nchains, byrow=F))
temp.richness$mean <- rowMeans(temp.richness[,1:nchains])
temp.richness$sd <- apply(temp.richness[,1:nchains],1,sd)

save(temp, temp.richness, file = path2temp %+% "imputed.SD.richness.Rdata")

### impute yield
temp <- complete(mice(data2imp.yield, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =20, printFlag = FALSE), 
                 "long")
data2imp.yield$yield.SD <- rowMeans(matrix(temp$yield.SD, ncol=nchains, byrow=F))

dataimp$yield.SD[is.na(dataimp$yield.SD)]<-data2imp.yield$yield.SD[match(dataimp$yieldID[is.na(dataimp$yield.SD)],data2imp.yield$yieldID)]

temp$is.imputed <- rep(data4imp.yield$yield.SD.is.imputed,10)

yield.mean.imp <- data.frame(.id=unique(temp$.id[temp$is.imputed=="yes"]),mean.imp=rowMeans(matrix(temp$yield.SD[temp$is.imputed=="yes"], ncol=nchains, byrow=F)))

### check variability of imputation
temp.yield <- data.frame(matrix(temp$yield.SD, ncol=nchains, byrow=F))
temp.yield$mean <- rowMeans(temp.yield[,1:nchains])
temp.yield$sd <- apply(temp.yield[,1:nchains],1,sd)

save(temp, temp.yield, file = path2temp %+% "imputed.SD.yield.Rdata")

save(dataimp,file=path2temp %+% "dataimp.Rdata")

###########################################################################
### 03.2. plot imputed missing data
###########################################################################
## richness
load(path2temp %+% "imputed.SD.richness.Rdata") # temp, temp.richness
nchains <- 50
richness.mean.imp <- data.frame(.id=unique(temp$.id[temp$is.imputed=="yes"]),mean.imp=rowMeans(matrix(temp$richness.SD[temp$is.imputed=="yes"], ncol=nchains, byrow=F)))
imp.richness <- ggplot() +
  geom_point(aes(x=.id,y=richness.SD),color="red",alpha=0.3,data=temp[temp$is.imputed=="yes",]) +
  geom_point(aes(x=richness.mean.imp$.id,y=richness.mean.imp$mean.imp),size=2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("SD(species richness)") + scale_x_discrete("",labels=c()) +
  theme_lubdes()

## yield
load(path2temp %+% "imputed.SD.yield.Rdata") # temp, temp.yield
nchains <- 50
yield.mean.imp <- data.frame(.id=unique(temp$.id[temp$is.imputed=="yes"]),mean.imp=rowMeans(matrix(temp$yield.SD[temp$is.imputed=="yes"], ncol=nchains, byrow=F)))
imp.yield <- ggplot() +
  geom_point(aes(x=.id,y=yield.SD),color="red",alpha=0.3,data=temp[temp$is.imputed=="yes",]) +
  geom_point(aes(x=yield.mean.imp$.id,y=yield.mean.imp$mean.imp),size=2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("SD(yield)") + scale_x_discrete("",labels=c()) + 
  theme_lubdes()

png(file = path2temp %+% "imputed.SD.png", width = 800, height = 500)
grid.arrange(imp.richness,imp.yield,nrow=2,heights=c(1,1))
dev.off()
