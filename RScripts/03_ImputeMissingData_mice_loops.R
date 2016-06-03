###########################################################################
## 03.1. impute missing data using mice package
## 
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

### impute richness
temp1 <- complete(mice(data2imp.richness, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =100, printFlag = FALSE), 
                 "long")
temp1$is.imputed <- rep(data4imp.richness$richness.SD.is.imputed,10)

richness.mean.imp <- data.frame(.id=unique(temp1$.id[temp1$is.imputed=="yes"]),mean.imp=rowMeans(matrix(temp1$richness.SD[temp1$is.imputed=="yes"], ncol=nchains, byrow=F)))

# plot distribution of imputed SDs
imp.richness <- ggplot() +
  geom_point(aes(x=.id,y=richness.SD),color="red",alpha=0.5,data=temp1[temp1$is.imputed=="yes",]) +
  geom_point(aes(x=richness.mean.imp$.id,y=richness.mean.imp$mean.imp),size=2) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(imp.richness)
ggsave(imp.richness, file=path2temp %+% "imputed.SD.richness.png",width=15)

### impute yield
temp2 <- complete(mice(data2imp.yield, predictorMatrix=predictorMatrix1,
                       method = "pmm",
                       m=nchains, maxit =100, printFlag = FALSE), 
                  "long")
temp2$is.imputed <- rep(data4imp.yield$yield.SD.is.imputed,10)

yield.mean.imp <- data.frame(.id=unique(temp2$.id[temp2$is.imputed=="yes"]),mean.imp=rowMeans(matrix(temp2$yield.SD[temp2$is.imputed=="yes"], ncol=nchains, byrow=F)))

# plot distribution of imputed SDs
imp.yield <- ggplot() +
  geom_point(aes(x=.id,y=yield.SD),color="red",alpha=0.5,data=temp2[temp2$is.imputed=="yes",]) +
  geom_point(aes(x=yield.mean.imp$.id,y=yield.mean.imp$mean.imp),size=2) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(imp.yield)
ggsave(imp.yield, file=path2temp %+% "imputed.SD.yield.png",width=15)

for(i in 1:nchains){
  data2imp.richness$richness.SD <- temp1$richness.SD[temp1$.imp==i]
  
  dataimp$richness.SD[dataimp$richness.SD.is.imputed=="yes"] <- data2imp.richness$richness.SD[match(dataimp$richnessID[dataimp$richness.SD.is.imputed=="yes"],data2imp.richness$richnessID)]
  
  p.richness <- ggplot(dataimp[dataimp$X..of.samples.for.BD.measure>1,]) + # to supress warnings for NA SDs for cases n=1
    geom_point(aes(x=richness.mean, y=richness.SD, color=richness.SD.is.imputed), size=4, alpha=.5) +
    xlim(range(dataimp$richness.mean[dataimp$richness.SD.is.imputed=="yes"],na.rm=T)) +
    ylim(range(dataimp$richness.SD[dataimp$richness.SD.is.imputed=="yes"],na.rm=T))
  print(p.richness)
  ggsave(p.richness, file=path2temp.list[i] %+% "imputation_mice_richness.png")
  
#   temp.richness <- data.frame(matrix(temp1$richness.SD, ncol=nchains, byrow=F))
#   temp.richness$mean <- rowMeans(temp.richness[,1:nchains])
#   temp.richness$sd <- apply(temp.richness[,1:nchains],1,sd)
  
  data2imp.yield$yield.SD <- temp2$yield.SD[temp2$.imp==i]
  
  dataimp$yield.SD[dataimp$yield.SD.is.imputed=="yes"]<-data2imp.yield$yield.SD[match(dataimp$yieldID[dataimp$yield.SD.is.imputed=="yes"],data2imp.yield$yieldID)]
  
  p.yield <- ggplot(dataimp[dataimp$X..of.samples.for.YD.measure>1,]) + # to supress warnings for NA SDs for cases n=1
    geom_point(aes(x=yield.mean, y=yield.SD, color=yield.SD.is.imputed), size=4, alpha=.5) +
    xlim(range(dataimp$yield.mean[dataimp$yield.SD.is.imputed=="yes"],na.rm=T)) +
    ylim(range(dataimp$yield.SD[dataimp$yield.SD.is.imputed=="yes"],na.rm=T))
  print(p.yield)
  ggsave(p.yield, file=path2temp.list[i] %+% "imputation_mice_yield.png")

save(dataimp,file=path2temp.list[i] %+% "dataimp.Rdata")
}

# ### check variability of imputation
# temp.yield <- data.frame(matrix(temp2$yield.SD, ncol=nchains, byrow=F))
# temp.yield$mean <- rowMeans(temp.yield[,1:nchains])
# temp.yield$sd <- apply(temp.yield[,1:nchains],1,sd)
# 
# par(mfrow=c(2,2))
# hist(temp.richness$mean)
# hist(temp.richness$sd)
# hist(temp.yield$mean)
# hist(temp.yield$sd)
# par(mfrow=c(1,1))


### remove temporary variables
#rm(dataimp,data2imp.richness, data2imp.yield, temp1,temp2, predictorMatrix1)

