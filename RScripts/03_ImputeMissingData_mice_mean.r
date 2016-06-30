###########################################################################
### 03.1. impute missing data using the mice package and the mean of 10 imputation chains
### 
### Authors: KG, MB...
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

nchains <- 10

### impute richness
temp <- complete(mice(data2imp.richness, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =20, printFlag = FALSE), 
                 "long")
data2imp.richness$richness.SD <- rowMeans(matrix(temp$richness.SD, ncol=nchains, byrow=F))

dataimp$richness.SD[is.na(dataimp$richness.SD)]<-data2imp.richness$richness.SD[match(dataimp$richnessID[is.na(dataimp$richness.SD)],data2imp.richness$richnessID)]

p.richness <- ggplot(dataimp) +
  geom_point(aes(x=richness.mean, y=richness.SD, color=richness.SD.is.imputed), size=2, alpha=.5) +
  xlim(range(dataimp$richness.mean[dataimp$richness.SD.is.imputed=="yes"],na.rm=T)) +
  ylim(range(dataimp$richness.SD[dataimp$richness.SD.is.imputed=="yes"],na.rm=T)) +
  scale_color_discrete(limits=c("no","yes"),labels=c("SD observed","SD imputed")) +
  guides(color=guide_legend(title=NULL)) +
  theme_lubdes(rel.text.size=0.8,legend.position="bottom")
ggsave(p.richness, file=path2temp %+% "imputation_mice_richness.png",height=4,width=3.5)

temp.richness <- data.frame(matrix(temp$richness.SD, ncol=nchains, byrow=F))
temp.richness$mean <- rowMeans(temp.richness[,1:nchains])
temp.richness$sd <- apply(temp.richness[,1:nchains],1,sd)

### impute yield
temp <- complete(mice(data2imp.yield, predictorMatrix=predictorMatrix1,
                      method = "pmm",
                      m=nchains, maxit =20, printFlag = FALSE), 
                 "long")
data2imp.yield$yield.SD <- rowMeans(matrix(temp$yield.SD, ncol=nchains, byrow=F))

dataimp$yield.SD[is.na(dataimp$yield.SD)]<-data2imp.yield$yield.SD[match(dataimp$yieldID[is.na(dataimp$yield.SD)],data2imp.yield$yieldID)]

p.yield <- ggplot(dataimp) +
  geom_point(aes(x=yield.mean, y=yield.SD, color=yield.SD.is.imputed), size=2, alpha=.5) +
  xlim(range(dataimp$yield.mean[dataimp$yield.SD.is.imputed=="yes"],na.rm=T)) +
  ylim(range(dataimp$yield.SD[dataimp$yield.SD.is.imputed=="yes"],na.rm=T)) +
  scale_color_discrete(limits=c("no","yes"),labels=c("SD observed","SD imputed")) +
  guides(color=guide_legend(title=NULL)) +
  theme_lubdes(legend.position="bottom",rel.text.size=0.8)
ggsave(p.yield, file=path2temp %+% "imputation_mice_yield.png",height=4,width=3.5)

### check variability of imputation
# temp.yield <- data.frame(matrix(temp$yield.SD, ncol=nchains, byrow=F))
# temp.yield$mean <- rowMeans(temp.yield[,1:nchains])
# temp.yield$sd <- apply(temp.yield[,1:nchains],1,sd)
# 
# par(mfrow=c(2,2))
# hist(temp.richness$mean)
# hist(temp.richness$sd)
# hist(temp.yield$mean)
# hist(temp.yield$sd)
# par(mfrow=c(1,1))

save(dataimp,file=path2temp %+% "dataimp.Rdata")

### remove temporary variables
rm(data2imp.richness, data2imp.yield, temp, temp.yield, temp.richness, predictorMatrix1, nchains)

