############################################################################
### Purpose of this skript module 03 is to:
###
### 03.1. impute missing sd data using bayesian linear regression with means and number of samples following Stevens (2011) Pharmaceutical Statistics
### 
### General comments:
### 
### Authors: KG, MB, SK ...
############################################################################

dataimp <- data

# save which results were imputed
dataimp$yield.SD.is.imputed <- ifelse(is.na(dataimp$yield.SD),"yes","no")
dataimp$richness.SD.is.imputed <- ifelse(is.na(dataimp$richness.SD),"yes","no")

# ensure that same mean and n combinations are imputed identically
dataimp$richnessID <- paste(dataimp$Study.ID,dataimp$richness.mean,dataimp$X..of.samples.for.BD.measure)
dataimp$yieldID <- paste(dataimp$Study.ID,dataimp$yield.mean,dataimp$X..of.samples.for.YD.measure)

### reduce dataframe, remove duplicates in Study.ID_meanRRs and restrict imputation to non-zero mean and SD cases
data4imp.richness <- subset(dataimp,!duplicated(dataimp[,"richnessID"]) & dataimp$richness.SD>0 & dataimp$X..of.samples.for.BD.measure > 1)
data4imp.yield <- subset(dataimp,!duplicated(dataimp[,"yieldID"]) & dataimp$yield.SD>0 & dataimp$X..of.samples.for.YD.measure > 1)

#data4imp.richness[,c("richness.mean","richness.SD")] <- scale(data4imp.richness[,c("richness.mean","richness.SD")])
#data4imp.yield[,c("yield.mean","yield.SD")] <- scale(data4imp.yield[,c("yield.mean","yield.SD")])

############################################################################
### the bayes model
############################################################################

cat("model{ # cf. Stevens (2011) Pharmaceutical Statistics
  for(i in 1:N.obs){
  # Normal likelihood for sample means
    mean[i] ~ dnorm(mu[i], tau[i])
    tau[i] <- n[i]/(sigma*sigma)

  # Gamma likelihood for sample variances
    sd2[i] ~ dgamma(a[i],b[i])
    a[i] <- (n[i]-1)/2
    b[i] <- (n[i]-1)/(2*sigma*sigma)
  }

  # Weak prior for population standard deviation
  log(sigma) <- logsigma
  logsigma ~ dunif(-10,10)
    
  # prior for fixed effects
  for (i in 1:N.obs){
    mu[i] ~ dnorm(0, 0.001)
  }

  # derived quantities
  for (i in 1:N.obs){
    dev[i] <- (mean[i] - mu[i])*(mean[i] - mu[i])*tau[i]
  }

  resdev <- sum(dev[])
    
}",file=path2temp %+% "imputation.model.Stevens.txt")

############################################################################
### imputation for richness SD
############################################################################
jags.data <- list(N.obs = nrow(data4imp.richness), mean = data4imp.richness$richness.mean, sd2=(data4imp.richness$richness.SD)^2,n=data4imp.richness$X..of.samples.for.BD.measure)

jm <- jags.model(path2temp %+% "imputation.model.Stevens.txt", data = jags.data, n.chains = 3, n.adapt = 1000)
update(jm, n.iter = 1000) # throw away the initial samples (the so-called “burn-in” phase)
jm.sample <- jags.samples(jm, variable.names = c("a", "b", "mu","sigma","tau","dev", "resdev"), n.iter = 2000, thin = 2)
pdf(path2temp %+% "TracePlot_Imputation_Richness.pdf")
sapply(c("a", "b", "mu","tau"),function(x) plot(as.mcmc.list(jm.sample[[x]]),main=paste(x))) # check convergence
dev.off()

### plot predictions
tau.summary <- summary(as.mcmc.list(jm.sample$tau))$statistics
tau.mean <- tau.summary[,"Mean"]
sd.mean <- 1/sqrt(tau.mean)
plot.range <- range(sd.mean,sqrt(jags.data$sd2))
png(path2temp %+% "MissingDataImputation_Richness.png")
plot(sd.mean~sqrt(jags.data$sd2), cex = 1, col = "lightgrey", pch = 1,lwd = 2, xlab = "original SD", ylab="predicted SD",xlim=plot.range,ylim=plot.range,main="Richness")
abline(0,1)
dev.off()

### impute 
data2imp.richness <- dataimp[which(dataimp$richness.SD.is.imputed=="yes"),]
nm1 <- (data2imp.richness$X..of.samples.for.BD.measure-1)
data2imp.richness$richness.SD <- sqrt(dgamma(nm1/2,nm1/(2*mean(jm.sample$sigma)^2)))
dataimp$richness.SD[which(dataimp$richness.SD.is.imputed=="yes")] <- data2imp.richness$richness.SD[match(dataimp$richnessID[which(dataimp$richness.SD.is.imputed=="yes")],data2imp.richness$richnessID)]

ggplot(data=dataimp)+
  geom_point(aes(x=richness.mean, y=richness.SD, color=factor(richness.SD.is.imputed)), size=4)

############################################################################
### imputation for yield SD
############################################################################
jags.data <- list(N.obs = nrow(data4imp.yield), mean = data4imp.yield$yield.mean, sd2=(data4imp.yield$yield.SD)^2,n=data4imp.yield$X..of.samples.for.YD.measure)

jm <- jags.model(path2temp %+% "imputation.model.Stevens.txt", data = jags.data, n.chains = 3, n.adapt = 1000)
update(jm, n.iter = 1000) # throw away the initial samples (the so-called “burn-in” phase)
jm.sample <- jags.samples(jm, variable.names = c("a", "b", "mu","sigma","tau","dev", "resdev"), n.iter = 2000, thin = 2)
pdf(path2temp %+% "TracePlot_Imputation_Yield.pdf")
sapply(c("a", "b", "mu","tau"),function(x) plot(as.mcmc.list(jm.sample[[x]]),main=paste(x))) # check convergence
dev.off()

### plot predictions
tau.summary <- summary(as.mcmc.list(jm.sample$tau))$statistics
tau.mean <- tau.summary[,"Mean"]
sd.mean <- 1/sqrt(tau.mean)
plot.range <- range(sd.mean,sqrt(jags.data$sd2))
png(path2temp %+% "MissingDataImputation_Yield.png")
plot(sd.mean~sqrt(jags.data$sd2), cex = 1, col = "lightgrey", pch = 1,lwd = 2, xlab = "original SD", ylab="predicted SD",xlim=plot.range,ylim=plot.range,main="Yield")
abline(0,1)
dev.off()

### impute 
data2imp.yield <- dataimp[which(dataimp$yield.SD.is.imputed=="yes"),]
nm1 <- (data2imp.yield$X..of.samples.for.YD.measure-1)
data2imp.yield$yield.SD <- sqrt(dgamma(nm1/2,nm1/(2*mean(jm.sample$sigma)^2)))
dataimp$yield.SD[which(dataimp$yield.SD.is.imputed=="yes")] <- data2imp.yield$yield.SD[match(dataimp$yieldID[which(dataimp$yield.SD.is.imputed=="yes")],data2imp.yield$yieldID)]

ggplot(data=dataimp)+
  geom_point(aes(x=yield.mean, y=yield.SD, color=factor(yield.SD.is.imputed)), size=4)

# # ###########
# cat("
#       model
#       {
#         # priors
#         beta0 ~ dnorm(0,0.001)
#         beta1 ~ dnorm(0,0.001)
#         beta2 ~ dnorm(0,0.001)
#         sigma ~ dunif(0,100)
#         tau <- pow(sigma,-2)
#         # likelihood
#         for(i in 1:N.obs)
#         {
#         log.sd[i] ~ dnorm(mu[i],tau)
#         mu[i] <- beta0 + beta1*mean[i] + beta2*n.sample[i]
#         # this part is here in order to make nice prediction curves:
#         prediction[i] ~ dnorm(mu[i],tau)
#         }
#         
#         # derived quantities
#         for (i in 1:N.obs){
#           dev[i] <- (log.sd[i] - mu[i])*(log.sd[i] - mu[i])*tau
#         }
#       
#         resdev <- sum(dev[])
#       }
#   ", file=path2temp %+% "imputation.model.txt")

############################################################################
### imputation for richness SD
############################################################################
# jags.data <- list(N.obs = nrow(data4imp.richness), mean = data4imp.richness$richness.mean, log.sd=log(data4imp.richness$richness.SD),n.sample=data4imp.richness$X..of.samples.for.BD.measure)
# 
# jm <- jags.model(path2temp %+% "imputation.model.txt", data = jags.data, n.chains = 3, n.adapt = 1000)
# params <- c("beta0", "beta1", "beta2","mu","tau", "prediction","resdev")
# update(jm, n.iter = 1000) # throw away the initial samples (the so-called “burn-in” phase)
# jm.sample <- jags.samples(jm, variable.names = params, n.iter = 2000, thin = 2)
# sapply(c("beta0", "beta1", "beta2"),function(x) plot(as.mcmc.list(jm.sample[[x]]),main=paste(x))) # check convergence
# 
# ### plot predictions
# predictions <- summary(as.mcmc.list(jm.sample$prediction))
# prds <- data.frame(mean = data4imp.richness$richness.mean, predictions$statistics)
# prds <- prds[order(prds[, 1]), ]
# plot(data4imp.richness$richness.SD, exp(prds$Mean), cex = 1, col = "lightgrey", pch = 1,lwd = 2, xlab = "original SD", ylab="predicted SD",xlim=range(c(range(data4imp.richness$richness.SD),range(exp(prds$Mean)))),ylim=range(c(range(data4imp.richness$richness.SD),range(exp(prds$Mean)))),main="Richness")
# abline(0,1)
# # points(prds[, 1], prds[, 4], pch=1, lwd = 2, col = "red")
# # legend("bottomright",legend=c("original Data", "Imputed Data"),col=c("lightgrey","red"),pch=1,lwd=2,title="Richness")
# 
# ### impute 
# data2imp.richness <- dataimp[which(dataimp$richness.SD.is.imputed=="yes"),]
# data2imp.richness$richness.SD <- exp(mean(jm.sample[["beta0"]]) + mean(jm.sample[["beta1"]])*scale(data2imp.richness$richness.mean) + mean(jm.sample[["beta2"]])*scale(data2imp.richness$X..of.samples.for.BD.measure))
# dataimp$richness.SD[which(dataimp$richness.SD.is.imputed=="yes")] <- data2imp.richness$richness.SD[match(dataimp$richnessID[which(dataimp$richness.SD.is.imputed=="yes")],data2imp.richness$richnessID)]
# 
# # p.richness <- ggplot(dataimp) +
# #   geom_point(aes(x=richness.mean, y=log(richness.SD), color=richness.SD.is.imputed, size=4, alpha=.5)) 
# # print(p.richness)
# 
# ############################################################################
# ### imputation for yield SD
# ############################################################################
# jags.data <- list(N.obs = nrow(data4imp.yield), mean = data4imp.yield$yield.mean, log.sd=log(data4imp.yield$yield.SD),n.sample=data4imp.yield$X..of.samples.for.YD.measure)
# 
# jm <- jags.model(path2temp %+% "imputation.model.txt", data = jags.data, n.chains = 3, n.adapt = 1000)
# params <- c("beta0", "beta1", "beta2","mu","tau", "prediction")
# update(jm, n.iter = 1000) # throw away the initial samples (the so-called “burn-in” phase)
# jm.sample <- jags.samples(jm, variable.names = params, n.iter = 2000, thin = 2)
# sapply(c("beta0", "beta1", "beta2"),function(x) plot(as.mcmc.list(jm.sample[[x]]),main=paste(x))) # check convergence
# 
# ### plot predictions
# predictions <- summary(as.mcmc.list(jm.sample$prediction))
# prds <- data.frame(mean = data4imp.yield$yield.mean, predictions$statistics)
# prds <- prds[order(prds[, 1]), ]
# plot(data4imp.yield$yield.SD, exp(prds$Mean), cex = 1, col = "lightgrey", pch = 1,lwd = 2, xlab = "original SD", ylab="predicted SD",xlim=range(c(range(data4imp.yield$yield.SD),range(exp(prds$Mean)))),ylim=range(c(range(data4imp.yield$yield.SD),range(exp(prds$Mean)))),main="Yield")
# abline(0,1)
# # plot(log(yield.SD) ~ yield.mean, cex = 1, col = "lightgrey", pch = 1,lwd=2, ylab = "log(SD)", xlab = "Scaled Mean",data=data4imp.yield)
# # points(prds[, 1], prds[, 2], pch=1, lwd = 2, col = "red")
# # legend("bottomright",legend=c("original Data", "Imputed Data"),col=c("lightgrey","red"),pch=1,lwd=2,title="Yield")
# 
# # ### impute 
# # data2imp.yield <- dataimp[which(dataimp$yield.SD.is.imputed=="yes"),]
# # data2imp.yield$yield.SD <- exp(mean(jm.sample[["beta0"]]) + mean(jm.sample[["beta1"]])*scale(data2imp.yield$yield.mean) + mean(jm.sample[["beta2"]])*scale(data2imp.yield$X..of.samples.for.YD.measure))
# # dataimp$yield.SD[which(dataimp$yield.SD.is.imputed=="yes")] <- data2imp.yield$yield.SD[match(dataimp$yieldID[which(dataimp$yield.SD.is.imputed=="yes")],data2imp.yield$yieldID)]
# # 
# # # p.yield <- ggplot(dataimp) +
# # #   geom_point(aes(x=yield.mean, y=log(yield.SD), color=yield.SD.is.imputed, size=4, alpha=.5)) 
# # # print(p.yield)
# 

### Resterampe

# data2imp.yield <- subset(dataimp,!duplicated(dataimp[,"yieldID"]) & dataimp$yield.mean>0)
# data2imp.yield <- data2imp.yield[-which(data2imp.yield$yield.SD==0),]
# # dataimp$richness.mean.n <- dataimp$richness.mean*dataimp$X..of.samples.for.BD.measure
# # dataimp$yield.mean.n <- dataimp$yield.mean*dataimp$X..of.samples.for.YD.measure
# 
# imp.richness.glm1 <- glm(richness.SD ~ richness.mean*X..of.samples.for.BD.measure+I(richness.mean)^2+I(X..of.samples.for.BD.measure)^2,data=data2imp.richness[!is.na(data2imp.richness$richness.SD),],family=gaussian(link="log"))
# imp.richness.glm2 <- stepAIC(imp.richness.glm1, k=2)
# summary(imp.richness.glm2)
# plot(imp.richness.glm2) # diagnostic plots, look ok!
# 
# ### fill NAs in original dataframe
# data2imp.richness$richness.SD[is.na(data2imp.richness$richness.SD)] <- predict(imp.richness.glm2, type="response", newdata=data2imp.richness[is.na(data2imp.richness$richness.SD),])
# dataimp$richness.SD[dataimp$Richness.SD.is.imputed=="yes"] <- data2imp.richness$richness.SD[match(dataimp$richnessID[dataimp$Richness.SD.is.imputed=="yes"],data2imp.richness$richnessID)]
# 
# p.richness <- ggplot(dataimp) +
#   geom_point(aes(x=richness.mean, y=richness.SD, color=Richness.SD.is.imputed, size=4, alpha=.5)) #+
# #   xlim(range(dataimp$richness.mean[dataimp$richness.SD.is.imputed=="yes"],na.rm=T)) +
# #   ylim(range(dataimp$richness.SD[dataimp$richness.SD.is.imputed=="yes"],na.rm=T)) 
# print(p.richness)
# 
# imp.yield.lm <- step(lm(log(yield.SD)~log(yield.mean)*log(X..of.samples.for.YD.measure),data=data2imp.yield[!is.na(data2imp.yield$yield.SD),]))
# # summary(imp.yield.lm)
# # plot(imp.yield.lm) # diagnostic plots, look ok!
# data2imp.yield$yield.SD[is.na(data2imp.yield$yield.SD)] <- exp(predict(imp.yield.lm, newdata=data2imp.yield[is.na(data2imp.yield$yield.SD),]))
# dataimp$yield.SD[is.na(dataimp$yield.SD)] <- data2imp.yield$yield.SD[match(dataimp$yieldID[is.na(dataimp$yield.SD)],data2imp.yield$yieldID)]
# 
# p.yield <- ggplot(dataimp) +
#   geom_point(aes(x=yield.mean, y=yield.SD, color=Yield.SD.is.imputed, size=4, alpha=.5)) #+
# #   xlim(range(dataimp$yield.mean[dataimp$yield.SD.is.imputed=="yes"],na.rm=T)) +
# #   ylim(range(dataimp$yield.SD[dataimp$yield.SD.is.imputed=="yes"],na.rm=T)) 
# print(p.yield)
# 
# rm(data2imp.richness,data2imp.yield, imp.richness.lm, imp.yield.lm, p.richness, p.yield)


############################################################################
### 03.1. impute missing data using mice package
### 
############################################################################
### specify columns necessary for imputation
#data2imp.richness <- dataimp[,c("richnessID","richness.mean", "richness.SD", "X..of.samples.for.BD.measure")]
#data2imp.yield <- dataimp[dataimp$Intensity.broad!="no LU",c("yieldID","yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] 

### specify columns used for prediction
### check whether mean/sd ratio for yield differ according to yield units
#boxplot(yield.mean/yield.SD~Yield.Unit.Type,data=data) # no, they don't

### only impute SDs using the corresponding means and sample.size
# predictorMatrix1 <- matrix(c(rep(0,4),rep(0,4),c(0,1,0,1),rep(0,4)),
#                            ncol=4,byrow=T)
# 
# nchains <- 10
# 
# ### impute
# temp <- complete(mice(data2imp.richness, predictorMatrix=predictorMatrix1,
#                       method = "pmm",
#                       m=nchains, maxit =20, printFlag = FALSE), 
#                  "long")
# data2imp.richness$richness.SD <- rowMeans(matrix(temp$richness.SD, ncol=nchains, byrow=F))
# 
# temp.richness <- data.frame(matrix(temp$richness.SD, ncol=nchains, byrow=F))
# temp.richness$mean <- rowMeans(temp.richness[,1:nchains])
# temp.richness$sd <- apply(temp.richness[,1:nchains],1,sd)
# 
# temp <- complete(mice(data2imp.yield, predictorMatrix=predictorMatrix1,
#                       method = "pmm",
#                       m=nchains, maxit =20, printFlag = FALSE), 
#                  "long")
# data2imp.yield$yield.SD <- rowMeans(matrix(temp$yield.SD, ncol=nchains, byrow=F))
# 
# dataimp$richness.SD[is.na(dataimp$richness.SD)]<-data2imp.richness$richness.SD[match(dataimp$richnessID[is.na(dataimp$richness.SD)],data2imp.richness$richnessID)]
# dataimp$yield.SD[is.na(dataimp$yield.SD)]<-data2imp.yield$yield.SD[match(dataimp$yieldID[is.na(dataimp$yield.SD)],data2imp.yield$yieldID)]
# 
# ### check variability of imputation
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
# 
# p.yield <- ggplot(dataimp) +
#   geom_point(aes(x=yield.mean, y=yield.SD, color=Yield.SD.is.imputed, size=4, alpha=.5)) +
#   xlim(range(dataimp$yield.mean[dataimp$Yield.SD.is.imputed=="yes"],na.rm=T)) +
#   ylim(range(dataimp$yield.SD[dataimp$Yield.SD.is.imputed=="yes"],na.rm=T))
# p.yield
# 
# rm(data2imp.richness, data2imp.yield, temp, temp.yield, temp.richness, predictorMatrix1, nchains)
             
############################################################################
### crude-impute based on average SD/mean ratio
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


# ############################################################################
### impute missing data using mi package
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
