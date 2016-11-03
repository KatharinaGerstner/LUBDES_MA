############################################################################
### Purpose of this skript module 09.2 is to:
###
### 09.2.1. Predictions for richness
### 09.2.2. Predictions for yield
### 09.2.3. Join and save predictions for richness and yield
### 09.2.4. Map predictions facetted by Product and/or Species.Group
###
### Authors: KG, ...
############################################################################

############################################################################
### 09.2.1. Predictions for richness
############################################################################
model <- Richness.MA.model[["LUI.SGP"]]

newdat.richness.LUI.SGP <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                               Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness.LUI.SGP$n.richness <- NA

for(i in 1:nrow(newdat.richness.LUI.SGP)){
  newdat.richness.LUI.SGP$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness.LUI.SGP$Product[i] & modelDataRichness$Species.Group %in% newdat.richness.LUI.SGP$Species.Group[i] & modelDataRichness$LUI.range.level %in% newdat.richness.LUI.SGP$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + Species.Group + LUI.range.level:Product + LUI.range.level:Species.Group + Species.Group:Product-1,data=newdat.richness.LUI.SGP)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness.LUI.SGP[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness.LUI.SGP <- subset(newdat.richness.LUI.SGP,n.richness>0)
newdat.richness.LUI.SGP <- newdat.richness.LUI.SGP[,c("Species.Group","Product","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]
newdat.richness.LUI.SGP <- rename.factor.levels(newdat.richness.LUI.SGP)

############################################################################
model <- Richness.MA.model[["LUI.SG"]]

newdat.richness.LUI.SG <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                                      Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness.LUI.SG$n.richness <- NA

for(i in 1:nrow(newdat.richness.LUI.SG)){
  newdat.richness.LUI.SG$n.richness[i] <- length(which(modelDataRichness$Species.Group %in% newdat.richness.LUI.SG$Species.Group[i] & modelDataRichness$LUI.range.level %in% newdat.richness.LUI.SG$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Species.Group + LUI.range.level:Species.Group-1,data=newdat.richness.LUI.SG)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness.LUI.SG[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness.LUI.SG <- subset(newdat.richness.LUI.SG,n.richness>0)
newdat.richness.LUI.SG <- newdat.richness.LUI.SG[,c("Species.Group","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]
newdat.richness.LUI.SG <- rename.factor.levels(newdat.richness.LUI.SG)

############################################################################
model <- Richness.MA.model[["LUI.P"]]

newdat.richness.LUI.P <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                                     Product = levels(modelDataRichness$Product)) 

## count number of cases within groups
newdat.richness.LUI.P$n.richness <- NA

for(i in 1:nrow(newdat.richness.LUI.P)){
  newdat.richness.LUI.P$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness.LUI.P$Product[i] & modelDataRichness$LUI.range.level %in% newdat.richness.LUI.P$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + LUI.range.level:Product-1,data=newdat.richness.LUI.P)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness.LUI.P[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness.LUI.P <- subset(newdat.richness.LUI.P,n.richness>0)
newdat.richness.LUI.P <- newdat.richness.LUI.P[,c("Product","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]
newdat.richness.LUI.P <- rename.factor.levels(newdat.richness.LUI.P)

############################################################################
model <- Richness.MA.model[["SGP"]]

newdat.richness.SGP <- expand.grid(Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 
newdat.richness.SGP$LUI.range.level <- "Grand mean"

## count number of cases within groups
newdat.richness.SGP$n.richness <- NA

for(i in 1:nrow(newdat.richness.SGP)){
  newdat.richness.SGP$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness.SGP$Product[i] & modelDataRichness$Species.Group %in% newdat.richness.SGP$Species.Group[i]))
}

mm <- model.matrix(~Product + Species.Group + Species.Group:Product-1,data=newdat.richness.SGP)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness.SGP[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness.SGP <- subset(newdat.richness.SGP,n.richness>0)
newdat.richness.SGP <- newdat.richness.SGP[,c("Species.Group","Product","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]
newdat.richness.SGP <- rename.factor.levels(newdat.richness.SGP)

############################################################################
model <- Richness.MA.model[["SG"]]

newdat.richness.SG <- expand.grid(Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness.SG$n.richness <- NA

for(i in 1:nrow(newdat.richness.SG)){
  newdat.richness.SG$n.richness[i] <- length(which(modelDataRichness$Species.Group %in% newdat.richness.SG$Species.Group[i]))
}

mm <- model.matrix(~Species.Group-1,data=newdat.richness.SG)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness.SG[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness.SG <- subset(newdat.richness.SG,n.richness>0)
newdat.richness.SG <- newdat.richness.SG[,c("Species.Group","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]
newdat.richness.SG <- rename.factor.levels(newdat.richness.SG)

############################################################################
model <- Richness.MA.model[["P"]]

newdat.richness.P <- expand.grid(Product = levels(modelDataRichness$Product)) 

## count number of cases within groups
newdat.richness.P$n.richness <- NA

for(i in 1:nrow(newdat.richness.P)){
  newdat.richness.P$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness.P$Product[i]))
}

mm <- model.matrix(~Product-1,data=newdat.richness.P)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness.P[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness.P <- subset(newdat.richness.P,n.richness>0)
newdat.richness.P <- newdat.richness.P[,c("Product","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]
newdat.richness.P <- rename.factor.levels(newdat.richness.P)

############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["LUI.P"]]

newdat.yield.LUI.SGP <- expand.grid(LUI.range.level=levels(modelDataYield$LUI.range.level),
                            Product = levels(modelDataYield$Product))

## count number of cases within groups
newdat.yield.LUI.SGP$n.yield <- NA

for(i in 1:nrow(newdat.yield.LUI.SGP)){
  newdat.yield.LUI.SGP$n.yield[i] <- length(which(modelDataYield$Product %in% newdat.yield.LUI.SGP$Product[i] & modelDataYield$LUI.range.level %in% newdat.yield.LUI.SGP$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + LUI.range.level:Product - 1,data=newdat.yield.LUI.SGP)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield.LUI.SGP[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield.LUI.SGP <- subset(newdat.yield.LUI.SGP,n.yield>0)
newdat.yield.LUI.SGP <- newdat.yield.LUI.SGP[,c("Product","LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]
newdat.yield.LUI.SGP <- rename.factor.levels(newdat.yield.LUI.SGP)

############################################################################
model <- Yield.MA.model[["LUI"]]

newdat.yield.LUI.SG <- expand.grid(LUI.range.level=levels(modelDataYield$LUI.range.level))

## count number of cases within groups
newdat.yield.LUI.SG$n.yield <- NA

for(i in 1:nrow(newdat.yield.LUI.SG)){
  newdat.yield.LUI.SG$n.yield[i] <- length(which(modelDataYield$LUI.range.level %in% newdat.yield.LUI.SG$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level - 1,data=newdat.yield.LUI.SG)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield.LUI.SG[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield.LUI.SG <- subset(newdat.yield.LUI.SG,n.yield>0)
newdat.yield.LUI.SG <- newdat.yield.LUI.SG[,c("LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]
newdat.yield.LUI.SG <- rename.factor.levels(newdat.yield.LUI.SG)

##################
newdat.yield.LUI.P <- newdat.yield.LUI.SGP

##########################################################
model <- Yield.MA.model[["P"]]

newdat.yield.P <- expand.grid(Product = levels(modelDataYield$Product))
newdat.yield.P$LUI.range.level <- "Grand Mean"

## count number of cases within groups
newdat.yield.P$n.yield <- NA

for(i in 1:nrow(newdat.yield.P)){
  newdat.yield.P$n.yield[i] <- length(which(modelDataYield$Product %in% newdat.yield.P$Product[i]))
}

mm <- model.matrix(~Product - 1,data=newdat.yield.P)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield.P[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield.P <- subset(newdat.yield.P,n.yield>0)
newdat.yield.P <- newdat.yield.P[,c("Product","LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]
newdat.yield.P <- rename.factor.levels(newdat.yield.P)

############################################################################
model <- Yield.MA.model[["None"]]

newdat.yield.SGP <- data.frame(1)
newdat.yield.SGP$LUI.range.level <- "Grand mean"
newdat.yield.SGP$logRR.yield <- model$b
newdat.yield.SGP$logRR.yield.se <- model$se
newdat.yield.SGP$n.yield <- nrow(modelDataYield)
newdat.yield.SGP$logRR.yield.ci.lb <- model$b-1.96*model$se
newdat.yield.SGP$logRR.yield.ci.ub <- model$b+1.96*model$se

############################################################################
### 09.2.3. Join and save predictions for richness and yield
############################################################################
### LUI.SPG and SGP
newdat.SGP <- join_all(list(newdat.richness.SGP,newdat.yield.SGP),type="full")
newdat.LUI.SGP <- join_all(list(newdat.richness.LUI.SGP,newdat.yield.LUI.SGP,newdat.SGP),type="full")
newdat.LUI.SGP$CI95.richness <- "[" %+% round(newdat.LUI.SGP$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SGP$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.SGP$CI95.yield <- "[" %+% round(newdat.LUI.SGP$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SGP$logRR.yield.ci.ub,digits=2) %+% "]" 
newdat.LUI.SGP[,c("perc.rich.change","perc.rich.change.ci.lb","perc.rich.change.ci.ub","CI95.perc.rich.change")] <- convert.log2equidist(newdat.LUI.SGP$logRR.richness,newdat.LUI.SGP$logRR.richness.ci.lb,newdat.LUI.SGP$logRR.richness.ci.ub)
newdat.LUI.SGP[,c("perc.yield.change","perc.yield.change.ci.lb","perc.yield.change.ci.ub","CI95.perc.yield.change")] <- convert.log2equidist(newdat.LUI.SGP$logRR.yield,newdat.LUI.SGP$logRR.yield.ci.lb,newdat.LUI.SGP$logRR.yield.ci.ub)

newdat.LUI.SGP$LUI.range.level <- factor(newdat.LUI.SGP$LUI.range.level, levels = c("Grand mean","Low-low","Medium-medium","High-high","Low-medium","Medium-high","Low-high"))

# write.csv(newdat.LUI.SGP[,c("Species.Group","Product","LUI.range.level",
#                  "n.richness", "logRR.richness",  "logRR.richness.se", "CI95.richness",
#                  "n.yield", "logRR.yield",  "logRR.yield.se", "CI95.yield")],
#           file=path2temp %+% "preds.LUI.SGP.csv",row.names=F)
#print(xtable(newdat, caption="Response ratios for the LUI.SGP model and available evidence"),type="latex",include.rownames=F)

### LUI.SG and SG
newdat.SG <- cbind(newdat.richness.SG,rbind(newdat.yield.SGP[,-1],newdat.yield.SGP[,-1],newdat.yield.SGP[,-1]))
newdat.LUI.SG <- join_all(list(newdat.richness.LUI.SG,newdat.yield.LUI.SG,newdat.SG),type="full")
newdat.LUI.SG$CI95.richness <- "[" %+% round(newdat.LUI.SG$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SG$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.SG$CI95.yield <- "[" %+% round(newdat.LUI.SG$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SG$logRR.yield.ci.ub,digits=2) %+% "]" 
newdat.LUI.SG[,c("perc.rich.change","perc.rich.change.ci.lb","perc.rich.change.ci.ub","CI95.perc.rich.change")] <- convert.log2equidist(newdat.LUI.SG$logRR.richness,newdat.LUI.SG$logRR.richness.ci.lb,newdat.LUI.SG$logRR.richness.ci.ub)
newdat.LUI.SG[,c("perc.yield.change","perc.yield.change.ci.lb","perc.yield.change.ci.ub","CI95.perc.yield.change")] <- convert.log2equidist(newdat.LUI.SG$logRR.yield,newdat.LUI.SG$logRR.yield.ci.lb,newdat.LUI.SG$logRR.yield.ci.ub)


### LUI.P and P
newdat.P <- join_all(list(newdat.richness.P,newdat.yield.P),type="full")
newdat.LUI.P <- join_all(list(newdat.richness.LUI.P,newdat.yield.LUI.P,newdat.P),type="full")
newdat.LUI.P$CI95.richness <- "[" %+% round(newdat.LUI.P$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.P$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.P$CI95.yield <- "[" %+% round(newdat.LUI.P$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.P$logRR.yield.ci.ub,digits=2) %+% "]" 
newdat.LUI.P[,c("perc.rich.change","perc.rich.change.ci.lb","perc.rich.change.ci.ub","CI95.perc.rich.change")] <- convert.log2equidist(newdat.LUI.P$logRR.richness,newdat.LUI.P$logRR.richness.ci.lb,newdat.LUI.P$logRR.richness.ci.ub)
newdat.LUI.P[,c("perc.yield.change","perc.yield.change.ci.lb","perc.yield.change.ci.ub","CI95.perc.yield.change")] <- convert.log2equidist(newdat.LUI.P$logRR.yield,newdat.LUI.P$logRR.yield.ci.lb,newdat.LUI.P$logRR.yield.ci.ub)

############################################################################
# newdat.LUI.SG$Product <- NA
# newdat.LUI.SG$Product <- factor(newdat.LUI.SG$Product, levels = c("Crop","Green fodder","Wood","NA"))
# newdat.LUI.P$Species.Group <- NA
# newdat.LUI.P$Species.Group <- factor(newdat.LUI.P$Species.Group, levels = c("Plants","Invertebrates","Vertebrates","NA"))

newdat.SG.P <- join_all(list(newdat.LUI.SG,newdat.LUI.P),type="full")
newdat.all <- join_all(list(newdat.LUI.SGP,newdat.LUI.SG,newdat.LUI.P),type="full")

newdat.all[,c("perc.rich.change","perc.rich.change.ci.lb","perc.rich.change.ci.ub","CI95.perc.rich.change")] <- convert.log2equidist(newdat.all$logRR.richness,newdat.all$logRR.richness.ci.lb,newdat.all$logRR.richness.ci.ub)
newdat.all[,c("perc.yield.change","perc.yield.change.ci.lb","perc.yield.change.ci.ub","CI95.perc.yield.change")] <- convert.log2equidist(newdat.all$logRR.yield,newdat.all$logRR.yield.ci.lb,newdat.all$logRR.yield.ci.ub)

# newdat.all$perc.rich.change <- 100*(exp(newdat.all$logRR.richness)-1)
# newdat.all$perc.rich.change.ci.lb <- 100*(exp(newdat.all$logRR.richness.ci.lb)-1)
# newdat.all$perc.rich.change.ci.ub <- 100*(exp(newdat.all$logRR.richness.ci.ub)-1)
# newdat.all$CI95.perc.rich.change <- "[" %+% round(newdat.all$perc.rich.change.ci.lb,digits=2) %+% "," %+%  round(newdat.all$perc.rich.change.ci.ub,digits=2) %+% "]"
# 
# newdat.all$perc.yield.change <- 100*(exp(newdat.all$logRR.yield)-1)
# newdat.all$perc.yield.change.ci.lb <- 100*(exp(newdat.all$logRR.yield.ci.lb)-1)
# newdat.all$perc.yield.change.ci.ub <- 100*(exp(newdat.all$logRR.yield.ci.ub)-1)
# newdat.all$CI95.perc.yield.change <- "[" %+% round(newdat.all$perc.yield.change.ci.lb,digits=2) %+% "," %+%  round(newdat.all$perc.yield.change.ci.ub,digits=2) %+% "]"

write.csv(newdat.all[,c("Species.Group","Product","LUI.range.level",
                        "n.richness", "perc.rich.change",  "CI95.perc.rich.change",
                        "n.yield", "perc.yield.change","CI95.perc.yield.change")],
          file=path2temp %+% "preds.LUI.SGP.SG.P.csv",row.names=F)

############################################################################
### 09.2.4. Map predictions facetted by Product and/or Species.Group
############################################################################

# seqBreaks <- log(sapply(-2:7,function(x) 2^x))
# seqLabels <- 100*(exp(seqBreaks)-1)
seqBreaks <- seq(-1,2,by=0.5)# c(0.6,0.8,0.9,1,1.25,1.5,1.75,2)
seqLabels <- 100*seqBreaks

### plot 1 with legend
plot1 <- ggplot(data=newdat.LUI.SGP) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=0.6) + geom_vline(aes(xintercept=0), linetype="twodash",size=0.6) +
  geom_point(aes(x=perc.yield.change, y=perc.rich.change, color=LUI.range.level), size=2.5) +
  geom_pointrange(aes(x=perc.yield.change, y=perc.rich.change, ymin=perc.rich.change.ci.lb, 
                      ymax=perc.rich.change.ci.ub,color=LUI.range.level), size=1.3) +
  geom_segment(aes(x=perc.yield.change.ci.lb, xend=perc.yield.change.ci.ub, y = perc.rich.change, yend = perc.rich.change, color=LUI.range.level),size=1.2) +
  scale_y_continuous(breaks=seqLabels,limits=c(-65,90), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(breaks=seqLabels,limits=c(-75,140), oob = squish, expand=c(0,0)) +
  scale_color_manual(name="",values=c("Low-low"='#2b83ba',"Medium-medium"='#008837',"High-high"='#abdda4',"Low-medium"='#fdae61',"Medium-high"='#d7191c',"Low-high"='#7b3294',"Grand mean"="black"),labels=levels(newdat.LUI.SGP$LUI.range.level)) +
  ylab("% Richness difference") + xlab("% Yield difference") + 
  facet_grid(Species.Group~Product) + 
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
  theme(strip.text.x = element_blank(),strip.text.y = element_blank(),legend.key.size=unit(1.6,"line")) +
  guides(color=guide_legend(direction="vertical"))
#  guides(color=F)
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

### save legend
# # if legend from plot1 is drawn
legend <- g_legend(plot1)
png(path2temp %+% "legend.png")
grid.draw(legend)
dev.off()

### plot1 without legend
plot1 <- ggplot(data=newdat.LUI.SGP) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=0.6) + geom_vline(aes(xintercept=0), linetype="twodash",size=0.6) +
  geom_point(aes(x=perc.yield.change, y=perc.rich.change, color=LUI.range.level), size=2.5) +
  geom_pointrange(aes(x=perc.yield.change, y=perc.rich.change, ymin=perc.rich.change.ci.lb, 
                      ymax=perc.rich.change.ci.ub,color=LUI.range.level), size=1.3) +
  geom_segment(aes(x=perc.yield.change.ci.lb, xend=perc.yield.change.ci.ub, y = perc.rich.change, yend = perc.rich.change, color=LUI.range.level),size=1.2) +
  scale_y_continuous(breaks=seqLabels,limits=c(-65,90), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(breaks=seqLabels,limits=c(-75,140), oob = squish, expand=c(0,0)) +
  scale_color_manual(name="",values=c("Low-low"='#2b83ba',"Medium-medium"='#008837',"High-high"='#abdda4',"Low-medium"='#fdae61',"Medium-high"='#d7191c',"Low-high"='#7b3294',"Grand mean"="black"),labels=levels(newdat.LUI.SGP$LUI.range.level)) +
  ylab("% Richness difference") + xlab("% Yield difference") + 
  facet_grid(Species.Group~Product) + 
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
  theme(strip.text.x = element_blank(),strip.text.y = element_blank(),legend.key.size=unit(1.6,"line")) +
  guides(color=guide_legend(direction="vertical")) +
  guides(color=F)

plot2 <- ggplot(data=newdat.LUI.SG) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=0.6) + geom_vline(aes(xintercept=0), linetype="twodash",size=0.6) +
  geom_point(aes(x=perc.yield.change, y=perc.rich.change, color=LUI.range.level), size=2.5) +
  geom_pointrange(aes(x=perc.yield.change, y=perc.rich.change, ymin=perc.rich.change.ci.lb, 
                      ymax=perc.rich.change.ci.ub,color=LUI.range.level), size=1.3) +
  geom_segment(aes(x=perc.yield.change.ci.lb, xend=perc.yield.change.ci.ub, y = perc.rich.change, yend = perc.rich.change, color=LUI.range.level),size=1.2) +
  scale_y_continuous(breaks=seqLabels,limits=c(-65,90), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(breaks=seqLabels,limits=c(-75,140), oob = squish, expand=c(0,0)) +
  scale_color_manual(name="",values=c("Low-low"='#2b83ba',"Medium-medium"='#008837',"High-high"='#abdda4',"Low-medium"='#fdae61',"Medium-high"='#d7191c',"Low-high"='#7b3294',"Grand mean"="black"),labels=levels(newdat.LUI.SGP$LUI.range.level)) +
  ylab("") + xlab("") + 
  facet_grid(Species.Group~.) + 
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
  theme(axis.text.y=element_blank()) +
  guides(color=F)

plot3 <- ggplot(data=newdat.LUI.P) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=0.6) + geom_vline(aes(xintercept=0), linetype="twodash",size=0.6) +
  geom_point(aes(x=perc.yield.change, y=perc.rich.change, color=LUI.range.level), size=2.5) +
  geom_pointrange(aes(x=perc.yield.change, y=perc.rich.change, ymin=perc.rich.change.ci.lb, 
                      ymax=perc.rich.change.ci.ub,color=LUI.range.level), size=1.3) +
  geom_segment(aes(x=perc.yield.change.ci.lb, xend=perc.yield.change.ci.ub, y = perc.rich.change, yend = perc.rich.change, color=LUI.range.level),size=1.2) +
  scale_y_continuous(breaks=seqLabels,limits=c(-65,90), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(breaks=seqLabels,limits=c(-75,140), oob = squish, expand=c(0,0)) +
  scale_color_manual(name="",values=c("Low-low"='#2b83ba',"Medium-medium"='#008837',"High-high"='#abdda4',"Low-medium"='#fdae61',"Medium-high"='#d7191c',"Low-high"='#7b3294',"Grand mean"="black"),labels=levels(newdat.LUI.SGP$LUI.range.level)) +
  ylab("") + xlab("") + 
  facet_grid(.~Product) +
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
  theme(axis.text.x=element_blank()) +
  guides(color=F)

plot.grid <- grid.arrange(plot3,legend, plot1, plot2, nrow=2,ncol=2, heights=c(5,13),widths=c(13,5))
ggsave(plot.grid,file=path2temp %+% "CrossPlot_equidist.png",height=10,width=15, units = "in")




# 
# model1 <- Richness.MA.model[["None"]]
# preds <- predict.rma(model1)
# newdat.GM <- data.frame(Species.Group=newdat.LUI.SGP$Species.Group,Product=newdat.LUI.SGP$Product,LUI.range.level="Grand Mean",n.richness=nrow(modelDataRichness),logRR.richness=preds$pred,logRR.richness.se=preds$se,logRR.richness.ci.lb=preds$ci.lb,logRR.richness.ci.ub=preds$ci.ub)
# 
# model2 <- Yield.MA.model[["None"]]
# preds <- predict.rma(model2)
# newdat.GM[,c("n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- matrix(rep(c(nrow(modelDataYield),preds$pred,preds$se,preds$ci.lb,preds$ci.ub),times=nrow(newdat.GM)),ncol=5,byrow=T)

#newdat <- join_all(list(newdat.LUI.SGP,newdat.SGP),type="full")
