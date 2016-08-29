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

############################################################################
model <- Richness.MA.model[["SGP"]]

newdat.richness.SGP <- expand.grid(Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 
newdat.richness.SGP$LUI.range.level <- "Grand Mean"

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

############################################################################
model <- Yield.MA.model[["None"]]

newdat.yield.SGP <- data.frame(1)
newdat.yield.SGP$LUI.range.level <- "Grand Mean"
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

newdat.LUI.SGP$LUI.range.level <- factor(newdat.LUI.SGP$LUI.range.level, levels = c("low-low","medium-medium","high-high","Grand Mean","low-medium","medium-high","low-high"))

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

### LUI.P and P
newdat.P <- join_all(list(newdat.richness.P,newdat.yield.P),type="full")
newdat.LUI.P <- join_all(list(newdat.richness.LUI.P,newdat.yield.LUI.P,newdat.P),type="full")
newdat.LUI.P$CI95.richness <- "[" %+% round(newdat.LUI.P$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.P$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.P$CI95.yield <- "[" %+% round(newdat.LUI.P$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.P$logRR.yield.ci.ub,digits=2) %+% "]" 

############################################################################
newdat.LUI.SG$Product <- NA
newdat.LUI.SG$Product <- factor(newdat.LUI.SG$Product, levels = c("crop","green fodder","wood","NA"))
newdat.LUI.P$Species.Group <- NA
newdat.LUI.P$Species.Group <- factor(newdat.LUI.P$Species.Group, levels = c("plants","invertebrates","vertebrates","NA"))

newdat.SG.P <- join_all(list(newdat.LUI.SG,newdat.LUI.P),type="full")
newdat.all <- join_all(list(newdat.LUI.SGP,newdat.LUI.SG,newdat.LUI.P),type="full")

newdat.all$perc.rich.change <- 100*(exp(newdat.all$logRR.richness)-1)
newdat.all$perc.rich.change.ci.lb <- 100*(exp(newdat.all$logRR.richness.ci.lb)-1)
newdat.all$perc.rich.change.ci.ub <- 100*(exp(newdat.all$logRR.richness.ci.ub)-1)
newdat.all$CI95.perc.rich.change <- "[" %+% round(newdat.all$perc.rich.change.ci.lb,digits=2) %+% "," %+%  round(newdat.all$perc.rich.change.ci.ub,digits=2) %+% "]"

newdat.all$perc.yield.change <- 100*(exp(newdat.all$logRR.yield)-1)
newdat.all$perc.yield.change.ci.lb <- 100*(exp(newdat.all$logRR.yield.ci.lb)-1)
newdat.all$perc.yield.change.ci.ub <- 100*(exp(newdat.all$logRR.yield.ci.ub)-1)
newdat.all$CI95.perc.yield.change <- "[" %+% round(newdat.all$perc.yield.change.ci.lb,digits=2) %+% "," %+%  round(newdat.all$perc.yield.change.ci.ub,digits=2) %+% "]"

write.csv(newdat.all[,c("Species.Group","Product","LUI.range.level",
                        "n.richness", "perc.rich.change",  "CI95.perc.rich.change",
                        "n.yield", "perc.yield.change","CI95.perc.yield.change")],
          file=path2temp %+% "preds.LUI.SGP.SG.P.csv",row.names=F)

############################################################################
### 09.2.4. Map predictions facetted by Product and/or Species.Group
############################################################################

seqBreaks <- log(sapply(-2:7,function(x) 2^x))
seqLabels <- 100*(exp(seqBreaks)-1)

plot1 <- ggplot(data=newdat.LUI.SGP) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=3) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                      ymax=logRR.richness + (1.96*logRR.richness.se),color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1.2) +
  scale_y_continuous(labels=seqLabels,breaks=seqBreaks,limits=c(log(0.4),log(2.05)), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(labels=seqLabels,breaks=seqBreaks,limits=c(log(0.3),log(3.25)), oob = squish, expand=c(0,0)) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","Grand Mean"="black","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027"),breaks=c(levels(newdat.LUI.SGP$LUI.range.level))) +
  ylab("% Richness difference") + xlab("% Yield difference") + labs(color='') + 
  facet_grid(Species.Group~Product) + 
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
#  guides(color=guide_legend(ncol=2))
  guides(color=F)
ggsave(plot1,file = path2temp %+% "CrossPlot_LUI.SGP.png", width = 15, height = 10, units = "in")

plot2 <- ggplot(data=newdat.LUI.SG) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=2) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                      ymax=logRR.richness + (1.96*logRR.richness.se),color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1) +
  scale_y_continuous(labels=seqLabels,breaks=seqBreaks,limits=c(log(0.4),log(2.05)), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(labels=seqLabels,breaks=seqBreaks,limits=c(log(0.3),log(3.25)), oob = squish, expand=c(0,0)) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="black"),breaks=c(levels(newdat.LUI.SG$LUI.range.level))) +
  ylab("% Richness difference") + xlab("% Yield difference") + labs(color='') + 
  facet_grid(Species.Group~.) + 
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
  guides(color=F)
ggsave(plot2,file=path2temp %+% "CrossPlot_LUI.SG.png",height=9,width=5.5, units = "in")

plot3 <- ggplot(data=newdat.LUI.P) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=2) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                      ymax=logRR.richness + (1.96*logRR.richness.se),color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1) +
  scale_y_continuous(labels=seqLabels,breaks=seqBreaks,limits=c(log(0.4),log(2.05)), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(labels=seqLabels,breaks=seqBreaks,limits=c(log(0.3),log(3.25)), oob = squish, expand=c(0,0)) +
  scale_colour_manual("",values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="black"),breaks=c(levels(newdat.LUI.P$LUI.range.level))) +
  ylab("% Richness difference") + xlab("% Yield difference") + labs(color='') + 
  facet_grid(.~Product) +
  theme_lubdes(legend.position="bottom",rel.text.size=1.8) +
  guides(color=F)
ggsave(plot3,file=path2temp %+% "CrossPlot_LUI.P.png",height=3.8,width=14, units = "in")

g_legend<-function(a.gplot){ 
      tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
      legend <- tmp$grobs[[leg]] 
      return(legend)} 
 
# # if legend from plot1 is drawn
# legend <- g_legend(plot1) 
# png(path2temp %+% "legend.png")
# grid.draw(legend) 
# dev.off()


# 
# model1 <- Richness.MA.model[["None"]]
# preds <- predict.rma(model1)
# newdat.GM <- data.frame(Species.Group=newdat.LUI.SGP$Species.Group,Product=newdat.LUI.SGP$Product,LUI.range.level="Grand Mean",n.richness=nrow(modelDataRichness),logRR.richness=preds$pred,logRR.richness.se=preds$se,logRR.richness.ci.lb=preds$ci.lb,logRR.richness.ci.ub=preds$ci.ub)
# 
# model2 <- Yield.MA.model[["None"]]
# preds <- predict.rma(model2)
# newdat.GM[,c("n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- matrix(rep(c(nrow(modelDataYield),preds$pred,preds$se,preds$ci.lb,preds$ci.ub),times=nrow(newdat.GM)),ncol=5,byrow=T)

#newdat <- join_all(list(newdat.LUI.SGP,newdat.SGP),type="full")
