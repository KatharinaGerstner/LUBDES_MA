############################################################################
### Purpose of this skript module 09.2 is to:
###
### 09.2.1/2 Predict effects of LUI on richness/yield using the context model
### 09.2.2/2 Map Biomes with high risk of biodiversity loss/high potential of yield gains facetted using product 
### 09.2.3/2 Predict effects of LUI on richness/yield using the select model
### 09.2.2/2 Map Biomes with high risk of biodiversity loss/high potential of yield gains facetted using product and LUI.range.level
###
### Authors: KG, ...
############################################################################

seqBreaks <- log(sapply(-2:7,function(x) 2^x))

############################################################################
### 09.2.1. Predictions for richness
############################################################################
model <- Richness.MA.model[["SGP"]]

newdat.richness <- expand.grid(Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 
newdat.richness$LUI.range.level <- "Grand Mean"

## count number of cases within groups
newdat.richness$n.richness <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i]))
}

mm <- model.matrix(~Product + Species.Group + Species.Group:Product-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n.richness>0)
newdat.richness <- newdat.richness[,c("Species.Group","Product","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]

############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["SGP"]]

newdat.yield <- expand.grid(Product = levels(modelDataYield$Product))
newdat.yield$LUI.range.level <- "Grand Mean"

## count number of cases within groups
newdat.yield$n.yield <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n.yield[i] <- length(which(modelDataYield$Product %in% newdat.yield$Product[i]))
}

mm <- model.matrix(~Product - 1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n.yield>0)
newdat.yield <- newdat.yield[,c("Product","LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]

newdat.SGP <- join_all(list(newdat.richness,newdat.yield),type="full")
newdat.SGP$CI95.richness <- "[" %+% round(newdat.SGP$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.SGP$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.SGP$CI95.yield <- "[" %+% round(newdat.SGP$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.SGP$logRR.yield.ci.ub,digits=2) %+% "]" 

############################################################################
### 09.2.1. Predictions for richness
############################################################################
model <- Richness.MA.model[["LUI.SGP"]]

newdat.richness <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                               Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness$n.richness <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + Species.Group + LUI.range.level:Product + LUI.range.level:Species.Group + Species.Group:Product-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n.richness>0)
newdat.richness <- newdat.richness[,c("Species.Group","Product","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]

############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["LUI.SGP"]]

newdat.yield <- expand.grid(LUI.range.level=levels(modelDataYield$LUI.range.level),
                            Product = levels(modelDataYield$Product))

## count number of cases within groups
newdat.yield$n.yield <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n.yield[i] <- length(which(modelDataYield$Product %in% newdat.yield$Product[i] & modelDataYield$LUI.range.level %in% newdat.yield$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + LUI.range.level:Product - 1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n.yield>0)
newdat.yield <- newdat.yield[,c("Product","LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]

newdat.LUI.SGP <- join_all(list(newdat.richness,newdat.yield),type="full")
newdat.LUI.SGP$CI95.richness <- "[" %+% round(newdat.LUI.SGP$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SGP$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.SGP$CI95.yield <- "[" %+% round(newdat.LUI.SGP$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SGP$logRR.yield.ci.ub,digits=2) %+% "]" 

write.csv(newdat.LUI.SGP[,c("Species.Group","Product","LUI.range.level",
                 "n.richness", "logRR.richness",  "logRR.richness.se", "CI95.richness",
                 "n.yield", "logRR.yield",  "logRR.yield.se", "CI95.yield")],
          file=path2temp %+% "preds.LUI.SGP.csv",row.names=F)
#print(xtable(newdat, caption="Response ratios for the LUI.SGP model and available evidence"),type="latex",include.rownames=F)

############################################################################
model <- Richness.MA.model[["LUI.SG"]]

newdat.richness <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                               Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness$n.richness <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n.richness[i] <- length(which(modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Species.Group + LUI.range.level:Species.Group-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n.richness>0)
newdat.richness <- newdat.richness[,c("Species.Group","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]

############################################################################
model <- Yield.MA.model[["LUI"]]

newdat.yield <- expand.grid(LUI.range.level=levels(modelDataYield$LUI.range.level))

## count number of cases within groups
newdat.yield$n.yield <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n.yield[i] <- length(which(modelDataYield$LUI.range.level %in% newdat.yield$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level - 1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n.yield>0)
newdat.yield <- newdat.yield[,c("LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]

newdat.LUI.SG <- join_all(list(newdat.richness,newdat.yield),type="full")
newdat.LUI.SG$CI95.richness <- "[" %+% round(newdat.LUI.SG$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SG$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.SG$CI95.yield <- "[" %+% round(newdat.LUI.SG$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.SG$logRR.yield.ci.ub,digits=2) %+% "]" 

############################################################################
model <- Richness.MA.model[["LUI.P"]]

newdat.richness <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                               Product = levels(modelDataRichness$Product)) 

## count number of cases within groups
newdat.richness$n.richness <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + LUI.range.level:Product-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n.richness>0)
newdat.richness <- newdat.richness[,c("Product","LUI.range.level","n.richness","logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")]

############################################################################
model <- Yield.MA.model[["LUI.SGP"]]

newdat.yield <- expand.grid(LUI.range.level=levels(modelDataYield$LUI.range.level),
                            Product = levels(modelDataYield$Product))

## count number of cases within groups
newdat.yield$n.yield <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n.yield[i] <- length(which(modelDataYield$Product %in% newdat.yield$Product[i] & modelDataYield$LUI.range.level %in% newdat.yield$LUI.range.level[i]))
}

mm <- model.matrix(~LUI.range.level + Product + LUI.range.level:Product - 1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n.yield>0)
newdat.yield <- newdat.yield[,c("Product","LUI.range.level","n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")]

newdat.LUI.P <- join_all(list(newdat.richness,newdat.yield),type="full")
newdat.LUI.P$CI95.richness <- "[" %+% round(newdat.LUI.P$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.P$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.LUI.P$CI95.yield <- "[" %+% round(newdat.LUI.P$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.LUI.P$logRR.yield.ci.ub,digits=2) %+% "]" 

############################################################################
newdat <- join_all(list(newdat.LUI.SGP,newdat.SGP),type="full")
newdat$LUI.range.level <- factor(newdat$LUI.range.level, levels = c("low-low","medium-medium","high-high","Grand Mean","low-medium","medium-high","low-high"))

############################################################################
newdat.all <- join_all(list(newdat,newdat.LUI.SG,newdat.LUI.P),type="full")
write.csv(newdat.all[,c("Species.Group","Product","LUI.range.level",
                            "n.richness", "logRR.richness",  "logRR.richness.se", "CI95.richness",
                            "n.yield", "logRR.yield",  "logRR.yield.se", "CI95.yield")],
          file=path2temp %+% "preds.LUI.SGP.SG.P.csv",row.names=F)

############################################################################
### 09.2.3. Map predictions facetted by Product and LUI on top of biomes
############################################################################

plot1 <- ggplot(data=newdat) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=3) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                      ymax=logRR.richness + (1.96*logRR.richness.se),color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1.2) +
  scale_y_continuous(labels=exp(seqBreaks),breaks=seqBreaks,limits=c(log(0.4),log(2.05)), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(labels=exp(seqBreaks),breaks=seqBreaks,limits=c(log(0.3),log(3.25)), oob = squish, expand=c(0,0)) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","Grand Mean"="black","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027"),breaks=c(levels(newdat$LUI.range.level))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='') +
  facet_grid(Species.Group~Product) + 
  theme_lubdes(legend.position="bottom",rel.text.size=1.9) +
#  guides(color=guide_legend(ncol=2))
  guides(color=F)
ggsave(plot1,file = path2temp %+% "CrossPlot_LUI.SGP.png", width = 15, height = 10, units = "in")

plot2 <- ggplot(data=newdat.LUI.SG) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=2) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                      ymax=logRR.richness + (1.96*logRR.richness.se),color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1) +
  scale_y_continuous(labels=exp(seqBreaks),breaks=seqBreaks,limits=c(log(0.4),log(2.05)), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(labels=exp(seqBreaks),breaks=seqBreaks,limits=c(log(0.3),log(3.25)), oob = squish, expand=c(0,0)) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="black"),breaks=c(levels(newdat$LUI.range.level))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='') +
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
  scale_y_continuous(labels=exp(seqBreaks),breaks=seqBreaks,limits=c(log(0.4),log(2.05)), oob = squish, expand=c(0,0)) + 
  scale_x_continuous(labels=exp(seqBreaks),breaks=seqBreaks,limits=c(log(0.3),log(3.25)), oob = squish, expand=c(0,0)) +
  scale_colour_manual("",values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="black"),breaks=c(levels(newdat$LUI.range.level))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='') +
  facet_grid(.~Product) +
  theme_lubdes(legend.position="bottom",rel.text.size=1.4) +
  guides(color=F)
ggsave(plot3,file=path2temp %+% "CrossPlot_LUI.P.png",height=3.2,width=11.2, units = "in")

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
