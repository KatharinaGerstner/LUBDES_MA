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

mm <- model.matrix(~LUI.range.level + Product + Species.Group + LUI.range.level:Product + LUI.range.level:Species.Group-1,data=newdat.richness)
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

mm <- model.matrix(~Product + Species.Group-1,data=newdat.richness)
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

# 
# model1 <- Richness.MA.model[["None"]]
# preds <- predict.rma(model1)
# newdat.GM <- data.frame(Species.Group=newdat.LUI.SGP$Species.Group,Product=newdat.LUI.SGP$Product,LUI.range.level="Grand Mean",n.richness=nrow(modelDataRichness),logRR.richness=preds$pred,logRR.richness.se=preds$se,logRR.richness.ci.lb=preds$ci.lb,logRR.richness.ci.ub=preds$ci.ub)
# 
# model2 <- Yield.MA.model[["None"]]
# preds <- predict.rma(model2)
# newdat.GM[,c("n.yield","logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- matrix(rep(c(nrow(modelDataYield),preds$pred,preds$se,preds$ci.lb,preds$ci.ub),times=nrow(newdat.GM)),ncol=5,byrow=T)

newdat <- join_all(list(newdat.LUI.SGP,newdat.SGP),type="full")
write.csv(newdat,file=path2temp %+% "preds.LUI.SGP.csv",row.names=F)
print(xtable(newdat, caption="Response ratios for the LUI.SGP model and available evidence"),type="latex",include.rownames=F)

############################################################################
### 09.2.3. Map predictions facetted by Product and LUI on top of biomes
############################################################################

levels(newdat$Product)[levels(newdat$Product)=="animal_feed"]  <- "animal feed"
plot <- ggplot(data=newdat,aes(x=logRR.yield, y=logRR.richness,color=factor(LUI.range.level))) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=3) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                      ymax=logRR.richness + (1.96*logRR.richness.se),color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1.2) +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="black"),breaks=c(levels(newdat$LUI.range.level))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") +
  facet_grid(Species.Group~Product) + 
  theme_lubdes()
print(plot)
ggsave(plot, file = path2temp %+% "CrossPlot_LUI.SGP.png", width = 15, height = 8, type = "cairo-png")


