############################################################################
### Purpose of this skript module 09.3 is to:
###
### 09.3.1. Predictions for richness for model "full"
### 09.3.2. Predictions for yield for model "full"
### 09.3.3. join and save predictions for richness and yield for models "full"
###
### Authors: KG,...
############################################################################

############################################################################
### 09.3.1. Predictions for richness for model "full"
############################################################################
model <- Richness.MA.model[["Full"]]

newdat.richness <- expand.grid(LUI.range.level=levels(modelDataRichness$LUI.range.level),
                               Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group),
                               landuse_history=levels(modelDataRichness$landuse_history),
                               main_climate=levels(modelDataRichness$main_climate)) 

## count number of cases within groups
newdat.richness$n.richness <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n.richness[i] <- length(which(modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i] & modelDataRichness$landuse_history %in% newdat.richness$landuse_history[i] & modelDataRichness$main_climate %in% newdat.richness$main_climate[i]))
}

mm <- model.matrix(~LUI.range.level + Product + Species.Group + landuse_history + main_climate + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:landuse_history + LUI.range.level:main_climate + Product:Species.Group-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n.richness>0)

############################################################################
### 09.3.2. Predictions for yield for model "full"
############################################################################
model <- Yield.MA.model[["Full"]]

newdat.yield <- expand.grid(LUI.range.level=levels(modelDataYield$LUI.range.level),
                            Product = levels(modelDataYield$Product),
                            landuse_history=levels(modelDataYield$landuse_history),
                            main_climate=levels(modelDataYield$main_climate)) 

## count number of cases within groups
newdat.yield$n.yield <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n.yield[i] <- length(which(modelDataYield$Product %in% newdat.yield$Product[i] & modelDataYield$LUI.range.level %in% newdat.yield$LUI.range.level[i] & modelDataYield$landuse_history %in% newdat.yield$landuse_history[i] & modelDataYield$main_climate %in% newdat.yield$main_climate[i]))
}

mm <- model.matrix(~LUI.range.level + Product + landuse_history + main_climate + LUI.range.level:Product + LUI.range.level:landuse_history + LUI.range.level:main_climate -1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n.yield>0)


############################################################################
### 09.3.3. join and save predictions for richness and yield for models "full"
############################################################################

newdat.full <- join_all(list(newdat.richness,newdat.yield),type="full")
newdat.full$n.yield[is.na(newdat.full$n.yield)] <- 0
newdat.full$n.richness[is.na(newdat.full$n.richness)] <- 0
newdat.full$CI95.richness <- "[" %+% round(newdat.full$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.full$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.full$CI95.yield <- "[" %+% round(newdat.full$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.full$logRR.yield.ci.ub,digits=2) %+% "]" 

newdat.full[,c("perc.rich.change","perc.rich.change.ci.lb","perc.rich.change.ci.ub","CI95.perc.rich.change")] <- convert.log2equidist(newdat.full$logRR.richness,newdat.full$logRR.richness.ci.lb,newdat.full$logRR.richness.ci.ub)
newdat.full[,c("perc.yield.change","perc.yield.change.ci.lb","perc.yield.change.ci.ub","CI95.perc.yield.change")] <- convert.log2equidist(newdat.full$logRR.yield,newdat.full$logRR.yield.ci.lb,newdat.full$logRR.yield.ci.ub)

newdat.full <- rename.factor.levels(newdat.full)

write.csv(newdat.full[,c("LUI.range.level",  "Product",  "Species.Group",	"landuse_history",	"main_climate",
                        "n.richness", "perc.rich.change",  "CI95.perc.rich.change",
                        "n.yield", "perc.yield.change","CI95.perc.yield.change")],
          file=path2temp %+% "preds.full.csv",row.names=F)

