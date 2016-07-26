############################################################################
### 09.2.1. Predictions for richness
############################################################################
model <- Richness.MA.model[["Select"]]

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
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["Select"]]

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

newdat.select <- join_all(list(newdat.richness,newdat.yield),type="full")
newdat.select$n.yield[is.na(newdat.select$n.yield)] <- 0
newdat.select$n.richness[is.na(newdat.select$n.richness)] <- 0
newdat.select$CI95.richness <- "[" %+% round(newdat.select$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat.select$logRR.richness.ci.ub,digits=2) %+% "]"
newdat.select$CI95.yield <- "[" %+% round(newdat.select$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat.select$logRR.yield.ci.ub,digits=2) %+% "]" 

write.csv(newdat.select[,c("LUI.range.level",  "Product",	"Species.Group",	"landuse_history",	"main_climate",
                           "n.richness", "logRR.richness",  "logRR.richness.se", "CI95.richness",
                           "n.yield", "logRR.yield",  "logRR.yield.se", "CI95.yield")],file=path2temp %+% "preds.select.csv",row.names=F)
#print(xtable(newdat.select, caption="Response ratios for the selected model and available evidence"),type="latex",include.rownames=F)
