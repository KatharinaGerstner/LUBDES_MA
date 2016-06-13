############################################################################
### Purpose of this skript module 09.2 is to:
###
### 09.2.1/2 Predict effects of LUI on richness/yield using the full model
### 09.2.2/2 Map Biomes with high risk of biodiversity loss/high potential of yield gains facetted using product and LUI.range.level
### 09.2.3. LISA analysis for different species groups 
### 09.2.4. ...
###
### Authors: KG, ...
############################################################################

############################################################################
### 09.2.1. Predictions for richness
############################################################################
model <- Richness.MA.model[["Select"]]

newdat <- expand.grid(LUI.range.level = levels(modelDataRichness$LUI.range.level),
                      Product = levels(modelDataRichness$Product),
                      Species.Group = levels(modelDataRichness$Species.Group), 
                      BIOME = levels(modelDataRichness$BIOME))

## count number of cases within groups
newdat$n <- NA

for(i in 1:nrow(newdat)){
  newdat$n[i] <- length(which(modelDataRichness$LUI.range.level %in% newdat$LUI.range.level[i] & modelDataRichness$Product %in% newdat$Product[i] & modelDataRichness$Species.Group %in% newdat$Species.Group[i] & modelDataRichness$BIOME %in% newdat$BIOME[i]))
}

mm <- model.matrix(~LUI.range.level + Product + Species.Group + BIOME + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:BIOME-1,data=newdat)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat <- subset(newdat,n>0)

write.csv(newdat,file=path2temp %+% "preds.richness.csv",row.names=F)


############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["Select"]]

newdat <- expand.grid(LUI.range.level = levels(modelDataYield$LUI.range.level),
                      Product = levels(modelDataYield$Product),
                      BIOME = levels(modelDataYield$BIOME))

## count number of cases within groups
newdat$n <- NA

for(i in 1:nrow(newdat)){
  newdat$n[i] <- length(which(modelDataYield$LUI.range.level %in% newdat$LUI.range.level[i] & modelDataYield$Product %in% newdat$Product[i] & modelDataYield$BIOME %in% newdat$BIOME[i]))
}

mm <- model.matrix(~LUI.range.level + Product + BIOME + LUI.range.level:Product + LUI.range.level:BIOME-1,data=newdat)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat <- subset(newdat,n>0)

write.csv(newdat,file=path2temp %+% "preds.yield.csv",row.names=F)
