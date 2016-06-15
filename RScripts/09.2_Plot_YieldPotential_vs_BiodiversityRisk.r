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

newdat.richness <- expand.grid(LUI.range.level = levels(modelDataRichness$LUI.range.level),
                      Product = levels(modelDataRichness$Product),
                      Species.Group = levels(modelDataRichness$Species.Group), 
                      BIOME = levels(modelDataRichness$BIOME))

## count number of cases within groups
newdat.richness$n <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n[i] <- length(which(modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i] & modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$BIOME %in% newdat.richness$BIOME[i]))
  print(which(modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i] & modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$BIOME %in% newdat.richness$BIOME[i]))
}

mm <- model.matrix(~LUI.range.level + Product + Species.Group + BIOME + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:BIOME-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n>3)

write.csv(newdat.richness,file=path2temp %+% "preds.richness.csv",row.names=F)

############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["Select"]]

newdat.yield <- expand.grid(LUI.range.level = levels(modelDataYield$LUI.range.level),
                      Product = levels(modelDataYield$Product),
                      BIOME = levels(modelDataYield$BIOME))

## count number of cases within groups
newdat.yield$n <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n[i] <- length(which(modelDataYield$LUI.range.level %in% newdat.yield$LUI.range.level[i] & modelDataYield$Product %in% newdat.yield$Product[i] & modelDataYield$BIOME %in% newdat.yield$BIOME[i]))
}

mm <- model.matrix(~LUI.range.level + Product + BIOME + LUI.range.level:Product + LUI.range.level:BIOME-1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n>3)

write.csv(newdat.yield,file=path2temp %+% "preds.yield.csv",row.names=F)

save(newdat.richness,newdat.yield,file=path2temp %+% "newdat.Rdata")
############################################################################
### 09.2.3. Map predictions facetted by Product and LUI on top of biomes
############################################################################
load(path2temp %+% "newdat.Rdata") # newdat.richness,newdat.yield cf. module 09.2.2
load(path2temp %+% "biome_mapdata.Rdata") # biome_mapdata cf. module 07.1_biome_map

biome_mapdata_logRR.richness <- biome_mapdata[rep(seq_len(nrow(biome_mapdata)), times=nrow(newdat.richness)),]
biome_mapdata_logRR.richness$LUI.range.level <- rep(newdat.richness$LUI.range.level,each=nrow(biome_mapdata))
,"Product","Species.Group")] <- rep(newdat.richness[,c("LUI.range.level","Product","Species.Group")],each=nrow(biome_mapdata))
for(i in levels(modelData$Product)){
  for(j in levels(modelData$LUI.range.level)){
    for(k %in% levels(modelData$BIOME)){
      biome_mapdata[which(biome_mapdata$id == modelData$BIOME),paste(i,j,sep="_")] <- newdat$logRR[newdat$Product==i & newdat$LUI.range.level==j & newdat$BIOME==k]
    }
  }
}
