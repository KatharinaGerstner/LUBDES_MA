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
model <- Richness.MA.model[["Context"]]

newdat.richness <- expand.grid(BIOME = levels(modelDataRichness$BIOME),
                               Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness$n <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n[i] <- length(which(modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$BIOME %in% newdat.richness$BIOME[i]))
}

mm <- model.matrix(~Product + Species.Group + BIOME-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n>0)
newdat.richness <- newdat.richness[,c("Species.Group","Product","BIOME","n","logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")]

write.csv(newdat.richness,file=path2temp %+% "preds.richness.context.csv",row.names=F)
#print(xtable(newdat.richness, caption="Response ratios for the context model for richness and available evidence"),type="latex",include.rownames=F,size="\\fontsize{9pt}{10pt}\\selectfont")

############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["Context"]]

newdat.yield <- expand.grid(BIOME = levels(modelDataYield$BIOME),
                            Product = levels(modelDataYield$Product))

## count number of cases within groups
newdat.yield$n <- NA

for(i in 1:nrow(newdat.yield)){
  newdat.yield$n[i] <- length(which(modelDataYield$Product %in% newdat.yield$Product[i] & modelDataYield$BIOME %in% newdat.yield$BIOME[i]))
}

mm <- model.matrix(~Product + BIOME-1,data=newdat.yield)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.yield[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.yield <- subset(newdat.yield,n>0)
newdat.yield <- newdat.yield[,c("Product","BIOME","n","logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")]

write.csv(newdat.yield,file=path2temp %+% "preds.yield.context.csv",row.names=F)
#print(xtable(newdat.yield, caption="Response ratios for the context model for yield and available evidence"),type="latex",include.rownames=F)

save(newdat.richness,newdat.yield,file=path2temp %+% "newdat.context.Rdata")
############################################################################
### 09.2.3. Map predictions facetted by Product and LUI on top of biomes
############################################################################
load(path2temp %+% "newdat.context.Rdata") # newdat.richness,newdat.yield cf. module 09.2.2
load(path2temp %+% "biome_mapdata.Rdata") # biome_mapdata cf. module 07.1_biome_mapdata

for(SG in levels(newdat.richness$Species.Group)){
  newdat.SG <- subset(newdat.richness, n>1 & Species.Group==SG)
  for(P in levels(modelDataRichness$Product)){
    biome_mapdata$RR <- NA
    for(B in levels(modelDataRichness$BIOME)){
      temp <- exp(newdat.SG$logRR[newdat.SG$Product==P & newdat.SG$BIOME ==B])
      biome_mapdata$RR[biome_mapdata$BIOME == B] <- ifelse(length(temp)>0, temp, NA)
    }
    if(all(is.na(biome_mapdata$RR))) next
    p <- ggplot(biome_mapdata) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = RR)) +
      scale_fill_gradientn(limits=c(0,2),colours=c("#990000","#fc8d59","white","#1d91c0","#0c2c84"),breaks = c(0,1,2),guide="colourbar",na.value="grey90", name="Response ratio") + 
      scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90°W", "0", "90°E")) +
      scale_y_continuous(breaks=c(-30, 0, 30, 60), labels=c("30°S","0°", "30°N","60°N"),limits=c(-55,90)) +
      xlab("") + ylab("") +
      theme_lubdes()
    p 
    ggsave(path2temp %+% "RR_TopBiomes_" %+% SG %+% "_" %+% P %+% ".png", width=20, height=12, units="cm")
  }
}

for(P in levels(modelDataYield$Product)){
  biome_mapdata$RR <- NA
  for(B in levels(modelDataYield$BIOME)){
    temp <- exp(newdat.yield$logRR[newdat.yield$Product==P & newdat.yield$BIOME ==B])
    biome_mapdata$RR[biome_mapdata$BIOME == B] <- ifelse(length(temp)>0, temp, NA)
  }
  if(all(is.na(biome_mapdata$RR))) next
  p <- ggplot(biome_mapdata) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = RR)) +
    scale_fill_gradientn(limits=c(0,8.1),colours=c("red","white","#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0","#225ea8", "#0c2c84"),breaks = c(0,1,2,4,8),guide="colourbar",na.value="grey90", name="Response ratio") + 
    scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90°W", "0", "90°E")) +
    scale_y_continuous(breaks=c(-30, 0, 30, 60), labels=c("30°S","0°", "30°N","60°N"),limits=c(-55,90)) +
    xlab("") + ylab("") +
    theme_lubdes()
  p 
  ggsave(path2temp %+% "RR_TopBiomes_" %+% P %+% ".png", width=20, height=12, units="cm")
}


############################################################################
### 09.2.1. Predictions for richness
############################################################################
model <- Richness.MA.model[["Select"]]

newdat.richness <- expand.grid(BIOME = levels(modelDataRichness$BIOME),
                               LUI.range.level = levels(modelDataRichness$LUI.range.level),
                               Product = levels(modelDataRichness$Product),
                               Species.Group = levels(modelDataRichness$Species.Group)) 

## count number of cases within groups
newdat.richness$n <- NA

for(i in 1:nrow(newdat.richness)){
  newdat.richness$n[i] <- length(which(modelDataRichness$LUI.range.level %in% newdat.richness$LUI.range.level[i] & modelDataRichness$Product %in% newdat.richness$Product[i] & modelDataRichness$Species.Group %in% newdat.richness$Species.Group[i] & modelDataRichness$BIOME %in% newdat.richness$BIOME[i]))
}

mm <- model.matrix(~LUI.range.level + Product + Species.Group + BIOME + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:BIOME-1,data=newdat.richness)
mm <- mm[,colnames(mm) %in% rownames(model$b)] # remove colums without coeffs
preds <- predict.rma(model, newmods = mm)
newdat.richness[,c("logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

## remove levels with zero sample size
newdat.richness <- subset(newdat.richness,n>0)
newdat.richness <- newdat.richness[,c("Species.Group","Product","LUI.range.level","BIOME","n","logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")]

write.csv(newdat.richness,file=path2temp %+% "preds.richness.select.csv",row.names=F)
#print(xtable(newdat.richness, caption="Response ratios for the full model for richness and available evidence"),type="latex",include.rownames=F,size="\\fontsize{9pt}{10pt}\\selectfont")

############################################################################
### 09.2.2. Predictions for yield
############################################################################
model <- Yield.MA.model[["Select"]]

newdat.yield <- expand.grid(BIOME = levels(modelDataYield$BIOME),
                            LUI.range.level = levels(modelDataYield$LUI.range.level),
                            Product = levels(modelDataYield$Product))

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
newdat.yield <- subset(newdat.yield,n>0)
newdat.yield <- newdat.yield[,c("Product","LUI.range.level","BIOME","n","logRR","logRR.se","logRR.ci.lb","logRR.ci.ub")]

write.csv(newdat.yield,file=path2temp %+% "preds.yield.select.csv",row.names=F)
#print(xtable(newdat.yield, caption="Response ratios for the full model for yield and available evidence"),type="latex",include.rownames=F)

save(newdat.richness,newdat.yield,file=path2temp %+% "newdat.select.Rdata")
############################################################################
### 09.2.3. Map predictions facetted by Product and LUI on top of biomes
############################################################################
load(path2temp %+% "newdat.select.Rdata") # newdat.richness,newdat.yield cf. module 09.2.2
load(path2temp %+% "biome_mapdata.Rdata") # biome_mapdata cf. module 07.1_biome_mapdata

for(SG in levels(newdat.richness$Species.Group)){
  newdat.SG <- subset(newdat.richness, n>1 & Species.Group==SG)
  for(P in levels(modelDataRichness$Product)){
    for(LUI in levels(modelDataRichness$LUI.range.level)){
      biome_mapdata$RR <- NA
      for(B in levels(modelDataRichness$BIOME)){
        temp <- exp(newdat.SG$logRR[newdat.SG$Product==P & newdat.SG$LUI.range.level==LUI & newdat.SG$BIOME ==B])
        biome_mapdata$RR[biome_mapdata$BIOME == B] <- ifelse(length(temp)>0, temp, NA)
      }
      if(all(is.na(biome_mapdata$RR))) next
      p <- ggplot(biome_mapdata) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = RR)) +
        scale_fill_gradientn(limits=c(0,2),colours=c("#990000","#fc8d59","white","#1d91c0","#0c2c84"),breaks = c(0,1,2),guide="colourbar",na.value="grey90", name="Response ratio") + 
        scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90°W", "0", "90°E")) +
        scale_y_continuous(breaks=c(-30, 0, 30, 60), labels=c("30°S","0°", "30°N","60°N"),limits=c(-55,90)) +
        xlab("") + ylab("") +
        theme_lubdes()
      p 
      ggsave(path2temp %+% "RR_TopBiomes_" %+% SG %+% "_" %+% P %+% "_" %+% LUI %+% ".png", width=20, height=12, units="cm")
    }
  }
}

for(P in levels(modelDataYield$Product)){
  for(LUI in levels(modelDataYield$LUI.range.level)){
    biome_mapdata$RR <- NA
    for(B in levels(modelDataYield$BIOME)){
      temp <- exp(newdat.yield$logRR[newdat.yield$Product==P & newdat.yield$LUI.range.level==LUI & newdat.yield$BIOME ==B])
      biome_mapdata$RR[biome_mapdata$BIOME == B] <- ifelse(length(temp)>0, temp, NA)
    }
    if(all(is.na(biome_mapdata$RR))) next
    p <- ggplot(biome_mapdata) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = RR)) +
      scale_fill_gradientn(limits=c(0,8.1),colours=c("red","white","#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0","#225ea8", "#0c2c84"),breaks = c(0,1,2,4,8),guide="colourbar",na.value="grey90", name="Response ratio") + 
      scale_x_continuous(breaks=c(-90, 0, 90), labels=c("90°W", "0", "90°E")) +
      scale_y_continuous(breaks=c(-30, 0, 30, 60), labels=c("30°S","0°", "30°N","60°N"),limits=c(-55,90)) +
      xlab("") + ylab("") +
      theme_lubdes()
    p 
    ggsave(path2temp %+% "RR_TopBiomes_" %+% P %+% "_" %+% LUI %+% ".png", width=20, height=12, units="cm")
  }
}


