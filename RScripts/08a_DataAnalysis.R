############################################################################
### Purpose of this skript module 07 is to:
###
### 07.1. Prepare data analysis
### 07.2. Analysis without moderators
### 07.3. Analysis with moderators
### 07.4. Analysis with moderators for no LU vs low/medium/high LU
### 07.5. multivariate analysis with and without moderators (not yet working)
###
### General comments:
### * fit meta-analytic multivariate/multilevel fixed- and random/mixed-effects models with or
###   without moderators via linear (mixed-effects) models using rma.mv 
### * Viechtbauer 2015 p 189: the random argument can also contain one (and only one!) formula of the form ~ inner | outer . 
###   Effects or outcomes with different values/levels of the outer grouping variable/factor are assumed to be 
###   independent, while effects or outcomes with the same value/level of the outer grouping variable/factor share 
###   correlated random effects corresponding to the levels of the inner grouping variable/factor. The struct 
###   argument is used to specify the variance structure corresponding to the inner variable/factor. With 
###   struct="CS", a compound symmetric structure is assumed (i.e., a single variance component tau? corresponding
###   to all values/levels of the inner variable/factor and a single correlation coefficient rho for the correlation
###   between different values/levels). 
###
### Authors: KG, MB, SK ...
############################################################################

###########################################################################
### define list of moderators
###########################################################################

mods.Richness <- c("Species.Group","LUI.range.level","Product","BIOME", paste("LUI.range.level:",c("Species.Group","Product","BIOME"),sep=""))
modelDataRichness <- ES.frame.richness[,c('Log.RR','Log.RR.Var',paste(mods.Richness[1:4],sep=","),
                                          'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]
# modelDataRichness <- na.omit(modelDataRichness)

mods.Yield <- c("LUI.range.level","Product","BIOME", paste("LUI.range.level:",c("Product","BIOME"),sep=""))
modelDataYield <- ES.frame.yield[,c('Log.RR','Log.RR.Var',paste(mods.Yield[1:3],sep=","),
                                    'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]
# modelDataYield <- na.omit(modelDataYield)

### set reference level to the most common level, i.e.
### for richness: timber-non-woody.plants-temp.boreal.forest
### for yield: timber-temp.boreal.forest
setRefToMostCommonLevel <- function(f) { 
  f <- as.factor(f) 
  t <- table(f)
  relevel(f,ref=as.integer(which(t>=max(t))[[1]]))
}
for(x in c("Species.Group","Product","BIOME")){
  modelDataRichness[,x] <- setRefToMostCommonLevel(modelDataRichness[,x])
}
for(x in c("Product","BIOME")){
  modelDataYield[,x] <- setRefToMostCommonLevel(modelDataYield[,x])
}

###########################################################################
### store models in a list
###########################################################################
Richness.MA.model <- Yield.MA.model <- vector("list", length=4)
names(Richness.MA.model) <- names(Yield.MA.model) <- c("None","LUI","Full","Select")

###########################################################################
### Function for model fitting using rma.mv
###########################################################################
rma.mv.func <- function(df, moderators, fit.method)
{
  mods.formula <- as.formula("~" %+% paste(moderators,collapse="+"))
  fm <- try(rma.mv(yi=Log.RR, V=M.matrix(df)+diag(Log.RR.Var), 
                  mods=mods.formula, 
                  random = list(~1|Study.Case, ~1|Study.ID),
                  slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                  method=fit.method, tdist=FALSE, level=95, digits=4,data=df))
  
  if(inherits(fm, "try-error")){
    fm <- rma.mv(yi=Log.RR, V=M.matrix(df)+diag(Log.RR.Var), 
                 mods=mods.formula, 
                 random = list(~1|Study.Case, ~1|Study.ID),
                 slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                 method=fit.method, tdist=FALSE, level=95, digits=4,data=df,
                 control=list(optimizer="optim", optmethod="BFGS"))
    
  }
  return(fm)
}

###########################################################################
### Analysis for richness
###########################################################################
Richness.MA.model[["None"]] <- rma.mv.func(df=modelDataRichness, moderators=c(1), fit.method="REML")
Richness.MA.model[["LUI"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level"), fit.method="REML")
Richness.MA.model[["Full"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level", "Product", "Species.Group", "BIOME", "LUI.range.level:Product", "LUI.range.level:Species.Group", "LUI.range.level:BIOME"), fit.method="REML")
model2select <- try(rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(modelDataRichness$Log.RR.Var), 
                       mods=~LUI.range.level + Product + Species.Group + BIOME + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:BIOME,
                       random = list(~1|Study.Case, ~1|Study.ID),
                       slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                       method="ML", tdist=FALSE, level=95, digits=4,data=modelDataRichness))

if(inherits(model2select, "try-error")){
  model2select <- rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(modelDataRichness$Log.RR.Var), 
                         mods=~LUI.range.level + Product + Species.Group + BIOME + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:BIOME,
                         random = list(~1|Study.Case, ~1|Study.ID),
                         slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                         method="ML", tdist=FALSE, level=95, digits=4,data=modelDataRichness,
                         control=list(optimizer="optim", optmethod="BFGS"))
}

              
model.select <- RMASelect(model2select)
Richness.MA.model[["Select"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,model.select$call$mods[[2]]), fit.method="REML")

###########################################################################
### Analysis for yield
###########################################################################
Yield.MA.model[["None"]] <- rma.mv.func(df=modelDataYield, moderators=c(1), fit.method="REML")
Yield.MA.model[["LUI"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level"), fit.method="REML")
Yield.MA.model[["Full"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level", "Product", "BIOME", "LUI.range.level:Product", "LUI.range.level:BIOME"), fit.method="REML")
model2select <- try(rma.mv(yi=Log.RR, V=M.matrix(modelDataYield)+diag(modelDataYield$Log.RR.Var), 
                           mods=~LUI.range.level + Product + BIOME + LUI.range.level:Product + LUI.range.level:BIOME,
                           random = list(~1|Study.Case, ~1|Study.ID),
                           slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                           method="ML", tdist=FALSE, level=95, digits=4,data=modelDataYield))

if(inherits(model2select, "try-error")){
  model2select <- rma.mv(yi=Log.RR, V=M.matrix(modelDataYield)+diag(modelDataYield$Log.RR.Var), 
                         mods=~LUI.range.level + Product + BIOME + LUI.range.level:Product + LUI.range.level:BIOME,
                         random = list(~1|Study.Case, ~1|Study.ID),
                         slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                         method="ML", tdist=FALSE, level=95, digits=4,data=modelDataYield,
                         control=list(optimizer="optim", optmethod="BFGS"))
}

model.select <- RMASelect(model2select)
Yield.MA.model[["Select"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,model.select$call$mods[[2]]), fit.method="REML")

###########################################################################
### save models as Rdata
###########################################################################
save(Richness.MA.model,Yield.MA.model,modelDataRichness,modelDataYield,file=path2temp %+% "Models.Rdata")

###########################################################################
### extract fit statistics
###########################################################################
fit.tab.richness <- data.frame(model=names(Richness.MA.model),logLik=NA, deviance=NA, AIC=NA, BIC=NA, AICc=NA, R2.LMM.m=NA,R2.LMM.c=NA)
fit.tab.yield <- data.frame(model=names(Yield.MA.model),logLik=NA, deviance=NA, AIC=NA, BIC=NA, AICc=NA, R2.LMM.m=NA,R2.LMM.c=NA)
for(i in 1:4){
  var.fixed <- lapply(Richness.MA.model,function(x) {ifelse(length(coef(x))==1,0,var(coef(x)))})
  fit.tab.richness[i,2:6] <- t(fitstats.rma(Richness.MA.model[[i]]))
#  fit.tab.richness$R2.GH <- lapply(Richness.MA.model,function(x) {1-var(residuals(x))/var(ES.frame.richness$Log.RR)})
  fit.tab.richness$R2.LMM.m <- unlist(lapply(Richness.MA.model,function(x) {var.fixed[[i]]/(var.fixed[[i]]+x$sigma2[1]+x$sigma2[2]+var(residuals(x)))}))
  fit.tab.richness$R2.LMM.c <- unlist(lapply(Richness.MA.model,function(x) {(var.fixed[[i]]+x$sigma2[1]+x$sigma2[2])/(var.fixed[[i]]+x$sigma2[1]+x$sigma2[2]+var(residuals(x)))}))
  
  var.fixed <- lapply(Yield.MA.model,function(x) {ifelse(length(coef(x))==1,0,var(coef(x)))})
  fit.tab.yield[i,2:6] <- t(fitstats.rma(Yield.MA.model[[i]]))
  fit.tab.yield$R2.LMM.m <- unlist(lapply(Yield.MA.model,function(x) {var.fixed[[i]]/(var.fixed[[i]]+x$sigma2[1]+x$sigma2[2]+var(residuals(x)))}))
  fit.tab.yield$R2.LMM.c <- unlist(lapply(Yield.MA.model,function(x) {(var.fixed[[i]]+x$sigma2[1]+x$sigma2[2])/(var.fixed[[i]]+x$sigma2[1]+x$sigma2[2]+var(residuals(x)))}))
}

write.csv(fit.tab.richness,file=path2temp %+% "fit.tab.richness.csv",row.names=F)
write.csv(fit.tab.yield,file=path2temp %+% "fit.tab.yield.csv",row.names=F)


# ############################################################################
# ### 07.4. Analysis with moderators for no LU vs low/medium/high LU
# ############################################################################
# 
# Richness.MA.model.noLU <- list()
# 
# ### Analysis without moderators
# Richness.MA.fit.noLU <- rma.mv(yi=Richness.Log.RR, V=Richness.Log.RR.Var, mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.noLU)
# Richness.MA.model.noLU[["None"]] <- Richness.MA.fit.noLU
# 
# ### Store results in table
# MA.coeffs.noLU <- data.frame(Moderator="None",levels=1,mean.Richness=Richness.MA.fit.noLU$b,se.Richness=Richness.MA.fit.noLU$se)
# 
# # define list of moderators
# moderator.list <- c("Product","Product:High.LUI","Land.use...land.cover","Species.Group","BIOME")
# 
# # run analysis
# for(mods in moderator.list){
#   print(mods)
#   ### fit model 
# #  attach(ES.frame.noLU)
#   Richness.MA.fit.noLU <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.noLU.richness),silent=T)
#   Richness.MA.model.noLU[[mods]] <- Richness.MA.fit.noLU
#   ### catch errors
#   if(class(Richness.MA.fit.noLU)[1]=="try-error") {
#     geterrmessage()
#     Richness.MA.fit.noLU <- data.frame(b=NA,se=NA)
#   }
# #  detach(ES.frame.noLU)
# 
#   ### tabularize model parameters  
#   ifelse(mods=="Product:High.LUI", levels <- unlist(lapply(strsplit(unlist(lapply(strsplit(rownames(Richness.MA.fit.noLU$b),"Product"),function(x)x[[2]])),"High.LUI"),function(y) paste(y[1],y[2],sep=""))), levels <- unlist(lapply(strsplit(rownames(Richness.MA.fit.noLU$b),mods),function(x){x[[2]]})))
#   MA.coeffs.noLU <- rbind(MA.coeffs.noLU,data.frame(Moderator=rep(mods,length(Richness.MA.fit.noLU$b)),
#                                                     levels=levels,
#                                                     mean.Richness=Richness.MA.fit.noLU$b,
#                                                     se.Richness=Richness.MA.fit.noLU$se))
# }
# print(MA.coeffs.noLU)
# 


###########################################################################
### Resterampe
### use full model with all levels simultaneously or separate models for each level

# 
# 
# ### run analysis for categorical moderators
# for(mods in moderator.list.cat){
#   
#   print(mods)
# 
#   ### fit model
# #  attach(ES.frame)
#   Richness.MA.fit <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.richness), silent=T)
#   Richness.MA.model[[mods]] <- Richness.MA.fit
#   ### catch errors
#   if(class(Richness.MA.fit)=="try-error") {
#     print(geterrmessage())
#     Richness.MA.fit <- data.frame(b=NA,se=NA)
#   }
# 
#   ### fit model
#   Yield.MA.fit <- try(rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.yield),silent=T)
#   Yield.MA.model[[mods]] <- Yield.MA.fit
#   ### catch errors
#   if(class(Yield.MA.fit)=="try-error") {
#     print(geterrmessage())
#     Yield.MA.fit <- data.frame(b=NA,se=NA)
#   }
#   
# #  detach(ES.frame)
# 
#   ### tabularize model parameters
#   MA.coeffs.cat <- rbind(MA.coeffs.cat,data.frame(Moderator=rep(mods,length(Richness.MA.fit$b)),levels=unlist(lapply(strsplit(rownames(Richness.MA.fit$b),mods),function(x){x[[2]]})),mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se))
# }
# print(MA.coeffs.cat)
# 
# ### run analysis for continuous moderators
# 
# for(mods in moderator.list.cont){
#   
#   print(mods)
# #  attach(ES.frame)
#   
#   ### fit model
#   Richness.MA.fit <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.richness),silent=T)
#   Richness.MA.model[["None"]] <- Richness.MA.fit
#   ### catch errors
#   if(class(Richness.MA.fit)=="try-error") {
#     print(geterrmessage())
#     Richness.MA.fit <- data.frame(b=c(NA,NA),se=c(NA,NA))
#   }
# 
#   ### fit model
#   Yield.MA.fit <- try(rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.yield),silent=T)
#   Yield.MA.model[[mods]] <- Yield.MA.fit
#   ### catch errors
#   if(class(Yield.MA.fit)=="try-error") {
#     print(geterrmessage())
#     Yield.MA.fit <- data.frame(b=c(NA,NA),se=c(NA,NA))
#   }
#   
# #  detach(ES.frame)
# 
#   ### tabularize model parameters
#   MA.coeffs.cont <- rbind(MA.coeffs.cont,data.frame(Moderator=mods, Richness.intercept=Richness.MA.fit$b[1], Richness.slope=Richness.MA.fit$b[2], Richness.se.intercept=Richness.MA.fit$se[1], Richness.se.slope=Richness.MA.fit$se[2], Yield.intercept=Yield.MA.fit$b[1], Yield.slope=Yield.MA.fit$b[2], Yield.se.intercept=Yield.MA.fit$se[1], Yield.se.slope=Yield.MA.fit$se[2]))
#   preds.richness[[mods]] <- ifelse(all(is.na(Richness.MA.fit)),list(data.frame(pred = NA, se = NA, ci.lb = NA, ci.ub = NA, cr.lb = NA, cr.ub = NA)), list(predict.rma(Richness.MA.fit))) 
#   preds.yield[[mods]] <- ifelse(all(is.na(Yield.MA.fit)), list(data.frame(pred = NA, se = NA, ci.lb = NA, ci.ub = NA, cr.lb = NA, cr.ub = NA)), list(predict.rma(Yield.MA.fit))) 
# }
# print(MA.coeffs.cont)

### LMM.MA.fit function
# LMM.MA.fit <- function(yi,vi,mods,slab,inner2,outer2){
# # TO DO: calculate variance-covariance matrix, esp covariance for between LUI comparisons 
# #  VCov_matrix <- 
#   inner3<-inner2
#   outer3<-outer2
#   rma.mv.fit <- rma.mv(yi=yi, V=vi, mods = mods, random = ~factor(inner3)|factor(outer3), struct="CS", data=ES.frame, slab=slab,method="REML", tdist=FALSE, level=95, digits=4)
#   return(rma.mv.fit)
# }

### implement or not the variance-covariance matrix: 
# is the information content of the covariance of the ES worth to include at the harm of model robustness?
# theoretically: as was suggested by Wolfgang Viechtbauer
# empirically: calculate correlation matrix of ES for the entire data set and check whether there is a structure, i.e. correlations of ES within one study.case are generally higher
# if yes (how to determine?): apply theoretic calculations of covariance
# if no: there is no need to account for non-independen as it comes with increasing number of degrees of freedoms and makes the whole model less robust 
# data.frame(data$Study.Case,data$LUI.comparison)

## Model selection for MA models
## Account for scale dependency in biodiversity data
## Effects of various yield indices on the effect sizes (RR vs SMD)

# ############################################################################
# ### 07.1. Prepare data analysis
# ############################################################################
# 
# ES.frame <- subset(ES.frame, Richness.Log.RR.Var>0 & Yield.Log.RR.Var>0) # restrict analysis to study cases with positive variances
# 
# ### Remove pseudo-replicates
# ES.frame.richness <- ES.frame[!duplicated(ES.frame[,c("Study.ID","Case.ID","LUI.range.level","Species.Group")]),]
# ES.frame.yield <- ES.frame[!duplicated(ES.frame[,c("Study.ID","LUI.range.level","Product")]),]
# 
# # ES.frame.noLU <- subset(ES.frame.noLU, Richness.Log.RR.Var>0) # restrict analysis to study cases with positive variances
# # ES.frame.noLU.richness <- ES.frame.noLU[!duplicated(ES.frame.noLU[,c("Study.Case","High.LUI","Species.Group")]),]
# 
# ###########################################################################
# ### remove redundant cases, cases low-high are removed if all three comparisons are available
# ##########################################################################
# 
# for(x in unique(ES.frame$Study.Case)){
#   subset.richness <- subset(ES.frame.richness,Study.Case==x)
#   subset.yield <- subset(ES.frame.yield,Study.Case==x)
#   ## remove redundant cases
#   if (all(c("low-medium","low-high","medium-high") %in% unique(subset.richness$LUI.range.level))){
#     ES.frame.richness <- ES.frame.richness[!(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="low-high"),]
#   }
#   if (all(c("low-medium","low-high","medium-high") %in% unique(subset.yield$LUI.range.level))){
#     ES.frame.yield <- ES.frame.yield[!(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="low-high"),]
#   }
# }  
# 
# # ###########################################################################
# # ### Variance-Covariance Matrix
# # ###########################################################################
# # Var.Richness <- diag(ES.frame.richness$Richness.Log.RR.Var)
# # Var.Yield <- diag(ES.frame.yield$Yield.Log.RR.Var)
# # ## calculate covariance of shared control and store it on the off-diagonal 
# # ## TO DO: calculate covariance for cases with (low-medium, medium-high), (low-low, low-medium, low-high), (medium-medium, medium- high), (high-high, medium-high) -> ask Wolfagang Viechtbauer for advice
# # # for(x in unique(ES.frame$Study.Case)){
# # #   if (all(c("low-medium","medium-high") %in% unique(subset.richness$LUI.range.level))){
# # #     row.1 <- which(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="low-medium")
# # #     col.1 <- which(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="low-high")
# # #     Var.Richness[row.1,col.1] <- Var.Richness[col.1,row.1] <- subset.richness$Richness.SD.Low[which(subset.richness$LUI.range.level=="low-medium")]^2/(subset.richness$Richness.N.Low[which(subset.richness$LUI.range.level=="low-medium")]*subset.richness$Richness.Mean.Low[which(subset.richness$LUI.range.level=="low-medium")]) ## sd_c?/(n_c*mean(X_c)?), cf. Lajeunesse (2011) Ecology
# # #   }
# # #   if (all(c("low-medium","medium-high") %in% unique(subset.yield$LUI.range.level))){
# # #     row.1 <- which(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="low-medium")
# # #     col.1 <- which(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="low-high")
# # #     Var.Yield[row.1,col.1] <- Var.Yield[col.1,row.1] <- subset.yield$Yield.SD.Low[which(subset.yield$LUI.range.level=="low-medium")]^2/(subset.yield$Yield.N.Low[which(subset.yield$LUI.range.level=="low-medium")]*subset.yield$Yield.Mean.Low[which(subset.yield$LUI.range.level=="low-medium")]) ## sd_c?/(n_c*mean(X_c)?), cf. Lajeunesse (2011) Ecology
# # #   }
# # # }
# 
# ############################################################################
# ### 07.2. Analysis without moderators
# ############################################################################
# 
# Richness.MA.model[["None"]] <- rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(Log.RR.Var), 
#                                       mods=~1, 
#                                       random = list(~1|Study.Case, ~1|Study.ID),
#                                       slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
#                                       method="REML", tdist=FALSE, level=95, digits=4,data=modelDataRichness)
# 
# Yield.MA.model[["None"]] <- rma.mv(yi=Log.RR,V=M.matrix(modelDataYield)+diag(Log.RR.Var),mods=~1, random = ~1|Study.Case, struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=modelDataYield)
# 
# ############################################################################
# ### 07.3. Analysis with moderators
# ############################################################################
# Richness.MA.model[["LUI"]] <- rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(Log.RR.Var), 
#                                      mods=~LUI.range.level-1, 
#                                      random = list(~1|Study.Case, ~1|Study.ID),
#                                      slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
#                                      method="REML", tdist=FALSE, level=95, digits=4,data=modelDataRichness)
# 
# Yield.MA.model[["LUI"]] <- rma.mv(yi=Log.RR,V=M.matrix(modelDataYield)+diag(Log.RR.Var),
#                                   mods=~LUI.range.level-1, 
#                                   random = list(~1|Study.Case, ~1|Study.ID),
#                                   slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
#                                   method="REML", tdist=FALSE, level=95, digits=4,data=modelDataYield,
#                                   control=list(optimizer="optim", optmethod="BFGS")) # no convergence using default optimization algorithm
# 
# #mods.formula <- as.formula(paste("~",paste(mods.Richness,collapse="+")))
# Richness.MA.model[["Full"]] <- rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(modelDataRichness$Log.RR.Var), 
#                                       mods=~LUI.range.level * (Species.Group + Product + BIOME)-1,
#                                       random = list(~1|Study.Case, ~1|Study.ID),
#                                       slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
#                                       method="REML", tdist=FALSE, level=95, digits=4,data=modelDataRichness)
# 
# ## use ML instead of REML for model selection
# model2select <- rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(modelDataRichness$Log.RR.Var), 
#                        mods=~LUI.range.level + Product + Species.Group + BIOME + LUI.range.level:Product + LUI.range.level:Species.Group + LUI.range.level:BIOME,
#                        random = list(~1|Study.Case, ~1|Study.ID),
#                        slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
#                        method="ML", tdist=FALSE, level=95, digits=4,data=modelDataRichness)
# 
# model.select <- RMASelect(model2select)
# Richness.MA.model[["Select"]] <- rma.mv(yi=Log.RR, V=M.matrix(modelDataRichness)+diag(modelDataRichness$Log.RR.Var), 
#                                         mods=as.formula("~" %+% model.select$call$mods[2] %+% "-1"),
#                                         random = list(~1|Study.Case, ~1|Study.ID),
#                                         slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
#                                         method="REML", tdist=FALSE, level=95, digits=4,data=modelDataRichness)
# 
# Yield.MA.model[["Full"]] <- rma.mv(yi=Log.RR,V=M.matrix(modelDataYield)+diag(modelDataYield$Log.RR.Var),
#                                    mods=~-1+LUI.range.level * (Product + BIOME),
#                                    random = list(~1|Study.Case, ~1|Study.ID),
#                                    method="REML", tdist=FALSE, level=95, digits=4,data=modelDataYield)
# 
# ## use ML instead of REML for model selection
# model2select <- rma.mv(yi=Log.RR,V=M.matrix(modelDataYield)+diag(modelDataYield$Log.RR.Var),
#                        mods=~LUI.range.level + Product + BIOME + LUI.range.level:Product + LUI.range.level:BIOME,
#                        random = list(~1|Study.Case, ~1|Study.ID),
#                        method="ML", tdist=FALSE, level=95, digits=4,data=modelDataYield)
# 
# model.select <- RMASelect(model2select)
# 
# Yield.MA.model[["Select"]] <- rma.mv(yi=Log.RR,V=M.matrix(modelDataYield)+diag(modelDataYield$Log.RR.Var),
#                                      mods=as.formula("~" %+% model.select$call$mods[2] %+% "-1"),
#                                      random = list(~1|Study.Case, ~1|Study.ID),
#                                      method="REML", tdist=FALSE, level=95, digits=4,data=modelDataYield)
