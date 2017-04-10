############################################################################
### Purpose of this skript module 08 is to:
###
### 08.1. define list of moderators
### 08.2. Fit models for richness and yield
### 08.3. extract fit statistics
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
### 08.1. define list of moderators
###########################################################################

mods.Richness <- c("Species.Group","LUI.range.level","Product","landuse_history", "main_climate", paste("LUI.range.level:",c("Species.Group","Product","landuse_history", "main_climate"),sep=""))
modelDataRichness <- ES.frame.richness[,c('Log.RR','Log.RR.Var',paste(mods.Richness[1:5],sep=","),
                                          'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]
# modelDataRichness <- na.omit(modelDataRichness)

mods.Yield <- c("LUI.range.level","Product","landuse_history", "main_climate", paste("LUI.range.level:",c("Product", "main_climate"),sep=""))
modelDataYield <- ES.frame.yield[,c('Log.RR','Log.RR.Var',paste(mods.Yield[1:4],sep=","),
                                    'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]

###########################################################################
### 08.2. Fit models for richness and yield
###########################################################################

## set reference level to the most common level, i.e.
## for richness: timber-non-woody.plants-temp.boreal.forest
## for yield: timber-temp.boreal.forest
setRefToMostCommonLevel <- function(f) {
  f <- as.factor(f)
  t <- table(f)
  relevel(f,ref=as.integer(which(t>=max(t))[[1]]))
}
for(x in c("Species.Group","Product", "landuse_history", "main_climate")){
  modelDataRichness[,x] <- setRefToMostCommonLevel(modelDataRichness[,x])
}
for(x in c("Product", "landuse_history", "main_climate")){
  modelDataYield[,x] <- setRefToMostCommonLevel(modelDataYield[,x])
}

### Function for model fitting using rma.mv
rma.mv.func <- function(df, moderators, fit.method)
{
  mods.formula <- as.formula("~" %+% paste(moderators,collapse="+"))
  fm <- try(rma.mv(yi=Log.RR, V=M.matrix(df)+diag(Log.RR.Var), 
                  mods=mods.formula, 
                  random = list(~1|Study.Case, ~1|Study.ID),
#                  random = list(~1|Study.ID/Study.Case), # results are the same compared to include two random effects separately
                  slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                  method=fit.method, tdist=FALSE, level=95, digits=4,data=df))
  
  if(inherits(fm, "try-error")){
    fm <- rma.mv(yi=Log.RR, V=M.matrix(df)+diag(Log.RR.Var), 
                 mods=mods.formula, 
                 random = list(~1|Study.Case, ~1|Study.ID),
#                random = list(~1|Study.ID/Study.Case), # results are the same compared to include two random effects separately
                 slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                 method=fit.method, tdist=FALSE, level=95, digits=4,data=df,
                 control=list(optimizer="optim", optmethod="BFGS"))
    
  }
  return(fm)
}


### store models in a list
Richness.MA.model <- vector("list", length=9)
Yield.MA.model <- vector("list", length=5)
names(Richness.MA.model) <- c("None","LUI","LUI.SGP","LUI.SG","LUI.P","SGP","SG","P","Full")
names(Yield.MA.model) <- c("None","LUI","LUI.P","P","Full")

### Analysis for richness
Richness.MA.model[["None"]] <- rma.mv.func(df=modelDataRichness, moderators=c(1), fit.method="REML")
Richness.MA.model[["LUI"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level"), fit.method="REML")
Richness.MA.model[["LUI.SGP"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level","Product", "Species.Group", "LUI.range.level:Product", "LUI.range.level:Species.Group", "Species.Group:Product"), fit.method="REML")
Richness.MA.model[["LUI.SG"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level","Species.Group", "LUI.range.level:Species.Group"), fit.method="REML")
Richness.MA.model[["LUI.P"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level","Product", "LUI.range.level:Product"), fit.method="REML")
Richness.MA.model[["SGP"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"Product", "Species.Group", "Species.Group:Product"), fit.method="REML")
Richness.MA.model[["SG"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"Species.Group"), fit.method="REML")
Richness.MA.model[["P"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"Product"), fit.method="REML")
Richness.MA.model[["Full"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level", "Product", "Species.Group", "landuse_history","main_climate", "LUI.range.level:Product", "LUI.range.level:Species.Group","LUI.range.level:landuse_history","LUI.range.level:main_climate","Species.Group:Product"), fit.method="REML")

### Analysis for yield
Yield.MA.model[["None"]] <- rma.mv.func(df=modelDataYield, moderators=c(1), fit.method="REML")
Yield.MA.model[["LUI"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level"), fit.method="REML")
Yield.MA.model[["LUI.P"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level", "Product", "LUI.range.level:Product"), fit.method="REML")
Yield.MA.model[["P"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"Product"), fit.method="REML")

Yield.MA.model[["Full"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level", "Product", "landuse_history","main_climate", "LUI.range.level:Product","LUI.range.level:landuse_history","LUI.range.level:main_climate"), fit.method="REML")

### for AIC comparisons
### Zuur et al. (2009; PAGE 122) suggest that "To compare models with nested fixed effects (but with the same random structure), ML estimation must be used and not REML."
### Faraway's (2006) Extending the linear model with R (p. 156): "The reason is that REML estimates the random effects by considering linear combinations of the data that remove the fixed effects. If these fixed effects are changed, the likelihoods of the two models will not be directly comparable."
### store models in a list
Richness.MA.model.ML <- vector("list", length=9)
Yield.MA.model.ML <- vector("list", length=5)
names(Richness.MA.model.ML) <- c("None","LUI","LUI.SGP","LUI.SG","LUI.P","SGP","SG","P","Full")
names(Yield.MA.model.ML) <- c("None","LUI","LUI.P","P","Full")

### Analysis for richness
Richness.MA.model.ML[["None"]] <- rma.mv.func(df=modelDataRichness, moderators=c(1), fit.method="ML")
Richness.MA.model.ML[["LUI"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level"), fit.method="ML")
Richness.MA.model.ML[["LUI.SGP"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level","Product", "Species.Group", "LUI.range.level:Product", "LUI.range.level:Species.Group", "Species.Group:Product"), fit.method="ML")
Richness.MA.model.ML[["LUI.SG"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level","Species.Group", "LUI.range.level:Species.Group"), fit.method="ML")
Richness.MA.model.ML[["LUI.P"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level","Product", "LUI.range.level:Product"), fit.method="ML")
Richness.MA.model.ML[["SGP"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"Product", "Species.Group", "Species.Group:Product"), fit.method="ML")
Richness.MA.model.ML[["SG"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"Species.Group"), fit.method="ML")
Richness.MA.model.ML[["P"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"Product"), fit.method="ML")
Richness.MA.model.ML[["Full"]] <- rma.mv.func(df=modelDataRichness, moderators=c(-1,"LUI.range.level", "Product", "Species.Group", "landuse_history","main_climate", "LUI.range.level:Product", "LUI.range.level:Species.Group","LUI.range.level:landuse_history","LUI.range.level:main_climate","Species.Group:Product"), fit.method="ML")

### Analysis for yield
Yield.MA.model.ML[["None"]] <- rma.mv.func(df=modelDataYield, moderators=c(1), fit.method="ML")
Yield.MA.model.ML[["LUI"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level"), fit.method="ML")
Yield.MA.model.ML[["LUI.P"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level", "Product", "LUI.range.level:Product"), fit.method="ML")
Yield.MA.model.ML[["P"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"Product"), fit.method="ML")

Yield.MA.model.ML[["Full"]] <- rma.mv.func(df=modelDataYield, moderators=c(-1,"LUI.range.level", "Product", "landuse_history","main_climate", "LUI.range.level:Product","LUI.range.level:landuse_history","LUI.range.level:main_climate"), fit.method="ML")


###########################################################################
### 08.3. extract fit statistics
###########################################################################
Richness.models2report <- list(Richness.MA.model[["None"]],
                      Richness.MA.model[["LUI"]],
                      Richness.MA.model[["LUI.SGP"]],
                      Richness.MA.model[["Full"]])
Richness.models2report4AIC <- list(Richness.MA.model.ML[["None"]],
                               Richness.MA.model.ML[["LUI"]],
                               Richness.MA.model.ML[["LUI.SGP"]],
                               Richness.MA.model.ML[["Full"]])
fit.tab.richness <- data.frame(model=c("None","LUI","LUI.SGP","Full"),
                               logLik=NA, deviance=NA, AIC=NA, BIC=NA, AICc=NA, 
                               QE=unlist(lapply(Richness.models2report,function(x) x$QE)), QEp=unlist(lapply(Richness.models2report,function(x) x$QEp)),
                               QM=unlist(lapply(Richness.models2report,function(x) x$QM)),QMp=unlist(lapply(Richness.models2report,function(x) x$QMp)))
Yield.models2report <- list(Yield.MA.model[["None"]],
                               Yield.MA.model[["LUI"]],
                               Yield.MA.model[["LUI.P"]],
                               Yield.MA.model[["Full"]])
Yield.models2report4AIC <- list(Yield.MA.model.ML[["None"]],
                            Yield.MA.model.ML[["LUI"]],
                            Yield.MA.model.ML[["LUI.P"]],
                            Yield.MA.model.ML[["Full"]])
fit.tab.yield <- data.frame(model=c("None","LUI","LUI.P","Full"),
                               logLik=NA, deviance=NA, AIC=NA, BIC=NA, AICc=NA, 
                               QE=unlist(lapply(Yield.models2report,function(x) x$QE)), QEp=unlist(lapply(Yield.models2report,function(x) x$QEp)),
                               QM=unlist(lapply(Yield.models2report,function(x) x$QM)),QMp=unlist(lapply(Yield.models2report,function(x) x$QMp)))

fit.tab.richness[,2:6] <- t(sapply(Richness.models2report4AIC,fitstats.rma))
fit.tab.yield[,2:6] <- t(sapply(Yield.models2report4AIC,fitstats.rma))

fit.tab.richness$I2_StudyID <- sapply(Richness.models2report, function(x) x$sigma2[1]/sum(x$sigma2))
fit.tab.richness$I2_StudyCase <- sapply(Richness.models2report, function(x) x$sigma2[2]/sum(x$sigma2))
fit.tab.yield$I2_StudyID <- sapply(Yield.models2report, function(x) x$sigma2[1]/sum(x$sigma2))
fit.tab.yield$I2_StudyCase <- sapply(Yield.models2report, function(x) x$sigma2[2]/sum(x$sigma2))

fit.tab.richness$R2 <- fit.tab.richness$QM/(fit.tab.richness$QM+fit.tab.richness$QE)
fit.tab.yield$R2 <- fit.tab.yield$QM/(fit.tab.yield$QM+fit.tab.yield$QE)

fit.tab.richness$deltaAICc <- fit.tab.richness$AICc - min(fit.tab.richness$AICc)
fit.tab.richness$deltaBIC <- fit.tab.richness$BIC - min(fit.tab.richness$BIC)
fit.tab.yield$deltaAICc <- fit.tab.yield$AICc - min(fit.tab.yield$AICc)
fit.tab.yield$deltaBIC <- fit.tab.yield$BIC - min(fit.tab.yield$BIC)

write.csv(fit.tab.richness[,c("model","deltaAICc","deltaBIC","QM","QMp","QE","R2","I2_StudyID","I2_StudyCase")],file=path2temp %+% "fit.tab.richness.csv",row.names=F)
write.csv(fit.tab.yield[,c("model","deltaAICc","deltaBIC","QM","QMp","QE","R2","I2_StudyID","I2_StudyCase")],file=path2temp %+% "fit.tab.yield.csv",row.names=F)

