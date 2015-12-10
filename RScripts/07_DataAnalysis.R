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

############################################################################
### 07.1. Prepare data analysis
############################################################################

ES.frame <- subset(ES.frame, Richness.Log.RR.Var>0 & Yield.Log.RR.Var>0) # restrict analysis to study cases with positive variances

ES.frame$Species.Group<-paste(ES.frame$Species.Group)
ES.frame$Species.Group[(ES.frame$Species.Group=="arthropods")]<-"invertebrates"
ES.frame$Species.Group[(ES.frame$Species.Group=="non-arthropod invertebrates")]<-"invertebrates"
ES.frame$Species.Group[(ES.frame$Species.Group=="fungi")]<-NA
ES.frame$Species.Group<-factor(ES.frame$Species.Group)


### Remove pseudo-replicates
ES.frame.richness <- ES.frame[!duplicated(ES.frame[,c("Study.ID","Case.ID","LUI.range.level","Species.Group")]),]
ES.frame.yield <- ES.frame[!duplicated(ES.frame[,c("Study.ID","LUI.range.level","Product")]),]

ES.frame.noLU <- subset(ES.frame.noLU, Richness.Log.RR.Var>0) # restrict analysis to study cases with positive variances
ES.frame.noLU.richness <- ES.frame.noLU[!duplicated(ES.frame.noLU[,c("Study.Case","High.LUI","Species.Group")]),]

###########################################################################
### remove redundant cases
for(x in unique(ES.frame$Study.Case)){
  subset.richness <- subset(ES.frame.richness,Study.Case==x)
  subset.yield <- subset(ES.frame.yield,Study.Case==x)
  ## remove redundant cases
  if (all(c("low-medium","low-high","medium-high") %in% unique(subset.richness$LUI.range.level))){
    ES.frame.richness <- ES.frame.richness[!(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="medium-high"),]
  }
  if (all(c("low-medium","low-high","medium-high") %in% unique(subset.yield$LUI.range.level))){
    ES.frame.yield <- ES.frame.yield[!(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="medium-high"),]
  }
}  

###########################################################################
### Variance-Covariance Matrix
###########################################################################
Var.Richness <- diag(ES.frame.richness$Richness.Log.RR.Var)
Var.Yield <- diag(ES.frame.yield$Yield.Log.RR.Var)
## calculate covariance of shared control and store it on the off-diagonal 
for(x in unique(ES.frame$Study.Case)){
  if (all(c("low-medium","low-high") %in% unique(subset.richness$LUI.range.level))){
    row.1 <- which(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="low-medium")
    col.1 <- which(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="low-high")
    Var.Richness[row.1,col.1] <- Var.Richness[col.1,row.1] <- subset.richness$Richness.SD.Low[which(subset.richness$LUI.range.level=="low-medium")]^2/(subset.richness$Richness.N.Low[which(subset.richness$LUI.range.level=="low-medium")]*subset.richness$Richness.Mean.Low[which(subset.richness$LUI.range.level=="low-medium")]) ## sd_c²/(n_c*mean(X_c)²), cf. Lajeunesse (2011) Ecology
  }
  if (all(c("low-medium","low-high") %in% unique(subset.yield$LUI.range.level))){
    row.1 <- which(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="low-medium")
    col.1 <- which(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="low-high")
    Var.Yield[row.1,col.1] <- Var.Yield[col.1,row.1] <- subset.yield$Yield.SD.Low[which(subset.yield$LUI.range.level=="low-medium")]^2/(subset.yield$Yield.N.Low[which(subset.yield$LUI.range.level=="low-medium")]*subset.yield$Yield.Mean.Low[which(subset.yield$LUI.range.level=="low-medium")]) ## sd_c²/(n_c*mean(X_c)²), cf. Lajeunesse (2011) Ecology
  }
}


### store models in a list
Richness.MA.model <- list() 
Yield.MA.model <- list()

### store predictions in a list
preds.richness <- list()
preds.yield <- list()

############################################################################
### 07.2. Analysis without moderators
############################################################################

Richness.MA.fit <- rma.mv(yi=Richness.Log.RR, V=Var.Richness, mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.richness)
Richness.MA.model[["None"]] <- Richness.MA.fit
preds.richness[["None"]] <- predict.rma(Richness.MA.fit) 

Yield.MA.fit <- rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.yield)#[!duplicated(data[c("Study.ID","LUI.range.level","Product"),])])
Yield.MA.model[["None"]] <- Yield.MA.fit
preds.yield[["None"]] <- predict.rma(Yield.MA.fit) 

### Store parameter estimates in a table
MA.coeffs.cat <- data.frame(Moderator="None",levels=1,mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se)
MA.coeffs.cont <- data.frame(Moderator="None",Richness.intercept=Richness.MA.fit$b,Richness.slope=0, Richness.se.intercept=Richness.MA.fit$se, Richness.se.slope=0, Yield.intercept=Yield.MA.fit$b, Yield.slope=0, Yield.se.intercept=Yield.MA.fit$se, Yield.se.slope=0)

############################################################################
### 07.3. Analysis with moderators
############################################################################

### define list of moderators
moderator.list.cat <- c("Species.Group","LUI.range.level","Product","BIOME")
moderator.list.cont <- c("rel_capital_stock_in_agriculture","habitat_dissimilarity","time.since.first.use","npp")

moderator.list <- c(moderator.list.cat,moderator.list.cont)
modelFormula <- as.formula(paste("~",paste(moderator.list,collapse="+"),sep=""))

modelData <- ES.frame.richness[,c('Richness.Log.RR','Richness.Log.RR.Var','Species.Group','LUI.range.level','Product','BIOME',
                         'rel_capital_stock_in_agriculture','habitat_dissimilarity','time.since.first.use','npp',
                         'Case.ID','Study.ID','Study.Case','Low.LUI','High.LUI')]
modelData <- na.omit(modelData)

Richness.MA.fit <- rma.mv(yi=Richness.Log.RR, V=Richness.Log.RR.Var, mods=~Species.Group + LUI.range.level + Product + BIOME + 
                            rel_capital_stock_in_agriculture + habitat_dissimilarity + 
                            time.since.first.use + npp, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", 
                          slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),
                          method="ML", tdist=FALSE, level=95, digits=4,data=modelData)

stats<-RMASelect(Richness.MA.fit)

### run analysis for categorical moderators
for(mods in moderator.list.cat){
  
  print(mods)

  ### fit model
#  attach(ES.frame)
  Richness.MA.fit <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.richness), silent=T)
  Richness.MA.model[[mods]] <- Richness.MA.fit
  ### catch errors
  if(class(Richness.MA.fit)=="try-error") {
    print(geterrmessage())
    Richness.MA.fit <- data.frame(b=NA,se=NA)
  }

  ### fit model
  Yield.MA.fit <- try(rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.yield),silent=T)
  Yield.MA.model[[mods]] <- Yield.MA.fit
  ### catch errors
  if(class(Yield.MA.fit)=="try-error") {
    print(geterrmessage())
    Yield.MA.fit <- data.frame(b=NA,se=NA)
  }
  
#  detach(ES.frame)

  ### tabularize model parameters
  MA.coeffs.cat <- rbind(MA.coeffs.cat,data.frame(Moderator=rep(mods,length(Richness.MA.fit$b)),levels=unlist(lapply(strsplit(rownames(Richness.MA.fit$b),mods),function(x){x[[2]]})),mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se))
}
print(MA.coeffs.cat)

### run analysis for continuous moderators

for(mods in moderator.list.cont){
  
  print(mods)
#  attach(ES.frame)
  
  ### fit model
  Richness.MA.fit <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.richness),silent=T)
  Richness.MA.model[["None"]] <- Richness.MA.fit
  ### catch errors
  if(class(Richness.MA.fit)=="try-error") {
    print(geterrmessage())
    Richness.MA.fit <- data.frame(b=c(NA,NA),se=c(NA,NA))
  }

  ### fit model
  Yield.MA.fit <- try(rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.yield),silent=T)
  Yield.MA.model[[mods]] <- Yield.MA.fit
  ### catch errors
  if(class(Yield.MA.fit)=="try-error") {
    print(geterrmessage())
    Yield.MA.fit <- data.frame(b=c(NA,NA),se=c(NA,NA))
  }
  
#  detach(ES.frame)

  ### tabularize model parameters
  MA.coeffs.cont <- rbind(MA.coeffs.cont,data.frame(Moderator=mods, Richness.intercept=Richness.MA.fit$b[1], Richness.slope=Richness.MA.fit$b[2], Richness.se.intercept=Richness.MA.fit$se[1], Richness.se.slope=Richness.MA.fit$se[2], Yield.intercept=Yield.MA.fit$b[1], Yield.slope=Yield.MA.fit$b[2], Yield.se.intercept=Yield.MA.fit$se[1], Yield.se.slope=Yield.MA.fit$se[2]))
  preds.richness[[mods]] <- ifelse(all(is.na(Richness.MA.fit)),list(data.frame(pred = NA, se = NA, ci.lb = NA, ci.ub = NA, cr.lb = NA, cr.ub = NA)), list(predict.rma(Richness.MA.fit))) 
  preds.yield[[mods]] <- ifelse(all(is.na(Yield.MA.fit)), list(data.frame(pred = NA, se = NA, ci.lb = NA, ci.ub = NA, cr.lb = NA, cr.ub = NA)), list(predict.rma(Yield.MA.fit))) 
}
print(MA.coeffs.cont)

############################################################################
### 07.4. Analysis with moderators for no LU vs low/medium/high LU
############################################################################

Richness.MA.model.noLU <- list()

### Analysis without moderators
Richness.MA.fit.noLU <- rma.mv(yi=Richness.Log.RR, V=Richness.Log.RR.Var, mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.noLU)
Richness.MA.model.noLU[["None"]] <- Richness.MA.fit.noLU

### Store results in table
MA.coeffs.noLU <- data.frame(Moderator="None",levels=1,mean.Richness=Richness.MA.fit.noLU$b,se.Richness=Richness.MA.fit.noLU$se)

# define list of moderators
moderator.list <- c("Product","Product:High.LUI","Land.use...land.cover","Species.Group","BIOME")

# run analysis
for(mods in moderator.list){
  print(mods)
  ### fit model 
#  attach(ES.frame.noLU)
  Richness.MA.fit.noLU <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4,data=ES.frame.noLU.richness),silent=T)
  Richness.MA.model.noLU[[mods]] <- Richness.MA.fit.noLU
  ### catch errors
  if(class(Richness.MA.fit.noLU)[1]=="try-error") {
    geterrmessage()
    Richness.MA.fit.noLU <- data.frame(b=NA,se=NA)
  }
#  detach(ES.frame.noLU)

  ### tabularize model parameters  
  ifelse(mods=="Product:High.LUI", levels <- unlist(lapply(strsplit(unlist(lapply(strsplit(rownames(Richness.MA.fit.noLU$b),"Product"),function(x)x[[2]])),"High.LUI"),function(y) paste(y[1],y[2],sep=""))), levels <- unlist(lapply(strsplit(rownames(Richness.MA.fit.noLU$b),mods),function(x){x[[2]]})))
  MA.coeffs.noLU <- rbind(MA.coeffs.noLU,data.frame(Moderator=rep(mods,length(Richness.MA.fit.noLU$b)),
                                                    levels=levels,
                                                    mean.Richness=Richness.MA.fit.noLU$b,
                                                    se.Richness=Richness.MA.fit.noLU$se))
}
print(MA.coeffs.noLU)



###########################################################################
### Resterampe
### use full model with all levels simultaneously or separate models for each level


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
