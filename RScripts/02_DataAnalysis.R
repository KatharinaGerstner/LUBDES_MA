############################################################################
### Purpose of this skript module 02 is to:
###
### 02.1. Prepare data analysis
### 02.2. LMM.MA.fit function
### 02.3. Analysis without moderators
### 02.4. Analysis with moderators
### 02.5. Analysis with moderators for no LU vs low/medium/high LU
### 02.6. multivariate analysis with and without moderators (not yet working)
###
### General comments:
### * 02.2. LMM.MA.fit function is currently not needed
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
### 02.1. Prepare data analysis
### 
############################################################################

ES.frame <- subset(ES.frame, Richness.Log.RR.Var>0 & Yield.Log.RR.Var>0) # restrict analysis to study cases with positive variances
#ES.frame$LUI.range <- factor(ES.frame$LUI.range)

ES.frame.noLU <- subset(ES.frame.noLU, Richness.Log.RR.Var>0) # restrict analysis to study cases with positive variances

############################################################################
### 02.2. LMM.MA.fit function
### * 02.2. LMM.MA.fit function is currently not needed
############################################################################

### LMM.MA.fit function
# LMM.MA.fit <- function(yi,vi,mods,slab,inner2,outer2){
# # TO DO: calculate variance-covariance matrix, esp covariance for between LUI comparisons 
# #  VCov_matrix <- 
#   inner3<-inner2
#   outer3<-outer2
#   rma.mv.fit <- rma.mv(yi=yi, V=vi, mods = mods, random = ~factor(inner3)|factor(outer3), struct="CS", data=ES.frame, slab=slab,method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE)
#   return(rma.mv.fit)
# }

############################################################################
### 02.3. Analysis without moderators
### 
############################################################################

# TO DO: make a list of moderators to be tested, store model results in a list, so they can be plotted in a next step (03_Plotting.r)


### Analysis without moderators
Richness.MA.fit <- rma.mv(yi=Richness.Log.RR, V=Richness.Log.RR.Var, mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame)
Yield.MA.fit <- rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame)
preds.richness <- predict.rma(Richness.MA.fit) 
preds.yield <- predict.rma(Yield.MA.fit) 

### Store results in table
MA.coeffs.cat <- data.frame(Moderator="None",levels=1,mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se)
MA.coeffs.cont <- data.frame(Moderator="None",Richness.intercept=Richness.MA.fit$b,Richness.slope=0, Richness.se.intercept=Richness.MA.fit$se, Richness.se.slope=0, Yield.intercept=Yield.MA.fit$b, Yield.slope=0, Yield.se.intercept=Yield.MA.fit$se, Yield.se.slope=0)

############################################################################
### 02.4. Analysis with moderators
### 
############################################################################

# define list of moderators
moderator.list.cat <- c("Land.use...land.cover","Species.Group","Trophic.Level","LUI.range.level","Product", "ES.From.BD","BIOME")
moderator.list.cont <- c("GDP.pc.2000","annual_mean_radiation","capital_stock_in_agriculture","habitat_dissimilarity")
# run analysis
for(mods in moderator.list.cat){
  print(mods)
  ### fit model
  attach(ES.frame)
  Richness.MA.fit <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame), silent=T)
  Yield.MA.fit <- try(rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame),silent=T)
  detach(ES.frame)
  ### catch errors
  if(class(Richness.MA.fit)=="try-error") {
    geterrmessage()
    Richness.MA.fit <- data.frame(b=NA,se=NA)
  }
  if(class(Yield.MA.fit)=="try-error") {
    geterrmessage()
    Yield.MA.fit <- data.frame(b=NA,se=NA)
  }
  ### tabularize model parameters
  MA.coeffs.cat <- rbind(MA.coeffs.cat,data.frame(Moderator=rep(mods,length(Richness.MA.fit$b)),levels=unlist(lapply(strsplit(rownames(Richness.MA.fit$b),mods),function(x){x[[2]]})),mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se))
  print(MA.coeffs.cat)
}

for(mods in moderator.list.cont){
  print(mods)
  attach(ES.frame)
  ### fit model
  Richness.MA.fit <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame),silent=T)
  Yield.MA.fit <- try(rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame),silent=T)
  detach(ES.frame)
  ### catch errors
  if(class(Richness.MA.fit)=="try-error") {
    geterrmessage()
    Richness.MA.fit <- data.frame(b=c(NA,NA),se=c(NA,NA))
  }
  if(class(Yield.MA.fit)=="try-error") {
    geterrmessage()
    Yield.MA.fit <- data.frame(b=c(NA,NA),se=c(NA,NA))
  }
  ### tabularize model parameters
  MA.coeffs.cont <- rbind(MA.coeffs.cont,data.frame(Moderator=mods, Richness.intercept=Richness.MA.fit$b[1], Richness.slope=Richness.MA.fit$b[2], Richness.se.intercept=Richness.MA.fit$se[1], Richness.se.slope=Richness.MA.fit$se[2], Yield.intercept=Yield.MA.fit$b[1], Yield.slope=Yield.MA.fit$b[2], Yield.se.intercept=Yield.MA.fit$se[1], Yield.se.slope=Yield.MA.fit$se[2]))
  print(MA.coeffs.cont)
  preds.richness <- ifelse(all(is.na(Richness.MA.fit)),list(preds.richness, pred = NA, se = NA, ci.lb = NA, ci.ub = NA, cr.lb = NA, cr.ub = NA),list(preds.richness, predict.rma(Richness.MA.fit))) 
  preds.yield <- ifelse(all(is.na(Yield.MA.fit)),list(preds.yield,  pred = NA, se = NA, ci.lb = NA, ci.ub = NA, cr.lb = NA, cr.ub = NA),list(preds.yield, predict.rma(Yield.MA.fit))) 
}

############################################################################
### 02.5. Analysis with moderators for no LU vs low/medium/high LU
### 
############################################################################

### Analysis without moderators
Richness.MA.fit.noLU <- rma.mv(yi=Richness.Log.RR, V=Richness.Log.RR.Var, mods=~1, random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame.noLU)

### Store results in table
MA.coeffs.noLU <- data.frame(Moderator="None",levels=1,mean.Richness=Richness.MA.fit.noLU$b,se.Richness=Richness.MA.fit.noLU$se)

# define list of moderators
moderator.list <- c("Land.use...land.cover","Species.Group","Trophic.Level","BIOME")

# run analysis
for(mods in moderator.list){
  print(mods)
  ### fit model 
  attach(ES.frame.noLU)
  Richness.MA.fit.noLU <- try(rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame.noLU),silent=T)
  detach(ES.frame.noLU)
  ### catch errors
  if(class(Richness.MA.fit.noLU)=="try-error") {
    geterrmessage()
    Richness.MA.fit.noLU <- data.frame(b=NA,se=NA)
  }
  ### tabularize model parameters
  
  MA.coeffs.noLU <- rbind(MA.coeffs.noLU,data.frame(Moderator=rep(mods,length(Richness.MA.fit.noLU$b)),levels=unlist(lapply(strsplit(rownames(Richness.MA.fit.noLU$b),mods),function(x){x[[2]]})),mean.Richness=Richness.MA.fit.noLU$b,se.Richness=Richness.MA.fit.noLU$se))
  print(MA.coeffs.noLU)
}

############################################################################
### 02.6. multivariate analysis with and without moderators (not yet working)
### (cf. ?dat.berkey1998)
############################################################################
### restructure ES.frame: Richness.Log.RR, Yield.Log.RR in separate rows
# ES.frame.reduced <- ES.frame[,c("Study.Case","Land.use...land.cover","Species.Group","Species.Subgroup","Trophic.Level","LUI.range.level","LUI.range","BIOME")]
# dat <- rbind(ES.frame.reduced,ES.frame.reduced)
# dat$outcome <- c(rep("Richness",nrow(ES.frame)),rep("Yield",nrow(ES.frame)))
# 
# ### compute covariance matrix between Richness and Yield RR per Study.Case
# covar <- cov(ES.frame$Richness.Log.RR.Var,ES.frame$Yield.Log.RR.Var) ## don't know how to determine covariance of Richness and Yield RR per Study.Case
# dat$Log.RR <- c(ES.frame$Richness.Log.RR,ES.frame$Yield.Log.RR)
# dat$Richness.Log.RR.Var <- c(cbind(t(c(ES.frame$Richness.Log.RR.Var,covar))))
# dat$Yield.Log.RR.Var <- c(cbind(t(covar,ES.frame$Yield.Log.RR.Var)))
# 
# ### construct list of the variance-covariance matrices of the observed outcomes for the studies
# V <- lapply(split(dat[,c("Richness.Log.RR.Var", "Yield.Log.RR.Var")], dat$Study.Case), as.matrix)
# ### construct block diagonal matrix
# V <- bldiag(V)
# 
# ### Fit multivariate meta-analytic model
# MA.fit <- rma.mv(yi=Log.RR, V, mods = ~ outcome + outcome:BIOME - 1,
#                  random = ~factor(Case.ID)|factor(Study.ID), struct="UN", data=dat, method="ML")


###########################################################################
### Resterampe
### use full model with all levels simultaneously or separate models for each level

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
