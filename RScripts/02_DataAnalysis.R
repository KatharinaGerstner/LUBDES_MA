############################################################################
### Purpose of this skript module 02 is to:
###
### 02.1. Prepare data analysis
### 02.2. LMM.MA.fit function
### 02.3. Analysis without moderators
### 02.4. Analysis with moderators
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


ES.frame.posVar <- ES.frame[ES.frame$Richness.Log.RR.Var>0 & ES.frame$Yield.Log.RR.Var>0,] # restrict analysis to study cases with positive variances
ES.frame <- ES.frame.posVar
ES.frame$LUI.range <- factor(ES.frame$LUI.range)


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

### Store results in table
MA.coeffs <- data.frame(Moderator="None",levels=1,mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se)

############################################################################
### 02.4. Analysis with moderators
### 
############################################################################

# define list of moderators
moderator.list <- c("Land.use...land.cover","Species.Group","Trophic.Level","LUI.range","Product")

# run analysis
for(mods in moderator.list){
  print(mods)
  attach(ES.frame)
  Richness.MA.fit <- rma.mv(yi=Richness.Log.RR,V=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame)
  Yield.MA.fit <- rma.mv(yi=Yield.Log.RR,V=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),random = ~factor(Case.ID)|factor(Study.ID), struct="CS", slab=paste(Study.Case, Low.LUI, High.LUI,sep="_"),method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE,data=ES.frame)
  detach(ES.frame)
  MA.coeffs <- rbind(MA.coeffs,data.frame(Moderator=rep(mods,length(Richness.MA.fit$b)),levels=unlist(lapply(strsplit(rownames(Richness.MA.fit$b),mods),function(x){x[[2]]})),mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se))
  print(MA.coeffs)
}


