#ratio of ratios
library(metafor)
library(ggplot2)

setwd("c:/Users/hoppek/Dropbox/sDiv_workshop/Meta-Analysis/DataAnalysis") #KG
#setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB

ES.frame = read.csv("ES_table.csv")
ES.frame <- ES.frame[-which(ES.frame$Study.ID=="4788-Mosquera-Losada2009"),] 

#########################################################################################################################################################
LMM.MA.fit <- function(yi,vi,mods,slab,inner2,outer2){
  ##########################################################################################################################
  ### fit meta-analytic multivariate/multilevel fixed- and random/mixed-effects models with or without moderators via linear (mixed-effects) models using rma.mv
  ## Viechtbauer 2015 p 189: the random argument can also contain one (and only one!) formula of the form ~ inner | outer . Effects or outcomes with different values/levels of the outer grouping variable/factor are assumed to be independent, while effects or outcomes with the same value/level of the outer grouping variable/factor share correlated random effects corresponding to the levels of the inner grouping variable/factor. The struct argument is used to specify the variance structure corresponding to the inner variable/factor. With struct="CS", a compound symmetric structure is assumed (i.e., a single variance component tau? corresponding to all values/levels of the inner variable/factor and a single correlation coefficient rho for the correlation between different values/levels). 
  # btw, there seems to be a problem with the terms inner and out er as they are also R base functions
# TO DO: calculate variance-covariance matrix, esp covariance for between LUI comparisons 
#  VCov_matrix <- 
  rma.mv.fit <- rma.mv(yi=yi, V=vi, mods = mods, random = ~inner2|outer2, struct="CS", data=ES.frame, slab=slab,method="REML", tdist=FALSE, level=95, digits=4, Rscale="cor", sparse=FALSE, verbose=FALSE)
  return(rma.mv.fit)
}

# TO DO: make a list of moderators to be tested, store model results in a list, so they can be plotted in a next step (03_Plotting.r)
### Analysis without moderators
mods <- 1
attach(ES.frame)
Richness.MA.fit <- LMM.MA.fit(yi=ES.frame$Richness.Log.RR,vi=ES.frame$Richness.Log.RR.Var,mods=~1,slab=paste(ES.frame$Study.Case,ES.frame$Low.LUI,ES.frame$High.LUI,sep="_"),inner2=ES.frame$Study.Case,outer2=ES.frame$Study.ID)
Yield.MA.fit <- LMM.MA.fit(yi=Yield.Log.RR,vi=Yield.Log.RR.Var,mods=~1,slab=paste(Study.Case,Low.LUI,High.LUI,sep="_"),inner=Study.Case,outer=Study.ID)
detach(ES.frame)
save(mods,Richness.MA.fit,Yield.MA.fit, file="Output/MA_model_1.Rdata")

### Store results in table
MA.coeffs <- data.frame(Moderator="None",levels=1,number.of.studies.within.level=,mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se)

moderator.list <- c("Habitat.Type","Species.Group","Trophic.Level","LUI.comparison","Study.Type")

for(mods in moderator.list){
  print(mods)
  attach(ES.frame)
  Richness.MA.fit <- LMM.MA.fit(yi=Richness.Log.RR,vi=Richness.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),slab=paste(Study.Case,Low.LUI,High.LUI,sep="_"),inner=Study.Case,outer=Study.ID)
  Yield.MA.fit <- LMM.MA.fit(yi=Yield.Log.RR,vi=Yield.Log.RR.Var,mods=as.formula(paste("~",mods,"-1",sep="")),slab=paste(Study.Case,Low.LUI,High.LUI,sep="_"),inner=Study.Case,outer=Study.ID)
  detach(ES.frame)
  MA.coeffs <- rbind(MA.coeffs,data.frame(Moderator=rep(mods,length(Richness.MA.fit$b)),levels=unlist(lapply(strsplit(rownames(Richness.MA.fit$b),mods),function(x){x[[2]]})),mean.Richness=Richness.MA.fit$b,se.Richness=Richness.MA.fit$se,mean.Yield=Yield.MA.fit$b,se.Yield=Yield.MA.fit$se))
  print(MA.coeffs)
  save(mods,Richness.MA.fit,Yield.MA.fit, file=paste("Output/MA_model_",mods,".Rdata",sep=""))
}

write.csv(MA.coeffs, file="Output/MA.coeffs.csv",row.names=F)
