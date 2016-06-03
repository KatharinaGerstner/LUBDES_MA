############################################################################
### Purpose of this skript module 00 is to:
###
### Some general comments on the structure of the LUBDES_MA scripts:
### * Each module (00, 01 ...) builds on the previous ones and does NOT work standalone - this avoids redundancies.
### * Exceptions to that rule are -03 to -01 which should not be run by anyone other than MB.
### * Data, functions etc. will be carried over from one module to another. Saving and loading of interim outputs is done into path2temp. 
### * In case interim data needs to be saved it must NOT be saved in the git directory. This will avoid that accidentally our whole dataset is visible to everyone on the internet. 
### * All needed output files (tables, plots etc.) must NOT be saved in the git directory but in path2temp instead.
### * Hidden files like .rhistory .oauth etc must NOT be commited to github. This avoids errors. Also, uploading the authorization token for google docs to github might allow access to google accounts by hackers (presumably, #MB)
### * The data input comes directly from google docs. loading .csv is deprecated.
### 
###
### Overall structure of the modules is:
### -03 -03_create_csv_from_Zotero.R
### [-03.1. To extract zotero citation data into excel.]
###
### -02 -02_create_folders_for_papers.R
### [-02.1. set working directory to create folders in]
### [-02.2. create directory structure based on study IDs and place an empty note file within]
###
### -01 -01_load+analyse_screeningtable.r
### [-01.1. load screening data directly from google docs]
### [-01.2. plot pie charts about statistics]
###
### 00 00_initialize_directories.r
### 00.1. set the working and temporary directories
### 00.2. source all relevant R scripts
###
### 01 01_load_libraries_and_functions.r
### 01.1. load all libraries needed for subsequent analysis
### 01.2. load all self-written functions needed for subsequent analysis
###
### 02 02_load_table_directly_from_google.R
### 02.1. load data directly from google docs
### 02.2. adapt data structure
###
### 00_ComparativeAnalysis_complete_vs_single_vs_MI.r 
### Repeat all the following steps for 
### 1. Complete cases only
### 2. mean imputation
### 3. mulitple imputation

### 03 03_ImputeMissingData_mice
### _mean.r # impute missing data using the mice package and the mean of 10 imputation chains
### _loops.r # impute missing data using the mice package and saves all 10 imputation chains in separate folders
###
### 04 04_CompileESframe.R
### 04.1. Compile ES frame
### 04.2. Calculate response ratio effect sizes
###
### 05 05_AddMapDataToESframe.R
### 05.1. Intersect studies with global maps of WWF_REALMs Ecoregions, combine to coarser classes
###
### 06 06_DataPreparation4Analysis.R
### 06.1. Remove cases with zero variances, pseudo-replicates, redundant LUI.range.level comparisons 
### 06.2. remove columns not needed for the analysis, unify names
### 06.2. save rawdata as table in a word doc
###
### 07 07.1_DescriptiveStatsOfESFrame.Rmd
### 07.1. Protocol structure and summary of variables in the ES.frames for richness and yield
### 07.2. Plot Histograms of all variables in the ES.frame 
### 07.3. Plot map of studies
### 07.4. Forest Plots of study.cases per LUI.range.level

### 07 07.2_Plot_forest_plots_loops.r
### Forest Plots of study-cases per LUI.range.level, imputed SDs are highlighted by transparent lines
###
### 08 08a_DataAnalysis.R
### 07.1. Prepare data analysis
### 07.2. Analysis without moderators
### 07.3. Analysis with moderators
### 07.4. Analysis with moderators for no LU vs low/medium/high LU
###
### 09 09.1_Plot_model_coeffs.r
### 09.1. plot model parameter estimates
### 09.1.1. plot cross diagrams
### 09.1.2. plot Panel for LUIrangelevel
### 09.1.3. Plot model coefficients + SE relative to the intercept (cf. Fig1 in Newbold et al. 2015)
### 
###
### Authors: MB, KG, ...
############################################################################

############################################################################
### 00.1. set the working and temporary directories
###
### checks for nodename (or username in case of RS) and sets directories accordingly
############################################################################

.setwdntemp <- function(){
  cu <- Sys.info()["user"]
  cn <- Sys.info()["nodename"]
  
  if (cu == "rseppelt")
  {
    path2temp <- "/Users/rseppelt/Documents/Projekte/Synthese & Netzwerke/LU-BD-ES/Temp" 
    path2wd <- "/Users/rseppelt/Documents/git/LUBDES_MA/RScripts/" 
  } else if (cn == "UCBTTNE-LT"){
    path2wd <- "C:/Users/Tim/Documents/LUBDES_MA/RScripts/" #TN
    path2temp <- "C:/Users/Tim/Documents/LUBDES_MA_Out/" #TN
    
  } else if (cn == "juro-MacBookPro"){
    path2wd <- "/home/juro/git/LUBDES_MA/RScripts/" #MB
    path2temp <- "/home/juro/tmp/" #MB
    
  } else if (cn == "LEIH-HAL6"){
    path2wd <- "C:/Users/kambach/Desktop/aktuelle Arbeiten/SESYNC/LUBDES_MA-master/RScripts/" #SK
    path2temp <- "C:/Users/kambach/Desktop/aktuelle Arbeiten/SESYNC/LUBDES_MA-master/RScripts/" #SK
    
  } else if (cn == "Helen-Phillipss-MacBook-Pro.local"){
    path2wd <- "/Users/Helen/LUBDES_MA/RScripts/"
    path2temp <- "/Users/Helen/tmp/" ##HP
  } else {
    path2wd <- "C:/Users/hoppek/Documents/GitHub/LUBDES_MA/RScripts/" #KG
    path2temp <- "C:/Users/hoppek/Documents/temp/" #KG 
  }  
  return(list(path2temp,path2wd))
}

############################################################################
### 00.2. source all relevant R scripts
###
### 
############################################################################

############################################################################
### DATA PREPARATION
############################################################################
### helper function to combine strings
"%+%" <- function(x,y)paste(x,y,sep="")

set.list <-  .setwdntemp()
path2wd <- set.list[[2]]

### fit the models
source(path2wd %+% "00_ComparativeAnalysis_complete_vs_single_vs_MI.r")

############################################################################
## load model for complete cases
path2temp <- set.list[[1]] %+% "CompleteCases/"
load(path2temp %+% "Models.Rdata")
Richness.MA.complete <- Richness.MA.model
Yield.MA.complete <- Yield.MA.model

## load models for each imputation chain and collapse parameters in nested lists
path2temp.list <- c(set.list[[1]] %+% "imp" %+% 1:10 %+% "/")
Richness.MA <- Yield.MA <- vector("list", length=4)
names(Richness.MA) <- names(Yield.MA) <- c("None","LUI","Full","Select")

Richness.MA[["None"]] <- Richness.MA[["LUI"]] <- Richness.MA[["Full"]] <- Richness.MA[["Select"]] <- vector("list", length=nchains)
Yield.MA[["None"]] <- Yield.MA[["LUI"]] <- Yield.MA[["Full"]] <- Yield.MA[["Select"]] <- vector("list", length=nchains)

for(i in 1:nchains){
  path2temp <- path2temp.list[i]
  print(path2temp)
  load(path2temp %+% "Models.Rdata")
  Richness.MA[["None"]][[i]] <- data.frame(mean=Richness.MA.model[["None"]]$b,se=Richness.MA.model[["None"]]$se) 
  Richness.MA[["LUI"]][[i]] <- data.frame(mean=Richness.MA.model[["LUI"]]$b,se=Richness.MA.model[["LUI"]]$se) 
  Richness.MA[["Full"]][[i]] <- data.frame(mean=Richness.MA.model[["Full"]]$b,se=Richness.MA.model[["Full"]]$se) 
  Richness.MA[["Select"]][[i]] <- data.frame(mean=Richness.MA.model[["Select"]]$b,se=Richness.MA.model[["Select"]]$se) 

  Yield.MA[["None"]][[i]] <- data.frame(mean=Yield.MA.model[["None"]]$b,se=Yield.MA.model[["None"]]$se) 
  Yield.MA[["LUI"]][[i]] <- data.frame(mean=Yield.MA.model[["LUI"]]$b,se=Yield.MA.model[["LUI"]]$se) 
  Yield.MA[["Full"]][[i]] <- data.frame(mean=Yield.MA.model[["Full"]]$b,se=Yield.MA.model[["Full"]]$se) 
  Yield.MA[["Select"]][[i]] <- data.frame(mean=Yield.MA.model[["Select"]]$b,se=Yield.MA.model[["Select"]]$se) 
}
 
## load model using mean imputation
path2temp <- set.list[[1]]
load(path2temp %+% "Models.Rdata")

############################################################################
### plot model parameters of a single model using complete cases vs mean imputation vs. the average model parameters of models using single imputations
############################################################################

MeanImp_vs_ModelsAvgImp <- function(model.name){
  
  plot.height <- switch(model.name,"None"=2,"LUI"=6,"Full"=12,"Select"=12)
  offset <- switch(model.name,"None"=0.15,"LUI"=0.15,"Full"=0.2,"Select"=0.2)
  model0 <- data.frame(model.type="Incomplete-case removal",
                       sort.var=letters[1:length(Richness.MA.complete[[model.name]]$b)],
                       level=rownames(Richness.MA.complete[[model.name]]$b),
                       mean=Richness.MA.complete[[model.name]]$b,
                       CI95.ub=Richness.MA.complete[[model.name]]$b+1.96*Richness.MA.complete[[model.name]]$se,
                       CI95.lb=Richness.MA.complete[[model.name]]$b-1.96*Richness.MA.complete[[model.name]]$se)
  
  ## average model parameters based on different imputation chains
  model1 <- data.frame(model.type="Mean imputation",
                       sort.var=letters[1:length(Richness.MA.model[[model.name]]$b)],
                       level=rownames(Richness.MA.model[[model.name]]$b),
                       mean=Richness.MA.model[[model.name]]$b,
                       CI95.ub=Richness.MA.model[[model.name]]$b+1.96*Richness.MA.model[[model.name]]$se,
                       CI95.lb=Richness.MA.model[[model.name]]$b-1.96*Richness.MA.model[[model.name]]$se)
  temp <- Richness.MA[[model.name]][[1]]
  for(i in 2:nchains){
    temp <- cbind(temp,Richness.MA[[model.name]][[i]])
  }
  ## cf Nakagawa&Freckleton2011BehavEcolSociobiol eqn. 3-9
  Q.bar <- rowMeans(temp[,grep("mean",names(temp))]) # mean of parameter estimates
  U.bar <- rowMeans(temp[,grep("se",names(temp))]) # within-imputation variance
  B <- 1/(nchains-1)*rowSums(temp[,grep("mean",names(temp))]-Q.bar) # between-imputation variance
  Tvar <- U.bar + (1+1/nchains)*B # total variance
  
  df <- (nchains-1)*(1+(nchains*U.bar)/((nchains+1)*B))^2
  t.df <- Q.bar/sqrt(Tvar)
  CI95.ub <- Q.bar+qt(p=0.975,df=df)*sqrt(Tvar)
  CI95.lb <- Q.bar-qt(p=0.975,df=df)*sqrt(Tvar)
  
  model.mean <- data.frame(model.type="Multiple imputation",
                           sort.var=letters[1:length(Richness.MA.model[[model.name]]$b)],
                           level = factor(rownames(temp)), 
                           mean=Q.bar, 
                           within.imp.var=U.bar,
                           between.imp.var=B,
                           total.var=Tvar,
                           CI95.ub=CI95.ub,
                           CI95.lb=CI95.lb)              
  write.csv(model.mean, file=path2temp %+% "Richness_" %+% model.name %+% ".csv")
  
  model <- rbind(model0,model1,model.mean[,c("model.type", "sort.var", "level", "mean", "CI95.ub", "CI95.lb")])
  model$offset <- offset
  model$offset[model$model.type=="Mean imputation"] <- 0
  model$offset[model$model.type=="Multiple imputation"] <- -offset
  
  plot <- ggplot(data=model) +
    geom_point(aes(y=as.numeric(sort.var)+offset, x=mean,color=model.type), size=4) +
    geom_segment(aes(y=as.numeric(sort.var)+offset, yend=as.numeric(sort.var)+offset, x=CI95.lb, xend=CI95.ub, color=model.type), size=1.5) +
    geom_vline(aes(xintercept=0), linetype="twodash") +
    scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
    scale_y_discrete(limits=model$level,expand=offset) +
    xlab("RR (Species Richness)") + ylab("")
  print(plot)
  ggsave(plot, file=path2temp %+% "Complete_vs_MeanImp_vs_MulitpleImp_" %+% model.name %+% "_Richness.png", height=plot.height)
  
  model0 <- data.frame(model.type="Incomplete-case removal",
                       sort.var=letters[1:length(Yield.MA.complete[[model.name]]$b)],
                       level=rownames(Yield.MA.complete[[model.name]]$b),
                       mean=Yield.MA.complete[[model.name]]$b,
                       CI95.ub=Yield.MA.complete[[model.name]]$b+1.96*Yield.MA.complete[[model.name]]$se,
                       CI95.lb=Yield.MA.complete[[model.name]]$b-1.96*Yield.MA.complete[[model.name]]$se)
  
  ## average model parameters based on different imputation chains
  model1 <- data.frame(model.type="Mean imputation",
                       sort.var=letters[1:length(Yield.MA.model[[model.name]]$b)],
                       level=rownames(Yield.MA.model[[model.name]]$b),
                       mean=Yield.MA.model[[model.name]]$b,
                       CI95.ub=Yield.MA.model[[model.name]]$b+1.96*Yield.MA.model[[model.name]]$se,
                       CI95.lb=Yield.MA.model[[model.name]]$b-1.96*Yield.MA.model[[model.name]]$se)
  temp <- Yield.MA[[model.name]][[1]]
  for(i in 2:nchains){
    temp <- cbind(temp,Yield.MA[[model.name]][[i]])
  }
  ## cf Nakagawa&Freckleton2011BehavEcolSociobiol eqn. 3-9
  Q.bar <- rowMeans(temp[,grep("mean",names(temp))]) # mean of parameter estimates
  U.bar <- rowMeans(temp[,grep("se",names(temp))]) # within-imputation variance
  B <- 1/(nchains-1)*rowSums(temp[,grep("mean",names(temp))]-Q.bar) # between-imputation variance
  Tvar <- U.bar + (1+1/nchains)*B # total variance
  
  df <- (nchains-1)*(1+(nchains*U.bar)/((nchains+1)*B))^2
  t.df <- Q.bar/sqrt(Tvar)
  CI95.ub <- Q.bar+qt(p=0.975,df=df)*sqrt(Tvar)
  CI95.lb <- Q.bar-qt(p=0.975,df=df)*sqrt(Tvar)
  
  model.mean <- data.frame(model.type="Multiple imputation",
                           sort.var=letters[1:length(Yield.MA.model[[model.name]]$b)],
                           level = factor(rownames(temp)), 
                           mean=Q.bar, 
                           within.imp.var=U.bar,
                           between.imp.var=B,
                           total.var=Tvar,
                           CI95.ub=CI95.ub,
                           CI95.lb=CI95.lb)              
  write.csv(model.mean, file=path2temp %+% "Yield_" %+% model.name %+% ".csv")
  
  model <- rbind(model0,model1,model.mean[,c("model.type", "sort.var", "level", "mean", "CI95.ub", "CI95.lb")])
  model$offset <- offset
  model$offset[model$model.type=="Mean imputation"] <- 0
  model$offset[model$model.type=="Multiple imputation"] <- -offset
  
  plot <- ggplot(data=model) +
    geom_point(aes(y=as.numeric(sort.var)+offset, x=mean,color=model.type), size=4) +
    geom_segment(aes(y=as.numeric(sort.var)+offset, yend=as.numeric(sort.var)+offset, x=CI95.lb, xend=CI95.ub, color=model.type), size=1.5) +
    geom_vline(aes(xintercept=0), linetype="twodash") +
    scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
    scale_y_discrete(limits=model$level,expand=offset) +
    xlab("RR (Yield)") + ylab("")
  print(plot)
  ggsave(plot, file=path2temp %+% "Complete_vs_MeanImp_vs_MulitpleImp_" %+% model.name %+% "_Yield.png", height=plot.height)
}

model.name <- c("None", "LUI","Full","Select")
sapply(model.name,function(x) MeanImp_vs_ModelsAvgImp(x))

temp.richness <- data.frame(model=c("None","LUI","Full","Select"))
temp.yield <- data.frame(model=c("None","LUI","Full","Select"))

for(i in 1:nchains){
  path2temp <- path2temp.list[i]
  temp <- read.csv(path2temp %+% "fit.tab.richness.csv")
  temp.richness <- data.frame(temp.richness,temp[,-1])
  temp <- read.csv(path2temp %+% "fit.tab.yield.csv")
  temp.yield <- data.frame(temp.yield,temp[-1])
}

fit.tab.richness <- data.frame(model=c("None","LUI","Full","Select"))
fit.tab.yield <- data.frame(model=c("None","LUI","Full","Select"))

stats <- c("logLik","deviance","AIC","BIC","AICc","R2.LMM.m","R2.LMM.c")
for(i in stats){
  temp <- temp.richness[,grep(i,names(temp.richness))]
  rownames(temp) <- fit.tab.richness$model
#  apply(temp,1,function(x) hist(x,main=i))
  fit.tab.richness[,i] <- rowMeans(temp)
  temp <- temp.yield[,grep(i,names(temp.yield))]
#  apply(temp,1,function(x) hist(x,main=i))
  fit.tab.yield[,i] <- rowMeans(temp)
}

path2temp <- set.list[[1]]
write.csv(fit.tab.richness,file=path2temp %+% "ModelsAvgImp_fit.tab.richness.csv",row.names=F)
write.csv(fit.tab.yield,file=path2temp %+% "ModelsAvgImp_fit.tab.yield.csv",row.names=F)


####
#knit("Methods_rma.Rmd")
#knit("Results_rma.Rmd")
