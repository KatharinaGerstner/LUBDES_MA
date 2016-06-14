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
### 03 03_ImputeMissingData.r
### 03.1. impute missing data
###
### 04 04_CompileESframe.R
### 04.1. Compile ES frame
### 04.2. Calculate response ratio effect sizes
###
### 05 05_AddMapDataToESframe.R
### 05.1. Intersect studies with global maps of WWF_REALMs Ecoregions, combine to coarser classes
### 05.2. Intersect studies with potential NPP
###
### 06 06_DataPreparation4Analysis.R
### 06a.1. Remove cases with zero variances, pseudo-replicates, redundant LUI.range.level comparisons 
### 06a.2. remove columns not needed for the analysis, unify names
### 06a.3 load functions for the data analysis
###
### 07 07_DescriptiveStatsOfESFrame.r
### 07.1 Protocol structure and summary of variables in the ES.frame
### 07.2 Plot Histograms of all variables in the ES.frame 
### 07.3. Plot map of studies (full map)
### 07.4. Plot six maps by intensity classes
###
### 08 Run Frequentist/Bayesian Analysis
### overall structure of the scripts
### 1. define the model
### 2. run null model, i.e. grand mean
### 3. run full model
### 4. run model selection
###
### 08 08a_DataAnalysis.R
### 07.1. Prepare data analysis
### 07.2. Analysis without moderators
### 07.3. Analysis with moderators
### 07.4. Analysis with moderators for no LU vs low/medium/high LU
###
### 08 08b BayesianAnalysis
### 08b.1_BayesianAnalysis_1.R # fixed effects only
### 08b.2_BayesianAnalysis_2.R # fixed and random effects of study and study-case
### 08b.3_BayesianAnalysis_3.R # fixed and random effects of study and study-case, and non-independence from relatedness of LUI comparisons within one study-case
### 08b.4_BMASelect.R # fixed and random effects of study and study-case, and non-independence from relatedness of LUI comparisons within one study-case AND bayesian model selection using DIC

###
### 08.2 08.2_Plot_CatWhiskers.r  
### 08.2.1. Plot CatWhisker plots
###
### 08.3 08.3_Plot_forest_plots.r
### 08.3.1. plot forest plots for each LUI-comparison class
###
### 09 09_ModelDiagnostics.r
###
### 10 10_UncertaintyAnalysis.R
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


############################################################################
### DATA ANALYSIS
### save results in separate folders specified using path2temp
############################################################################

############################################################################
### Analysis with complete cases
############################################################################
path2temp <- set.list[[1]] %+% "CompleteCases/"
path2wd <- set.list[[2]]
source(path2wd %+% "01_load_libraries_and_functions.r")
source(path2wd %+% "02_load_table_directly_from_google.R")
dataimp <- data
dataimp[,c("yield.SD.is.imputed","richness.SD.is.imputed")] <- "no"

### Compile raw data
source(path2wd %+% "04_CompileESframe.R")
source(path2wd %+% "05_AddMapDataToESframe.R")
save(data,dataimp,ES.frame,file=path2temp %+% "SavedData.Rdata")

### some additional data preparation steps
load(path2temp %+% "SavedData.Rdata")
source(path2wd %+% "06_DataPreparation4Analysis.R")

### FREQUENTIST ANALYSIS
source(path2wd %+% "08a_DataAnalysis.R") # saves model into path2temp %+% "Models.Rdata"

### Plot coefficients
load(path2temp %+% "Models.Rdata")
source(path2wd %+% "09.1_Plot_model_coeffs.r") # cross plots for LUI range level and forest plots for model 

############################################################################
### Analysis with mean imputation
############################################################################
path2temp <- set.list[[1]]
path2wd <- set.list[[2]]

### helper function to combine strings
"%+%" <- function(x,y)paste(x,y,sep="")

### load libraries, functions and google sheets 
#source(path2wd %+% "-01_load+analyse_screeningtable.r")
source(path2wd %+% "01_load_libraries_and_functions.r")
source(path2wd %+% "02_load_table_directly_from_google.R")

### Compile raw data
source(path2wd %+% "03_ImputeMissingData_mice_mean.r")
source(path2wd %+% "04_CompileESframe.R")
source(path2wd %+% "05_AddMapDataToESframe.R")
save(data,dataimp,ES.frame,file=path2temp %+% "SavedData.Rdata")

### some additional data preparation steps
load(path2temp %+% "SavedData.Rdata")
source(path2wd %+% "06_DataPreparation4Analysis.R") 
knit("07.1_DescriptiveStatsOfESframe.Rmd")

### FREQUENTIST ANALYSIS
source(path2wd %+% "08a_DataAnalysis.R") # saves model into path2temp %+% "Models.Rdata"

### Plot coefficients
load(path2temp %+% "Models.Rdata")
source(path2wd %+% "09.1_Plot_model_coeffs.r") # cross plots for LUI range level and forest plots for model 


############################################################################
### Analysis with multiple imputation
############################################################################
path2temp.list <- c(set.list[[1]] %+% "imp" %+% 1:10 %+% "/")
sapply(path2temp.list,function(x) dir.create(x, showWarnings = F, recursive = FALSE, mode = "0777"))
### load libraries, functions and google sheets 
source(path2wd %+% "01_load_libraries_and_functions.r")
source(path2wd %+% "02_load_table_directly_from_google.R")
nchains <- 10 # number of chains used for imputation
source(path2wd %+% "03_ImputeMissingData_mice_loops.r")

for(i in 1:nchains){
  path2temp <- path2temp.list[i]
  print(path2temp)
  load(path2temp %+% "dataimp.Rdata")

  ### Compile raw data
  source(path2wd %+% "04_CompileESframe.R")
  source(path2wd %+% "05_AddMapDataToESframe.R")
  save(data,dataimp,ES.frame,file=path2temp %+% "SavedData.Rdata")
  
  ### some additional data preparation steps
  load(path2temp %+% "SavedData.Rdata")
  source(path2wd %+% "06_DataPreparation4Analysis.R")
   
  ### FREQUENTIST ANALYSIS
  source(path2wd %+% "08a_DataAnalysis.R") # saves model into path2temp %+% "Models.Rdata"
  
  ### Plot coefficients
  load(path2temp %+% "Models.Rdata")
  source(path2wd %+% "09.1_Plot_model_coeffs.r") # cross plots for LUI range level and forest plots for model 
}

source(path2wd %+% "07.2_Plot_forest_plots_loops.R")

