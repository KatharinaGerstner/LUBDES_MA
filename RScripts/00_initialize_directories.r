############################################################################
### Purpose of this skript module 00 is to:
###
### Some general comments on the structure of the LUBDES_MA scripts:
### * Each module (-01, 00, 01 ...) builds on the previous ones and does NOT work standalone - this avoids redundancies.
### * Exceptions to that rule are -03 to -01 which should not be run by anyone other than MB.
### * Data, functions etc. will be carried over from one module to another. Saving and loading of interim outputs is done into path2temp. 
### * In case interim data needs to be saved it must NOT be saved in the git directory. This will avoid that accidentally our whole dataset is visible to everyone on the internet. 
### * All needed output files (tables, plots etc.) must NOT be saved in the git directory but in path2temp instead.
### * Hidden files like .rhistory .oauth etc must NOT be commited to github. This avoids errors. Also, uploading the authorization token for google docs to github might allow access to google accounts by hackers (presumably, #MB)
### * The data input comes directly from google docs. loading .csv is deprecated.
### * created functions should be own sub-sections (i.e. 01.03 blabla function)
###
### Overall structure of the modules is:
### -03 -03_create_csv_from_Zotero.R
### [-03.1. To extract zotero citation data into excel.]
### -02 -02_create_folders_for_papers.R
### [-02.1. set working directory to create folders in]
### [-02.2. create directory structure based on study IDs and place an empty note file within]
### -01 -01_load+analyse_screeningtable.r
### [-01.1. load screening data directly from google docs]
### [-01.2. plot pie charts about statistics]
### 00 00_initialize_directories.r
### 00.1. set the working and temporary directories
### 00.2. define global helper functions
### 00.3. source all relevant R scripts
### 01 01_load_libraries.r
### 01.1. load all libraries needed for subsequent analysis
### 02 02_load_table_directly_from_google.R
### 02.1. load data directly from google docs
### 02.2. adapt data structure
### 03 03_ImputeMissingData.r
### 03.1. impute missing data using means and SDs
### 03.2. impute missing data using mi package 
### 03.3. impute missing data using mice package
### 04 04_CompileESframe.R
### 04.1. table.sort function
### 04.2. Compile ES frame
### 04.3. Calculate response ratio effect sizes
### 05 05_AddMapDataToESframe.R
### 05.1. Intersect studies with global maps of WWF_REALMs Ecoregions
### 05.2. Intersect studies with global maps of climate zones (Köppen-Geiger)
### 05.2. Intersect studies with global maps of GDP per capita
### 05.3. Intersect studies with annual mean radiation (Climond)
### 05.4. Intersect studies with gross capital stock in agriculture
### 05.5. Intersect studies with Agricultural intensity (efficiency) in the neighborhood
### 05.6. Intersect studies with Global Habitat Heterogeneity, Dissimilarity
### 05.7. Intersect studies with Land-use history
### 05.8. Intersect studies with Population density
### 06 06_DescriptiveStatsOfESFrame.r
### 06.1 Protocol structure and summary of variables in the ES.frame
### 06.2 Plot Histograms of all variables in the ES.frame 
### 06.3 Protocol structure and summary of variables in the ES.frame.noLU
### 06.4 Plot Histograms of all variables in the ES.frame.noLU 
### 07 07_DataAnalysis.R
### 07.1. Prepare data analysis
### 07.2. Analysis without moderators
### 07.3. Analysis with moderators
### 08 08_Plotting.r
### 08.1. Plot map of studies
### 08.2. Plot cross-diagrams
### 08.3. Forest plots for noLU vs low/medium/high LU
### 09 09_ModelDiagnostics.r
### 10 10_UncertaintyAnalysis.R
###
### Authors: MB, KG, ...
############################################################################

############################################################################
### 00.1. set the working and temporary directories
###
### needs to be adjusted individually
############################################################################

cn <- Sys.info()["nodename"]

if (cn == "cle162m-10.local")
{
  path2temp <- "/Users/rseppelt/Documents/Projekte/LU-BD-ES (SESYNC, Ralf)/Temp" 
  path2wd <- "/Users/rseppelt/Documents/git/LUBDES_MA/RScripts/" 
} else if (cn == "UCBTTNE-LT"){
  path2wd <- "C:/Users/Tim/Documents/LUBDES_MA/RScripts/" #TN
  path2temp <- "C:/Users/Tim/Documents/LUBDES_MA_Out/" #TN
  
} else if (cn == "juro-MacBookPro"){
  path2wd <- "/home/juro/git/LUBDES_MA/RScripts/" #MB
  path2temp <- "/home/juro/tmp/" #MB
  
}else
{
  path2wd <- "C:/Users/hoppek/Documents/GitHub/LUBDES_MA/RScripts/" #KG
  path2temp <- "C:/Users/hoppek/Documents/temp" #KG
  
  
}

############################################################################
### 00.2. define global helper functions
###
### 
############################################################################

### helper function to combine strings
"%+%" <- function(x,y)paste(x,y,sep="")

############################################################################
### 00.3. source all relevant R scripts
###
### 
############################################################################

#source(path2wd %+% "-01_load+analyse_screeningtable.r")
source(path2wd %+% "01_load_libraries.r")
source(path2wd %+% "02_load_table_directly_from_google.R")
source(path2wd %+% "03_ImputeMissingData.r")
source(path2wd %+% "04_CompileESframe.R")
source(path2wd %+% "05_AddMapDataToESframe.R")
#source(path2wd %+% "06_DescriptiveStatsOfESframe.r")
source(path2wd %+% "07_DataAnalysis.R")
source(path2wd %+% "08_Plotting.r")
#source(path2wd %+% "09_ModelDiagnostics.R") # not yet working
#source(path2wd %+% "10_UncertaintyAnalysis.R") # not yet working

