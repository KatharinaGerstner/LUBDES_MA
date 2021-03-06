Some general comments on the structure of the LUBDES_MA scripts:
* Each module (-01, 00, 01 ...) builds on the previous ones and does NOT work standalone - this avoids redundancies.
* Exceptions to that rule are -03 to -01 which should not be run by anyone other than MB.
* Data, functions etc. will be carried over from one module to another. Saving and loading of interim outputs is done into path2temp. 
* In case interim data needs to be saved it must NOT be saved in the git directory. This will avoid that accidentally our whole dataset is visible to everyone on the internet. 
* All needed output files (tables, plots etc.) must NOT be saved in the git directory but in path2temp instead.
* Hidden files like .rhistory .oauth etc must NOT be commited to github. This avoids errors. Also, uploading the authorization token for google docs to github might allow access to google accounts by hackers (presumably, #MB)
* The data input comes directly from google docs. loading .csv is deprecated.
* created functions should be own sub-sections (i.e. 01.03 blabla function)

Overall structure of the modules is:  
-04 -04_create_csv_from_zotero_missing_papers.R   
[-04.1. Extract zotero citation data of missing papers into excel.]  
  
-03 -03_create_csv_from_Zotero.R   
[-03.1. To extract zotero citation data into excel.]  
  
-02 -02_create_folders_for_papers.R    
[-02.1. set working directory to create folders in]   
[-02.2. create directory structure based on study IDs and place an empty note file within]   
  
-01 -01_load+analyse_screeningtable.r   
[-01.1. load screening data directly from google docs]   
[-01.2. plot pie charts about statistics]   
  
00 00_initialize_directories.r  
00.1. set the working and temporary directories  
00.2. source all relevant R scripts  
  
01 01_load_libraries_and_functions.r  
01.1. load all libraries needed for subsequent analysis  
01.2. load all self-written functions needed for subsequent analysis  
  
02 02_load_table_directly_from_google.R  
02.1. load data directly from google docs  
02.2. adapt data structure  
  
03 03_ImputeMissingData_mice_mean.r  
03.1. impute missing data using the mice package and the mean of 50 imputation chains  
03.2. plot imputed missing data  
  
04 04_CompileESframe.R  
04.1. Compile ES frame  
04.2. Calculate response ratio effect sizes  
  
05 05_AddMapDataToESframe.R  
05.1. Intersect studies with main climate zones  
05.2. Intersect studies with coarse classes of land-use history  
  
06 06_DataPreparation4Analysis.R  
06.1. Remove cases with zero variances, pseudo-replicates, redundant LUI.range.level comparisons   
06.2. remove columns not needed for the analysis, unify names  
06.3. save rawdata as table in a word doc  
  
07 07.1_DescriptiveStatsOfESFrame.Rmd  
07.1. Protocol structure and summary of variables in the ES.frames for richness and yield  
07.2. Plot Histograms of all variables in the ES.frame   
07.3. Plot maps of added data  
07.4. Plot map of studies  
07.5. Forest Plots of study.cases per LUI.range.level  
  
08 08a_DataAnalysis.R  
08.1. define list of moderators  
08.2. Fit models for richness and yield  
08.3. extract fit statistics  
  
09 09.1_Plot_model_coeffs.r  
09.1.1. plot raw data + grand mean  
09.1.2. plot LUI cross diagrams  
  
09 09.2_Plot_LUI.SGP.crossDiagrams.r  
09.2.1. Predictions for richness  
09.2.2. Predictions for yield  
09.2.3. Join and save predictions for richness and yield  
09.2.4. Map predictions facetted by Product and/or Species.Group  
  
09 09.3_Preds_Select.r  
09.3.1. Predictions for richness for model "select"  
09.3.2. Predictions for yield for model "select"  
09.3.3. join and save predictions for richness and yield for models "select"  
  
10 10_ModelDiagnostics.r  
10.1. relationship residuals vs model fit, non-linear?, homogeneity of variances?   
10.2. normality of resiuals  
10.3. influential points  
  