### Some general comments on the structure of the LUBDES_MA scripts:
### * Each module (-01, 00, 01 ...) builds on the previous ones and does NOT work standalone - this avoids redundancies.
### * Exceptions to that rule are -02 and -03 which should not be run by anyone other than MB.
### * Data, functions etc. will be carried over from one module to another. Saving and loading of interim outputs is unwanted. 
### * In case interim data needs to be saved it must NOT be saved in the git directory. This will avoid that accidentally our whole dataset is visible to everyone on the internet. 
### * All needed output files (tables, plots etc.) must NOT be saved in the git directory but in the LUBDES_MA dropbox folders instead.
### * Hidden files like .rhistory .oauth etc must NOT be commited to github. This avoids errors. Also, uploading the authorization token for google docs to github might allow access to google accounts by hackers (presumably, #MB)
### * The data input comes directly from google docs. loading .csv is deprecated.
### * created functions should be own sub-sections (i.e. 01.03 blabla function)
###
### Overall structure of the modules is:
### [-03.1. To extract zotero citation data into excel.]
### [-02.1. set working directory to create folders in]
### [-02.2. create directory structure based on study IDs and place an empty note file within]
### -01.1. set the working directory
### -01.2. load all libraries needed for subsequent analysis
### -01.3. load data directly from google docs
### -01.4. adapt data structure
### 00.1. impute missing data using mice package
### 00.2. impute missing data using mi package 
### 01.1. table.sort function
### 01.2. Compile ES frame
### 01.3. Calculate response ratio effect sizes
### 01a.1. Intersect studies with global maps
### 02.1. Prepare data analysis
### 02.2. LMM.MA.fit function
### 02.3. Analysis without moderators
### 02.4. Analysis with moderators
### 03.1. Specify output directory in the dropbox or other local folder - NOT in the git directory!
### 03.2. Plot map of studies
### 03.3. Plot cross-diagrams

