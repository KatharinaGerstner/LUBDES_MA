############################################################################
### Purpose of this skript module 00 is to:
###
### 00.1. impute missing data using mice package
### 00.2. impute missing data using mi package 
###
### General comments:
### * 00.2 is currently not working!
###
### Authors: KG, MB, SK ...
############################################################################


############################################################################
### 00.1. impute missing data using mice package
### 
############################################################################

### impute also zero SD's as we can't work with that in the analysis
data$richness.SD[data$richness.SD==0] <- NA
data$yield.SD[data$yield.SD==0] <- NA

### impute ES and BD separately
### after a long discussion with Ralf, lots of trial runs manipulating N of studies that had no data to be 
### imputed, MB and RS conclude that using mi in its current form is not a good idea.
### 2do: calculate relative percentage of SD of mean and apply this to missing SD means, also do the +1 and -1 SD to test if this is robust

# imp <- mice(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure")])
# data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure")] <- complete(imp)
# 
# imp <- mice(data[,c("yield.mean", "yield.SD", "X..of.samples.for.YD.measure")])
# data[,c("yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] <- complete(imp)


# 
# ### impute
# imp <- mice(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")])
# data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] <- complete(imp)

### Obsolete?
# ### calculate SE for richness and yield mean
# data$richness.SE <- data$richness.SD/sqrt(data$X..of.samples.for.BD.measure)
# data$yield.SE <- data$yield.SD/sqrt(data$X..of.samples.for.YD.measure)
# 
# ### check results, e.g. SD must be positive or SE will be NaN
# summary(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]) # does not produce negative SDs

############################################################################
### 00.2. impute missing data using mi package
###
### Currently not working!
############################################################################

# ### apply imputation methods, impute BD and yield sd and then calculate se
# ### specify data frame for imputation
# ### Adding "latitude..N..S.", "longitude..E..W.", "Land.cover", "PES.category" cause error
# ### TO DO: standardize sampled.size.unit for BD and yield, so that sampled.area has the same unit overall, then sampled.size.unit becomes obsolete 
# #data2impute <- data[,c("study.type", "Country", "Land.use...land.cover", "Intensity.broad", "Fertilization", "Irrigation", "Pesticides", "Grazing", "Mowing", "Clear.Cut.y.n.", "Selective.Logging.y.n.", "Partial.Logging.y.n.", "species.group", "trophic.level..species.guild", "richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "sampled.area", "sampled.size.unit", "product", "yield.unit", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure", "sampled.size.area", "sampled.size.unit.1")]
# data2impute <- data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]
# 
# ### convert the data.frame to a missing_data.frame, which is an enhanced version of a data.frame that includes metadata about the variables that is essential in a missing data context
# mi.df <- missing_data.frame(data2impute) 
# 
# ### check whether the missing_data.frame constructor function initially guessed the appropriate class for each missing_variable, if not use change() 
# show(mi.df)
# mi.df <- change_type(mi.df, c("X..of.samples.for.BD.measure","X..of.samples.for.YD.measure"), to="count")
# # mi.df <- change_link(mi.df, c("richness.SD","yield.SD"), to="log") # to ensure SD is estimated as positive number, however this does not work and cause errror when calling the mi-function: Error in checkForRemoteErrors(val) : 2 nodes produced errors; first error: cannot find valid starting values: please specify some
# 
# ### get a sense of the raw data and their missingness patterns
# image(mi.df) 
# 
# ### use the mi function to do the actual imputation, specify how many independent chains to utilize, how many iterations to conduct, and the maximum amount of time the user is willing to wait for all the iterations of all the chains to finish
# imputations <- mi(mi.df, n.iter = 20, n.chains = 4, max.minutes = 20) 
# 
# ### check convergence by calculating the mean over imputation chains
# (data.imp <- round(mipply(imputations, mean, to.matrix = TRUE), 3))
# 
# ###  Complete the missing data frame, take results fo the mth imputation chain
# data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")] <- complete(imputations, m=1)[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")]
# summary(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure")])
# 
# ### calculate SE for richness and yield mean
# data$richness.SE <- data$richness.SD/sqrt(data$X..of.samples.for.BD.measure)
# data$yield.SE <- data$yield.SD/sqrt(data$X..of.samples.for.YD.measure)
