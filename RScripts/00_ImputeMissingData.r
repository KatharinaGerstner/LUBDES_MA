library(metafor)
library(mice)
setwd("C:\\Users\\hoppek\\Documents\\GitHub\\LUBDES_MA") #KG
#setwd("C:\\Users\\kambach\\Desktop\\aktuelle Arbeiten\\SESYNC\\myAnalysis") #SK
#setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB

############################################################################
### read .csv file directly downloaded from google docs without any changes
data <- read.csv("Input/LUBDES coding table v2 - 1. Coding Table version 2.csv",na.strings=c("NA",""))

### alternatively (and more elegant) read data DIRECTLY from google docs as described in script -01
#gs_ls() # once authorized, this will list the files you have in GS
#LUBDES_gsheet<- gs_title("LUBDES coding table v2") # load LUBDES  coding table, this crashes sometimes but seems to work as of April 22 2015
#data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2") # consume data from sheet 1
### NOTE: data loaded this way is of classes 'tbl_df' and 'data.frame', rather than only 'data.frame' which is needed for imputation.

#names(data)
# data <- data[-1,] # remove empty row - obsolete
str(data) # check variable types 
#data[,c(30:32,39:41,34,43)] <- apply(data[,c(30:32,39:41,34,43)],2,as.numeric) # covert into numeric variables

#dissmiss studies with missing mean for BD or yield
data = data[-(which(is.na(data$richness.mean))),]
data = data[-(which(is.na(data$yield.mean))),]

################################
### create study-case identifier
data$study.case <- factor(paste(data$Study.ID,data$Case.ID,sep="_"))

#######################################################################
#apply imputation methods, impute BD and yield sd and then calculate se
# TO DO: Adapt package "mi" after Ellington et al 2015
library(mi)
# Adding "latitude..N..S.", "longitude..E..W.", "Land.cover", "PES.category" cause error
data2impute <- data[,c("study.type", "Country", "Land.use...land.cover", "Intensity.broad", "Fertilization", "Irrigation", "Pesticides", "Grazing", "Mowing", "Clear.Cut.y.n.", "Selective.Logging.y.n.", "Partial.Logging.y.n.", "species.group", "trophic.level..species.guild", "richness.mean", "richness.SD", "X..of.samples.for.BD.measure", "sampled.area", "sampled.size.unit", "product", "yield.unit", "yield.mean", "yield.SD", "X..of.samples.for.YD.measure", "sampled.size.area", "sampled.size.unit.1")]
# convert the data.frame to a missing_data.frame, which is an enhanced version of a data.frame that includes metadata about the variables that is essential in a missing data context
mi.df <- missing_data.frame(data2impute) 
# check whether the missing_data.frame constructor function initially guessed the appropriate class for each missing_variable, if not use change() 
show(mi.df)
image(mi.df) ## get a sense of the raw data and their missingness patterns
# use the mi function to do the actual imputation, specify how many independent chains to utilize, how many iterations to conduct, and the maximum amount of time the user is willing to wait for all the iterations of all the chains to finish
imputations <- mi(mi.df, n.iter = 30, n.chains = 4, max.minutes = 20)


imp = mi(data[,c("richness.mean", "richness.SD", "X..of.samples.for.BD.measure")],mi.df,n.imp=3,seed=42)
options(mc.cores =2)
imputations <- mi(mi.df, n.iter = 30, n.chains = 4, max.minutes = 20)
show(imputations)
data.imp <- round(mipply(imputations, mean, to.matrix = TRUE), 3)
data.imp <- pool(richness.SD ~ richness.mean + X..of.samples.for.BD.measure + species.group, data=imputations, m=5)
data.complete <- complete(imputations)
data[,c("richness.mean", "richness.SD")] <- imp
data$richness.SE <- data$richness.SD/sqrt(data$X..of.samples.for.BD.measure)

imp <- mice(data[,c("richness.mean", "richness.SD", "richness.SE")],seed=42)
data[,c("richness.mean", "richness.SD", "richness.SE")] = complete(imp)

imp = mice(data[,c("yield.mean", "yield.SD", "yield.SE")],method="rf",seed=42)
data[,c("yield.mean", "yield.SD", "yield.SE")] = complete(imp)

#############################################################
### merge baseline and increased LUI per study ID, case ID
### reformat table to store baseline and increased in one row

# baseline must be 'no LU', 'baseline LUI' or both c('no LU', 'baseline LUI')
# compile pairs of intensity level: 
# intensity.broad: low, medium, high, no LU
# within.study.intensity: no LU, baseline LUI, increased LUI, single measure within one LUI, pooled measure within one LUI

# two options:
# within-broad LUI comparisons:
# intensity.broad %in% c("low","medium","high") & within.study.intensity %in% c("baseline LUI","increased LUI")  
# levels: low_low, medium_medium, high_high
# between-broad LUI comparisons:
# intensity.broad %in% c("low","medium","high") & within.study.intensity %in% c("single measure within one LUI", "pooled measure within one LUI")
# levels: low_medium, low_high, medium_high

  ES.frame <- data.frame(matrix(ncol=36,nrow=0))
names(ES.frame) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Habitat.Type",        
                       "Product","ES.From.BD","Fertilization","Irrigation","Pesticides",          
                       "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
                       "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude",           
                       "Species.Group","Species.Subgroup","Trophic.Level","Richness.Mean.Low" ,  
                       "Richness.SD.Low","Richness.N.Low","Richness.Plot.Size","Richness.Mean.High","Richness.SD.High",    
                       "Richness.N.High","Yield.Mean.Low","Yield.SD.Low","Yield.N.Low","Yield.Mean.High" ,    
                       "Yield.SD.High","Yield.N.High")

table.sort = function(dat.low,dat.high,low,high){
  data.frame("Study.ID"=dat.low$Study.ID, "Case.ID" =dat.low$Case.ID, 
             "Low.LUI" = low, "High.LUI" = high,
             "Land.use...land.cover" = dat.low$Land.use...land.cover, "Product" = dat.low$Product, "ES.From.BD" =dat.low$ES.measured.from.BD.,
             
             "Fertilization" = paste(dat.low$Fertilization, dat.high$Fertilization, sep="_"), 
             "Irrigation" =paste(dat.low$Irrigation, dat.high$Irrigation, sep="_"),
             "Pesticides" = paste(dat.low$Pesticides, dat.high$Pesticides, sep="_"),
             "Grazing" =paste(dat.low$Grazing, dat.high$Grazing, sep="_"), 
             "Mowing" = paste(dat.low$Mowing, dat.high$Mowing, sep="_"), 
             "Clear.Cut" =paste(dat.low$Clear.Cut.y.n., dat.high$Clear.Cut.y.n., sep="_"),
             "Selective.Logging" = paste(dat.low$Selective.Logging.y.n., dat.high$Selective.Logging.y.n., sep="_"),
             "Partial.Logging" = paste(dat.low$Partial.Logging.y.n., dat.high$Partial.Logging.y.n., sep="_"), 
             "Additional.Treatment" =dat.high$Additional.Treatment,
             
             "Date.Start" =dat.low$Date.of.study..start, "Date.End" =dat.low$Date.of.study..end, 
             "Latitude" =dat.low$latitude..N..S., "Longitude" =dat.low$longitude..E..W.,
             
             "Species.Group" =dat.low$species.group, "Species.Subgroup" =dat.low$species.subgroup.if.provided, "Trophic.Level" =dat.low$trophic.level..species.guild,
             #"Product" = dat.low$product,
             
             "Richness.Mean.Low" =dat.low$richness.mean, "Richness.SD.Low" =dat.low$richness.SD, "Richness.N.Low" =dat.low$X..of.samples.for.BD.measure, 
             "Richness.Plot.Size" =dat.low$sampled.area,
             "Richness.Mean.High" = dat.high$richness.mean, "Richness.SD.High" =dat.high$richness.SD, "Richness.N.High" =dat.high$X..of.samples.for.BD.measure,
             
             "Yield.Mean.Low" =dat.low$yield.mean, "Yield.SD.Low" =dat.low$yield.SD, "Yield.N.Low" =dat.low$X..of.samples.for.YD.measure,
             "Yield.Mean.High" =dat.high$yield.mean, "Yield.SD.High" =dat.high$yield.SD, "Yield.N.High" =dat.high$X..of.samples.for.YD.measure)
}

# TO DO: remove "pooled within one LUI", change l 83: paste(data$study.case,data$species.group,sep="-") to data$study.case
for(i in unique(paste(data$study.case,data$species.group,sep="-"))){
  data.temp = subset(data, paste(data$study.case,data$species.group,sep="-") %in% i)
  # for between broad LUI comparisons
  temp.low = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled within one LUI","pooled measures within one LUI","baseline LUI", "increased LUI") & Intensity.broad   %in% "low")
  temp.medium = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled within one LUI", "pooled measures within one LUI","baseline LUI", "increased LUI") & Intensity.broad   %in% "medium")
  temp.high = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled within one LUI", "pooled measures within one LUI","baseline LUI", "increased LUI") & Intensity.broad   %in% "high")
  
  # for within broad LUI comparisons
  temp.low.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "low")
  temp.low.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "low")
  temp.medium.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "medium")
  temp.medium.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "medium")
  temp.high.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "high")
  temp.high.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "high")
  
  # table.sort broken? MB
  if((nrow(temp.low) + nrow (temp.medium)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.low,temp.medium,"low","medium"))}
  if((nrow(temp.low) + nrow (temp.high)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.low,temp.high,"low","high"))}
  if((nrow(temp.medium) + nrow (temp.high)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.medium,temp.high,"medium","high"))}
  
  if((nrow(temp.low.base) + nrow (temp.low.increase)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.low.base,temp.low.increase,"low","low"))}
  if((nrow(temp.medium.base) + nrow (temp.medium.increase)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.medium.base,temp.medium.increase,"medium","medium"))}
  if((nrow(temp.high.base) + nrow (temp.high.increase)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.high.base,temp.high.increase,"high","high"))}
  
  print(i)
}

ES.frame$LUI.range <- paste(ES.frame$Low.LUI,ES.frame$High.LUI,sep="-")
ES.frame$LUI.range[ES.frame$LUI.range %in% c("low-low","medium-medium","high-high")] <- 0
ES.frame$LUI.range[ES.frame$LUI.range %in% c("low-medium","medium-high")] <- 1
ES.frame$LUI.range[ES.frame$LUI.range %in% c("low-high")] <- 2

ES.frame$Study.Case <- factor(paste(ES.frame$Study.ID,ES.frame$Case.ID,sep="_"))
#############################
### calculate RR effect sizes
  
#Effect size calculation
ES.frame[,c("Richness.Log.RR","Richness.Log.RR.Var")] = 
  escalc("ROM",data= ES.frame, append =F,
         m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
         sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
         n2i = Richness.N.Low, n1i = Richness.N.High)

ES.frame[,c("Yield.Log.RR","Yield.Log.RR.Var")] = 
  escalc("ROM",data= ES.frame, append =F,
         m2i = Yield.Mean.Low, m1i = Yield.Mean.High,
         sd2i = Yield.SD.Low, sd1i = Yield.SD.High,
         n2i = Yield.N.Low, n1i = Yield.N.High)

ES.frame[,c("Richness.SMD","Richness.SMD.Var")] =
  escalc("SMDH",data= ES.frame, append =F,
         m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
         sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
         n2i = Richness.N.Low, n1i = Richness.N.High)

ES.frame[,c("Yield.SMD","Yield.SMD.Var")] = 
  escalc("SMDH",data= ES.frame, append = F,
         m2i = Yield.Mean.Low, m1i = Yield.Mean.High,
         sd2i = Yield.SD.Low, sd1i = Yield.SD.High,
         n2i = Yield.N.Low, n1i = Yield.N.High)

write.csv(ES.frame, "Input/ES_table.csv")

