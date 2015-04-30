library(metafor)
library(mice)
setwd("C:\\Users\\kambach\\Desktop\\aktuelle Arbeiten\\SESYNC\\myAnalysis") #KG
#setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB

############################################################################
### read .csv file directly downloaded from google docs without any changes
data <- read.csv("LUBDES coding table v2 - 1. Coding Table version 2.csv",skip=2,na.strings=c("NA",""))
#names(data)
data <- data[-1,] # remove empty row
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
imp = mi(data[,c(31:32,35)],mi.info(data[,c(31:32,35)]),n.imp=3,seed=42)

data[,c(31:32,35)] = imp

imp = mice(data[,c(41:42,45)],m=,method="pmm",seed=42)
data[,c(41:42,45)] = complete(imp)

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

  ES.frame <- data.frame(matrix(ncol=37,nrow=0))
names(ES.frame) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Habitat.Type",        
                       "PES.Category","ES.From.BD","Fertilization","Irrigation","Pesticides",          
                       "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
                       "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude",           
                       "Species.Group","Species.Subgroup","Trophic.Level","Product","Richness.Mean.Low" ,  
                       "Richness.SD.Low","Richness.N.Low","Richness.Plot.Size","Richness.Mean.High","Richness.SD.High",    
                       "Richness.N.High","Yield.Mean.Low","Yield.SD.Low","Yield.N.Low","Yield.Mean.High" ,    
                       "Yield.SD.High","Yield.N.High")

table.sort = function(dat.low,dat.high,low,high){
  data.frame("Study.ID"=dat.low$Study.ID, "Case.ID" =dat.low$Case.ID, 
             "Low.LUI" = low, "High.LUI" = high,
             "Habitat.Type" = dat.low$Habitat.type, "PES.Category" = dat.low$PES.category, "ES.From.BD" =dat.low$ES.measure.measured.from.BD.,
             
             "Fertilization" = paste(dat.low$Fertilization, dat.high$Fertilization, sep="_"), 
             "Irrigation" =paste(dat.low$Irrigation, dat.high$Irrigation, sep="_"),
             "Pesticides" = paste(dat.low$Pesticides, dat.high$Pesticides, sep="_"),
             "Grazing" =paste(dat.low$Grazing, dat.high$Grazing, sep="_"), 
             "Mowing" = paste(dat.low$Mowing, dat.high$Mowing, sep="_"), 
             "Clear.Cut" =paste(dat.low$Clear.Cut, dat.high$Clear.Cut, sep="_"),
             "Selective.Logging" = paste(dat.low$Selective.Logging, dat.high$Selective.Logging, sep="_"),
             "Partial.Logging" = paste(dat.low$Partial.Logging, dat.high$Partial.Logging, sep="_"), 
             "Additional.Treatment" =dat.high$Additional.Treatment,
             
             "Date.Start" =dat.low$Date.of.study..start, "Date.End" =dat.low$Date.of.study..end, 
             "Latitude" =dat.low$latitude..N..S., "Longitude" =dat.low$longitude..E..W.,
             
             "Species.Group" =dat.low$species.group, "Species.Subgroup" =dat.low$species.subgroup.if.provided, "Trophic.Level" =dat.low$trophic.level..species.guild,
             "Product" = dat.low$product,
             
             "Richness.Mean.Low" =dat.low$richness.mean, "Richness.SD.Low" =dat.low$richness.SD, "Richness.N.Low" =dat.low$X..of.Plots.for.BD.measure, 
             "Richness.Plot.Size" =dat.low$Plot.size,
             "Richness.Mean.High" = dat.high$richness.mean, "Richness.SD.High" =dat.high$richness.SD, "Richness.N.High" =dat.high$X..of.Plots.for.BD.measure,
             
             "Yield.Mean.Low" =dat.low$yield.mean, "Yield.SD.Low" =dat.low$yield.SD, "Yield.N.Low" =dat.low$X..of.Plots.for.YD.measure,
             "Yield.Mean.High" =dat.high$yield.mean, "Yield.SD.High" =dat.high$yield.SD, "Yield.N.High" =dat.high$X..of.Plots.for.YD.measure)
}

# TO DO: remove "pooled within one LUI", change l 83: paste(data$study.case,data$species.group,sep="-") to data$study.case
for(i in unique(paste(data$study.case,data$species.group,sep="-"))){
  data.temp = subset(data, paste(data$study.case,data$species.group,sep="-") %in% i)
  # for between broad LUI comparisons
  temp.low = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled within one LUI","pooled measures within one LUI") & Intensity.broad   %in% "low")
  temp.medium = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled within one LUI", "pooled measures within one LUI") & Intensity.broad   %in% "medium")
  temp.high = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled within one LUI", "pooled measures within one LUI") & Intensity.broad   %in% "high")
  
  # for within broad LUI comparisons
  temp.low.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "low")
  temp.low.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "low")
  temp.medium.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "medium")
  temp.medium.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "medium")
  temp.high.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "high")
  temp.high.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "high")
  
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
  escalc("SMDH",data= ES.frame,, append =F,
         m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
         sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
         n2i = Richness.N.Low, n1i = Richness.N.High)

ES.frame[,c("Yield.SMD","Yield.SMD.Var")] = 
  escalc("SMDH",data= ES.frame, append = F,
         m2i = Yield.Mean.Low, m1i = Yield.Mean.High,
         sd2i = Yield.SD.Low, sd1i = Yield.SD.High,
         n2i = Yield.N.Low, n1i = Yield.N.High)

write.csv(ES.frame, "ES_table_test.csv")

