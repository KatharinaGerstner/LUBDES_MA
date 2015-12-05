############################################################################
### Purpose of this skript module 04 is to:
###
### 04.1. table.sort function
### 04.2. Compile ES frame
### 04.3. Calculate response ratio effect sizes
###
### General comments:
###
### Authors: KG, MB, SK ...
############################################################################

############################################################################
### 04.1. table.sort function
### 
############################################################################

### table.sort function to restructure dataimp

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
             "Latitude" =as.numeric(dat.low$latitude..N..S.), "Longitude" =as.numeric(dat.low$longitude..E..W.),
             "Country.Code" = dat.low$Country,
             
             "Species.Group" =dat.low$species.group, "Species.Subgroup" =dat.low$species.subgroup.if.provided, "Trophic.Level" =dat.low$trophic.level..species.guild,
             #"Product" = dat.low$product,
             
             "Richness.Mean.Low" = as.numeric(dat.low$richness.mean), "Richness.SD.Low" =as.numeric(dat.low$richness.SD), "Richness.N.Low" =as.numeric(dat.low$X..of.samples.for.BD.measure), 
             "Richness.Plot.Size" = as.numeric(dat.low$sampled.area),
             "Richness.Mean.High" = as.numeric(dat.high$richness.mean), "Richness.SD.High" = as.numeric(dat.high$richness.SD), "Richness.N.High" = as.numeric(dat.high$X..of.samples.for.BD.measure),             
             "Yield.Mean.Low" = as.numeric(dat.low$yield.mean), "Yield.SD.Low" = as.numeric(dat.low$yield.SD), "Yield.N.Low" = as.numeric(dat.low$X..of.samples.for.YD.measure),
             "Yield.Mean.High" = as.numeric(dat.high$yield.mean), "Yield.SD.High" = as.numeric(dat.high$yield.SD), "Yield.N.High" = as.numeric(dat.high$X..of.samples.for.YD.measure))
}

############################################################################
### 04.2. Compile ES frame
### 
############################################################################

### create empty ES.frame

ES.frame <- data.frame(matrix(ncol=37,nrow=0))
names(ES.frame) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Habitat.Type",        
                     "Product","ES.From.BD","Fertilization","Irrigation","Pesticides",          
                     "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
                     "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude", "Country.Code",          
                     "Species.Group","Species.Subgroup","Trophic.Level","Richness.Mean.Low" ,  
                     "Richness.SD.Low","Richness.N.Low","Richness.Plot.Size","Richness.Mean.High","Richness.SD.High",    
                     "Richness.N.High","Yield.Mean.Low","Yield.SD.Low","Yield.N.Low","Yield.Mean.High" ,    
                     "Yield.SD.High","Yield.N.High")

ES.frame.noLU <- data.frame(matrix(ncol=33,nrow=0))
names(ES.frame.noLU) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Habitat.Type",        
                     "Product","ES.From.BD","Fertilization","Irrigation","Pesticides",          
                     "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
                     "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude", "Country.Code",          
                     "Species.Group","Species.Subgroup","Trophic.Level","Richness.Mean.Low" ,  
                     "Richness.SD.Low","Richness.N.Low","Richness.Plot.Size","Richness.Mean.High","Richness.SD.High", "Richness.N.High")

# TO DO: remove "pooled within one LUI", change l 83: paste(dataimp$study.case,dataimp$species.group,sep="-") to dataimp$study.case

### re-build dataimp to ES.frame using table.sort function

for(i in unique(dataimp$study.case)){
  
  print(i)
  
  ### remove some studies from further analysis as these cause error as they have same study.case and species group but diff. species subgroups. TO DO: Decide how to deal with that issue.
#  if(i %in% c("8235-Norvez2013_1","8002-Lohmus2013_1",paste("9078-Hautier2014_",1:12,sep=""),"516-Higgins1999_1",paste("4212-Kembel2008_",1:3,sep=""))){
#    print("ERROR. Omit from analysis.")
#    next}

  data.temp = subset(dataimp, dataimp$study.case %in% i)

  ### for nu LUI
  temp.noLU <- subset(data.temp, Within.study.Intensity %in% "no LU")
  temp.lowLU = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled measures within one LUI") & Intensity.broad %in% "low")
  temp.mediumLU = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled measures within one LUI") & Intensity.broad %in% "medium")
  temp.highLU = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled measures within one LUI") & Intensity.broad %in% "high")
  
  if(nrow(temp.noLU) > 1 | nrow(temp.lowLU) >1 | nrow(temp.mediumLU) >1 | nrow(temp.highLU) >1){ 
    print(paste(i, "single and pooled measures within one LUI"))
    next
  }

  if((nrow(temp.noLU) + nrow (temp.lowLU)) == 2){
    ES.frame.noLU = rbind(ES.frame.noLU,table.sort(temp.noLU,temp.lowLU,"no LU","low"))}
  if((nrow(temp.noLU) + nrow (temp.mediumLU)) == 2){
    ES.frame.noLU = rbind(ES.frame.noLU,table.sort(temp.noLU,temp.mediumLU,"no LU","medium"))}
  if((nrow(temp.noLU) + nrow (temp.highLU)) == 2){
    ES.frame.noLU = rbind(ES.frame.noLU,table.sort(temp.noLU,temp.highLU,"no LU","high"))}

  ### for within broad LUI comparisons
  temp.low.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "low")
  temp.low.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "low")
  temp.medium.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "medium")
  temp.medium.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "medium")
  temp.high.base = subset(data.temp, Within.study.Intensity %in% "baseline LUI" & Intensity.broad   %in% "high")
  temp.high.increase = subset(data.temp, Within.study.Intensity %in% "increased LUI" & Intensity.broad   %in% "high")

  if((nrow(temp.low.base) + nrow (temp.low.increase)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.low.base,temp.low.increase,"low","low"))}
  if((nrow(temp.medium.base) + nrow (temp.medium.increase)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.medium.base,temp.medium.increase,"medium","medium"))}
  if((nrow(temp.high.base) + nrow (temp.high.increase)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.high.base,temp.high.increase,"high","high"))}
  
  ### for between broad LUI comparisons
  temp.low = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled measures within one LUI") & Intensity.broad %in% "low")
  temp.medium = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled measures within one LUI") & Intensity.broad %in% "medium")
  temp.high = subset(data.temp, Within.study.Intensity %in% c("single measure within one LUI","pooled measures within one LUI") & Intensity.broad %in% "high")
  
  if(nrow(temp.low) >1 | nrow(temp.medium) >1 | nrow(temp.high) >1){ 
    print(paste(i, "single and pooled measures within one LUI"))
    next
  }

  if((nrow(temp.low) + nrow (temp.medium)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.low,temp.medium,"low","medium"))}
  if((nrow(temp.low) + nrow (temp.high)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.low,temp.high,"low","high"))}
  if((nrow(temp.medium) + nrow (temp.high)) == 2){
    ES.frame = rbind(ES.frame,table.sort(temp.medium,temp.high,"medium","high"))}

}

ES.frame$LUI.range.level <- paste(ES.frame$Low.LUI,ES.frame$High.LUI,sep="-")

ES.frame$LUI.range <- NA
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("low-low","medium-medium","high-high")] <- 0
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("low-medium","medium-high")] <- 1
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("low-high")] <- 2

ES.frame$Study.Case <- paste(ES.frame$Study.ID,ES.frame$Case.ID,sep="-")
ES.frame.noLU$Study.Case <- paste(ES.frame.noLU$Study.ID,ES.frame.noLU$Case.ID,sep="-")

############################################################################
### 04.3. Calculate response ratio effect sizes
### 
############################################################################

### Effect size calculation
### "ROM" ... ratio of means
### "SMD" ... standardized mean difference with bias correction (Hedges, 1981)

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
  escalc("SMD",data= ES.frame, append =F,
         m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
         sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
         n2i = Richness.N.Low, n1i = Richness.N.High)

ES.frame[,c("Yield.SMD","Yield.SMD.Var")] = 
  escalc("SMD",data= ES.frame, append = F,
         m2i = Yield.Mean.Low, m1i = Yield.Mean.High,
         sd2i = Yield.SD.Low, sd1i = Yield.SD.High,
         n2i = Yield.N.Low, n1i = Yield.N.High)

############################################################

ES.frame.noLU[,c("Richness.Log.RR","Richness.Log.RR.Var")] = 
  escalc("ROM", data= ES.frame.noLU, append =F,
         m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
         sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
         n2i = Richness.N.Low, n1i = Richness.N.High)

ES.frame.noLU[,c("Richness.SMD","Richness.SMD.Var")] =
  escalc("SMD", data= ES.frame.noLU, append =F,
         m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
         sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
         n2i = Richness.N.Low, n1i = Richness.N.High)

############################################################
### remove objects from workspace
rm(temp.noLU,temp.lowLU,temp.mediumLU,temp.highLU,
   temp.low.base,temp.medium.base,temp.high.base,
   temp.low.increase,temp.medium.increase,temp.high.increase,
   temp.low,temp.medium,temp.high)

###########################################################################
### Resterampe

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