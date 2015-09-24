############################################################################
### Purpose of this skript module 01 is to:
###
### 01.1. table.sort function
### 01.2. Compile ES frame
### 01.3. Calculate response ratio effect sizes
###
### General comments:
### * TO DO: build all correct pairs! e.g. for 2318-Guretzky2005 only the pair medium-low/medium-high is built but not the one between low and medium!
### * currently also not built: 2x 2997-Kern2006, 6528-Sasaki2011 (pooled medium vs pooled high), 6684-Vintu2011 (low medium)
###   7577-Mastrangelo2012 (pooled low vs medium or high), 8235-Norvez2013 (Case 1), 8568-Yan2013 (3 high-high cases)
###   516-Higgins1999, ...
### Authors: KG, MB, SK ...
############################################################################

############################################################################
### 01.1. table.sort function
### 
############################################################################

### table.sort function to restructure data

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


############################################################################
### 01.1. Compile ES frame
### 
############################################################################

### create empty ES.frame

ES.frame <- data.frame(matrix(ncol=36,nrow=0))
names(ES.frame) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Habitat.Type",        
                     "Product","ES.From.BD","Fertilization","Irrigation","Pesticides",          
                     "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
                     "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude",           
                     "Species.Group","Species.Subgroup","Trophic.Level","Richness.Mean.Low" ,  
                     "Richness.SD.Low","Richness.N.Low","Richness.Plot.Size","Richness.Mean.High","Richness.SD.High",    
                     "Richness.N.High","Yield.Mean.Low","Yield.SD.Low","Yield.N.Low","Yield.Mean.High" ,    
                     "Yield.SD.High","Yield.N.High")


# TO DO: remove "pooled within one LUI", change l 83: paste(data$study.case,data$species.group,sep="-") to data$study.case

### re-build data to ES.frame using table.sort function

for(i in unique(data$study.case)){
  
  print(i)
  
  ### remove some studies from further analysis as these cause error as they have same study.case and species group but diff. species subgroups. TO DO: Decide how to deal with that issue.
  if(i %in% c("8235-Norvez2013_1-arthropods","8002-Lohmus2013_1-woody plants",paste("9078-Hautier2014_",1:12,"-non-woody plants",sep=""),"516-Higgins1999_1-woody plants",paste("4212-Kembel2008_",1:3,"-all plants",sep=""))){
    print("ERROR. Omit from analysis.")
    next}

  data.temp = subset(data, data$study.case %in% i)
  
  # for within broad LUI comparisons
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

ES.frame$LUI.range <- NULL
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("low-low","medium-medium","high-high")] <- 0
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("low-medium","medium-high")] <- 1
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("low-high")] <- 2

ES.frame$Study.Case <- paste(ES.frame$Study.ID,ES.frame$Case.ID,sep="-")

############################################################################
### 01.3. Calculate response ratio effect sizes
### 
############################################################################

### Effect size calculation

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