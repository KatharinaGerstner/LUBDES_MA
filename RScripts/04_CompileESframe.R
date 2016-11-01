############################################################################
### Purpose of this skript module 04 is to:
###
### 04.1. Compile ES frame
### 04.2. Calculate response ratio effect sizes
###
### General comments:
###
### Authors: KG, MB, SK ...
############################################################################

############################################################################
### 04.1. Compile ES frame
### 
############################################################################

### create empty ES.frame

ES.frame <- data.frame(matrix(ncol=43,nrow=0))
names(ES.frame) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Study.Type","Habitat.Type",        
                     "Product","ES.From.BD","Fertilization","Irrigation","Pesticides",          
                     "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
                     "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude", "Country.Code",          
                     "Species.Group","Species.Subgroup","Trophic.Level","Richness.Plot.Size","Yield.Unit.Type","Richness.Mean.Low" ,  
                     "Richness.SD.Low","Richness.N.Low","Richness.Mean.High","Richness.SD.High",    
                     "Richness.N.High","Yield.Mean.Low","Yield.SD.Low","Yield.N.Low","Yield.Mean.High" ,    
                     "Yield.SD.High","Yield.N.High","Yield.SD.is.imputed.low","Yield.SD.is.imputed.high",
                     "Richness.SD.is.imputed.low","Richness.SD.is.imputed.high")


### re-build dataimp to ES.frame using table.sort function

for(i in unique(dataimp$study.case)){
  
  print(i)
  
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

## define LUI-range levels starting with a capital letter
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
ES.frame$LUI.range.level <- factor(paste(sapply(ES.frame$Low.LUI,function(x){.simpleCap(as.character(x))}),ES.frame$High.LUI,sep="-"))
ES.frame$LUI.range.level <- factor(ES.frame$LUI.range.level,levels=c("Low-low","Medium-medium","High-high","Low-medium","Medium-high","Low-high")) # reorder factor levels

ES.frame$LUI.range <- NA
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("Low-low","Medium-medium","High-high")] <- 0
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("Low-medium","Medium-high")] <- 1
ES.frame$LUI.range[ES.frame$LUI.range.level %in% c("Low-high")] <- 2

ES.frame$Species.Group<-paste(ES.frame$Species.Group)

ES.frame$Species.Group[(ES.frame$Species.Group %in% c("all plants","non-woody plants", "woody plants"))] <- "Plants"
ES.frame$Species.Group[ES.frame$Species.Group %in% c("birds","mammals","reptiles/amphibians")]<-"Vertebrates"
ES.frame$Species.Group[ES.frame$Species.Group %in% c("non-arthropod invertebrates","arthropods")]<-"Invertebrates"
ES.frame$Species.Group[(ES.frame$Species.Group=="fungi")]<-NA
ES.frame$Species.Group[(ES.frame$Species.Group=="NA")]<-NA
ES.frame$Species.Group<-factor(ES.frame$Species.Group)

ES.frame$Study.Case <- paste(ES.frame$Study.ID,ES.frame$Case.ID,sep="-")

levels(ES.frame$Product)[levels(ES.frame$Product)=="crop"]  <- "Crop"
levels(ES.frame$Product)[levels(ES.frame$Product)=="animal_feed"]  <- "Green fodder"
levels(ES.frame$Product)[levels(ES.frame$Product)=="timber"]  <- "Wood"

############################################################################
### 04.2. Calculate response ratio effect sizes
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
### remove objects from workspace
rm(i, data.temp, temp.noLU,temp.lowLU,temp.mediumLU,temp.highLU,
   temp.low.base,temp.medium.base,temp.high.base,
   temp.low.increase,temp.medium.increase,temp.high.increase,
   temp.low,temp.medium,temp.high)



###########################################################################
### Resterampe

# ES.frame.noLU <- data.frame(matrix(ncol=37,nrow=0))
# names(ES.frame.noLU) <- c("Study.ID","Case.ID","Low.LUI","High.LUI","Habitat.Type",        
#                           "Product","ES.From.BD","Fertilization","Irrigation","Pesticides",          
#                           "Grazing","Mowing","Clear.Cut","Selective.Logging","Partial.Logging",     
#                           "Additional.Treatment", "Date.Start","Date.End","Latitude","Longitude", "Country.Code",          
#                           "Species.Group","Species.Subgroup","Trophic.Level","Richness.Mean.Low" ,  
#                           "Richness.SD.Low","Richness.N.Low","Richness.Plot.Size","Richness.Mean.High","Richness.SD.High", "Richness.N.High",
#                           "Yield.SD.is.imputed.low","Yield.SD.is.imputed.high","Richness.SD.is.imputed.low","Richness.SD.is.imputed.high")
# 
# ES.frame.noLU$Study.Case <- paste(ES.frame.noLU$Study.ID,ES.frame.noLU$Case.ID,sep="-")
############################################################
# 
# ES.frame.noLU[,c("Richness.Log.RR","Richness.Log.RR.Var")] = 
#   escalc("ROM", data= ES.frame.noLU, append =F,
#          m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
#          sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
#          n2i = Richness.N.Low, n1i = Richness.N.High)
# 
# # ES.frame.noLU[,c("Richness.SMD","Richness.SMD.Var")] =
#   escalc("SMD", data= ES.frame.noLU, append =F,
#          m2i = Richness.Mean.Low, m1i = Richness.Mean.High,
#          sd2i = Richness.SD.Low, sd1i = Richness.SD.High,
#          n2i = Richness.N.Low, n1i = Richness.N.High)
# 
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
