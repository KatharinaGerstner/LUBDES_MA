############################################################################
### Purpose of this skript module 01 is to:
### 
### 01.1. load all libraries needed for subsequent analysis
### 01.2. load all self-written functions needed for subsequent analysis
###
### Authors: KG, MB ...
############################################################################

############################################################################
### 01.1. load all libraries needed for subsequent analysis
###
###
############################################################################

### first timers should run:
# install.packages("devtools","mice","mi","metafor","ggplot2","scales")
# library(devtools)
# devtools::install_github("jennybc/googlesheets") # documentation at:http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html and https://github.com/jennybc/googlesheets

library(devtools) # needed for library googlesheets
library(googlesheets) # for loading data directly from google
library(mice) # for imputation
#library(mi) # for imputation
library(metafor) # for meta analysis
library(ggplot2) # For plotting
library(scales) # ?
library(maps) # still needed? #For map data
library(rworldmap) # still needed?
#library(geosphere)
library(raster) # for adding map data
#library(shapefiles)
#library(sp)
require(rgdal) # for loading map data
library(rgeos) # dependency for rgdal
library(RColorBrewer) # fancy color schemes for plotting
library(plyr) # for joining datasets
library(countrycode) # convert FAO country IDs to ISO3
library(VennDiagram)
# library(venneuler)
library(reshape2)


############################################################################
### 01.2. load all self-written functions needed for subsequent analysis
###
### 
############################################################################

############################################################################
### helper function to combine strings
############################################################################
"%+%" <- function(x,y)paste(x,y,sep="")

############################################################################
### RMA select function (by HP and TN)
############################################################################
RMASelect <- function(model){
  
  allTerms <- trim(strsplit(paste(model$call$mods)[2],'[+]')[[1]])
  
  stats <- data.frame(terms=allTerms,df=NA,LR=NA,P=NA)
  
  currentModel <- model
  currentTerms <- allTerms
  
  while(TRUE){
    LRTs <- numeric(length(currentTerms))
    dfs <- character(length(currentTerms))
    Ps <- numeric(length(currentTerms))
    
    t<-1
    for (term in currentTerms){
      
      #newModel<-update(currentModel,paste("~.-",term,sep=""))
      newModel <- update(currentModel,paste("~.-",term,sep=""))
      
      an <- anova(currentModel,newModel)
      
      LRTs[t]<-an$LRT
      dfs[t]<-paste(an$p.r,an$p.f)
      Ps[t]<-an$pval
      
      t<-t+1
    }
    
    if (all(Ps<0.05)) break
    
    dropTermPos <- which(LRTs==min(LRTs))
    dropTerm <- currentTerms[dropTermPos]
    
    stats$LR[which(stats$terms==dropTerm)]<-LRTs[which(currentTerms==dropTerm)]
    stats$df[which(stats$terms==dropTerm)]<-dfs[which(currentTerms==dropTerm)]
    stats$P[which(stats$terms==dropTerm)]<-Ps[which(currentTerms==dropTerm)]
    
    #currentModel <- update(currentModel,paste("~.-",dropTerm,sep=""))
    currentModel <- update(currentModel,paste("~.-",dropTerm,sep=""))
    currentTerms <- currentTerms[-dropTermPos]
    
  }
  
  stats$LR[is.na(stats$LR)] <- LRTs[match(stats$terms[is.na(stats$LR)],currentTerms)]
  stats$df[is.na(stats$df)] <- dfs[match(stats$terms[is.na(stats$df)],currentTerms)]
  stats$P[is.na(stats$P)] <- Ps[match(stats$terms[is.na(stats$P)],currentTerms)]
  
  return(list(model=currentModel,stats=stats))
  
}


############################################################################
### standardize yield units (by HP, KG)
############################################################################

convertYieldUnits <- function(data){
  if(!("yield.unit" %in% names(data))){stop("There must be a column called 'yield.unit'")}
  if(!("yield.mean" %in% names(data))){stop("There must be a column called 'yield.mean'")}
  
  new_units <- data$yield.unit	
  new_means <- data$yield.mean
  new_sd <- data$yield.SD
  
  ## getting rid of inconsistencies with names
  new_units <- ifelse(!is.na(new_units) & new_units == "cm grass height", "cm", new_units)
  new_units <- ifelse(!is.na(new_units) & new_units == "Mg/ha", "t/ha", new_units)
  new_units <- ifelse(!is.na(new_units) & new_units == "m³/ha/year", "m³/ha", new_units)
  
  
  new_means  <- ifelse(!is.na(new_units) & new_units == "cm", new_means/100, new_means)
  new_SD <- ifelse(!is.na(new_units) & new_units == "cm", new_SD/100, new_SD)
  new_units <- ifelse(!is.na(new_units) & new_units == "cm", "m", new_units)	
  
  new_means  <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", new_means*10000, new_means)
  new_SD <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", new_SD*10000, new_SD)
  new_units <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", "kg/m²", new_units)
  
  new_means  <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", new_means*100, new_means)
  new_SD  <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", new_SD*100, new_SD)
  new_units <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", "m³/ha", new_units)
  
  new_means  <- ifelse(!is.na(new_units) & new_units == "g/m²", new_means/1000, new_means)
  new_SD  <- ifelse(!is.na(new_units) & new_units == "g/m²", new_SD/1000, new_SD)
  new_units <- ifelse(!is.na(new_units) & new_units == "g/m²", "kg/m²", new_units)
  new_means  <- ifelse(!is.na(new_units) & new_units == "kg/m²", new_means*10, new_means)
  new_SD  <- ifelse(!is.na(new_units) & new_units == "kg/m²", new_SD*10, new_SD)
  new_units <- ifelse(!is.na(new_units) & new_units == "kg/m²", "t/ha", new_units)
  
  data$yield.unit <- new_units
  data$yield.mean <- new_means
  
  data$Yield.Unit.Type <- NA
  counts <- c("% conifer", "% fruit set", "% of trees of original volume removed","fruit/sq.m.", "treecover (%)", "trees/ha", "trees/year")
  mass <- c("g", "kg / animal / day", "kg/tree", "t/ha")
  volume <- c("m³/ha")
  area <- c("m", "m²/ha" )
  data$Yield.Unit.Type[data$yield.unit %in% counts] <- "Count/area"
  data$Yield.Unit.Type[data$yield.unit %in% mass] <- "Mass/area"
  data$Yield.Unit.Type[data$yield.unit %in% volume] <- "Volume/area"
  data$Yield.Unit.Type[data$yield.unit %in% area] <- "Area/area"
  
  return(data)
}

############################################################################
### standardize area units
############################################################################

convertAreaUnits <- function(data, type=c("bd", "yield")){
  if(type == "yield"){
    if(!("sampled.size.area" %in% names(data))){stop("There must be a column called 'sampled.size.area'")}
    if(!("sampled.size.unit.1" %in% names(data))){stop("There must be a column called 'sampled.size.unit.1'")}
    new_units <- data$sampled.size.unit.1
    new_area <- as.numeric(data$sampled.size.area)
  }
  
  if(type == "bd"){
    if(!("sampled.area" %in% names(data))){stop("There must be a column called 'sampled.area'")}
    if(!("sampled.size.unit" %in% names(data))){stop("There must be a column called 'sampled.size.unit'")}
    new_units <- data$sampled.size.unit
    new_area <- as.numeric(data$sampled.area)
  }
  new_area  <- ifelse(!is.na(new_units) & new_units == "mÂ²", "m²", new_area)
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "ha", new_area * 10000, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "ha", "m²", new_units)	
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "cm²", new_area * 0.0001, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "cm²", "m²", new_units)	
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "mm²", new_area * 1e-6, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "mm²", "m²", new_units)	
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "km²", new_area * 1000000, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "km²", "m²", new_units)	
  
  if(type == "yield"){
    data$sampled.size.unit.1 <- new_units
    data$sampled.size.area <- as.numeric(new_area)
  }
  if(type == "bd"){
    data$sampled.size.unit <- new_units
    data$sampled.area <- as.numeric(new_area)
  }
  return(data)
}

### Sort transects and traps
SortTransectsTraps <- function(data){
  if(!("sampled.size.unit" %in% names(data))){stop("There must be a column called 'sampled.size.unit'")}
  
  new_units <- data$sampled.size.unit
  new_area <- as.numeric(data$sampled.area)
  
  transects <- c("points/transect", "transect", "transect (km)","transect (m)")
  new_units <- ifelse(new_units %in% transects, "transects", new_units)	
  new_area <- ifelse(new_units %in% transects, NA, new_area)
  
  traps <- c("traps", "traps (mistnets)")
  new_units <- ifelse(new_units %in% traps, "traps", new_units)	
  new_area <- ifelse(new_units %in% traps, NA, new_area)
  
  data$sampled.size.unit <- new_units
  data$sampled.area <- new_area
  
  return(data)
}

############################################################################
### table.sort function
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
             "Yield.Mean.High" = as.numeric(dat.high$yield.mean), "Yield.SD.High" = as.numeric(dat.high$yield.SD), "Yield.N.High" = as.numeric(dat.high$X..of.samples.for.YD.measure),
             "Yield.SD.is.imputed.low" = dat.low$Yield.SD.is.imputed,"Yield.SD.is.imputed.high" = dat.high$Yield.SD.is.imputed,
             "Richness.SD.is.imputed.low" = dat.low$Richness.SD.is.imputed,"Richness.SD.is.imputed.high" = dat.high$Richness.SD.is.imputed)
}
