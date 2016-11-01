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

needed_libs <- c("devtools",# needed for library googlesheets
                 "googlesheets",# for loading data directly from google
                 "mice", # for multiple imputation
                 "metafor",# for meta analysis
                 "ggplot2",# For plotting
                 "gridExtra", # for arranging multiple plots
                 "scales", # for transformation of axes labels
                 "maptools", # for converting shp into data.frame using fortify() in ggplot
#                 "rworldmap",
                 "raster",# for adding map data
                 "rasterVis", # for plotting map data with levelplot(), needs version >0.35
                 "rgdal", # for loading map data
                 "rgeos",# dependency for rgdal
                 "RColorBrewer",# fancy color schemes for plotting
                 "plyr",# for joining datasets
                 "countrycode",# convert FAO country IDs to ISO3
                 "VennDiagram",
                  "venneuler",
                 "reshape2",
#                 "rjags", # for running bayesian models
                 "knitr", # for knitting .Rmd-documents
                 "xtable",  # for saving tables as .doc
                  "ncdf4", # for loading landuse history data
                  "rasterVis" #plotting maps
)
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
sapply(needed_libs,usePackage)

rm(needed_libs, usePackage)

############################################################################
### 01.2. load all self-written functions needed for subsequent analysis
###
### 
############################################################################



############################################################################
### table.sort function
############################################################################

### table.sort function to restructure dataimp
table.sort = function(dat.low,dat.high,low,high){
  data.frame("Study.ID"=dat.low$Study.ID, "Case.ID" =dat.low$Case.ID, 
             "Low.LUI" = low, "High.LUI" = high,
             "Study.Type" = dat.low$study.type,
             "Land.use...land.cover" = dat.low$Land.use...land.cover, "Product" = dat.low$Product, "ES.and.BD" =dat.low$ES.and.BD, "LU.definition.and.ES"=dat.low$LU.definition.and.ES,             
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
             "Richness.Mean.High" = as.numeric(dat.high$richness.mean), "Richness.SD.High" = as.numeric(dat.high$richness.SD), "Richness.N.High" = as.numeric(dat.high$X..of.samples.for.BD.measure),             
             "Richness.Plot.Size" = as.numeric(dat.low$reported.area.of.BD),
             "Yield.Unit.Type" = dat.low$Yield.Unit.Type,
             "Yield.Mean.Low" = as.numeric(dat.low$yield.mean), "Yield.SD.Low" = as.numeric(dat.low$yield.SD), "Yield.N.Low" = as.numeric(dat.low$X..of.samples.for.YD.measure),
             "Yield.Mean.High" = as.numeric(dat.high$yield.mean), "Yield.SD.High" = as.numeric(dat.high$yield.SD), "Yield.N.High" = as.numeric(dat.high$X..of.samples.for.YD.measure),
             "Yield.SD.is.imputed.low" = dat.low$yield.SD.is.imputed,"Yield.SD.is.imputed.high" = dat.high$yield.SD.is.imputed,
             "Richness.SD.is.imputed.low" = dat.low$richness.SD.is.imputed,"Richness.SD.is.imputed.high" = dat.high$richness.SD.is.imputed)
}

###########################################################################
### Covariance Matrix of Log.RRs with zero on the diagonal
###########################################################################

### cov(X,Y) <- cor(X,Y)*sqrt(Var(X))*sqrt(Var(Y))
### cor(X,Y) is 0.5 if LUI.range.level within the same study-case share a control or treatment
M.matrix <- function(dat){
  M <- diag(nrow(dat))
  diag(M) <- 0
  ## calculate covariance for cases with (low-medium, medium-high), (low-low, low-medium, low-high), (medium-medium, medium- high), (high-high, medium-high)
  for(x in unique(dat$Study.Case)){
    sub.rows <- which(dat$Study.Case==x)
    for (i in sub.rows){
      for(j in sub.rows){
        if(paste(dat$LUI.range.level[i],dat$LUI.range.level[j],sep="_") %in% c("low-medium_medium-high","low-medium_medium-medium", "low-medium_medium-high","low-low_low-medium","low-low_low-high","medium-medium_medium-high","high-high_medium-high")){
          M[i,j] <- M[j,i] <- 0.5 * sqrt(dat$Log.RR.Var[i] * dat$Log.RR.Var[j])
        }
      }  
    }
  }
  return(M)
}  

###########################################################################
### Model selection based on likelihood-ratio test
###########################################################################
RMASelect <- function(model){
  
  allTerms <- trim(strsplit(paste(model$call$mods)[2],'[+]')[[1]])
  
  currentModel <- model
  currentTerms <- allTerms
  stats.list <- list()
  i <- 1
  
  while(length(currentTerms)>1){
    LRTs <- numeric(length(currentTerms))
    dfs <- character(length(currentTerms))
    Ps <- numeric(length(currentTerms))
    var2drop <- character(length(currentTerms))
    
    t<-1
    for (term in currentTerms){
      
#      print(term)
      if(length(unlist(grep(term,currentTerms)))>1) next # to avoid excluding main effects when interactions are still in
      newModel <- try(update(currentModel,paste("~.-",term,sep="")),silent=T)
      if(inherits(newModel, "try-error")){
        newModel <- update(currentModel,paste("~.-",term,sep=""),control=list(optimizer="optim", optmethod="BFGS"))     
      }
        
      an <- anova(currentModel,newModel)
      #       print(term)
      #       print(an)
      
      var2drop[t] <- term
      LRTs[t]<-an$LRT
      dfs[t]<-paste(an$p.r,an$p.f)
      Ps[t]<-an$pval
      
      t<-t+1
    }
    
    if (all(Ps<0.05)) break
    
    stats <- data.frame(var2drop,LRTs,dfs,Ps)
    if(length(which(stats$var2drop==""))>0) stats <- stats[-which(stats$var2drop==""),]
    print(stats)
    
    dropTerm <- stats$var2drop[which(stats$LRTs==min(stats$LRTs))]
    if (length(dropTerm)==0) break
    print(dropTerm)
    
    currentModel <- try(update(currentModel,paste("~.-",term,sep="")),silent=T)
    if(inherits(currentModel, "try-error")){
      currentModel <- update(currentModel,paste("~.-",term,sep=""),control=list(optimizer="optim", optmethod="BFGS"))     
    }
    currentTerms <- currentTerms[-which(currentTerms==dropTerm)]
    
    stats.list[[i]] <- stats
    i <- i+1
    
  }
  
  if(length(currentTerms)==1){
    newModel<-update(currentModel,"~ 1")
    an <- anova(currentModel,newModel)
    if(an$LRT > 0){
      currentModel <- newModel
      print(dropTerm)
    }  
  }
  
  #  return(list(model=currentModel,stats.list=stats.list))
  return(currentModel)
  
}

###########################################################################
### user defined theme for ggplot
###########################################################################
theme_lubdes <- function (base_size = 12, base_family = "", rel.text.size=1.5, legend.position = "right") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(rel.text.size)), 
      axis.ticks = element_line(colour = "black"), 
      axis.title = element_text(size = rel(rel.text.size)), 
      axis.text = element_text(size = rel(rel.text.size)),
      legend.text=element_text(size = rel(rel.text.size)),
      legend.title=element_text(size = rel(rel.text.size)),
      legend.key = element_rect(colour = "grey80"), 
      legend.position = legend.position,
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "grey50"), 
      panel.grid.major = element_line(colour = "grey90", size = 0.2), 
      panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
      strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
      strip.text = element_text(size=rel(rel.text.size)))
}

############################################################################
### standardize area units
############################################################################

convertAreaUnits <- function(data, type=c("bd", "yield")){
  if(type == "yield"){
    if(!("reported.area.of.ES" %in% names(data))){stop("There must be a column called 'reported.area.of.ES'")}
    if(!("reported.area.unit.of.ES" %in% names(data))){stop("There must be a column called 'reported.area.unit.of.ES'")}
    new_units <- data$sampled.size.unit.1
    new_area <- as.numeric(data$sampled.size.area)
  }
  
  if(type == "bd"){
    if(!("reported.area.of.BD" %in% names(data))){stop("There must be a column called 'reported.area.of.BD'")}
    if(!("reported.area.unit.of.BD" %in% names(data))){stop("There must be a column called 'reported.area.unit.of.BD'")}
    new_units <- data$reported.area.unit.of.BD
    new_area <- as.numeric(data$reported.area.of.BD)
  }
  new_units <- ifelse(!is.na(new_units) & new_units %in% c("mÂ²","m2","m²"), "m²", new_units)  
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "ha", new_area * 10000, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "ha", "m²", new_units)  
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "cm²", new_area * 0.0001, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "cm²", "m²", new_units)	
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "mm²", new_area * 1e-6, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "mm²", "m²", new_units)	
  
  new_area  <- ifelse(!is.na(new_units) & new_units == "km²", new_area * 1000000, new_area)
  new_units <- ifelse(!is.na(new_units) & new_units == "km²", "m²", new_units)	
#  
  if(type == "yield"){
    data$sampled.size.unit.1 <- new_units
    data$sampled.size.area <- as.numeric(new_area)
  }
  if(type == "bd"){
    data$reported.area.unit.of.BD <- new_units
    data$reported.area.of.BD <- as.numeric(new_area)
  }
  return(data)
}

### Sort transects and traps
# SortTransectsTraps <- function(data){
#   if(!("reported.area.of.BD" %in% names(data))){stop("There must be a column called 'reported.area.of.BD'")}
#   
#   new_units <- data$sampled.size.unit
#   new_area <- as.numeric(data$sampled.area)
#   
#   transects <- c("points/transect", "transect", "transect (km)","transect (m)")
#   new_units <- ifelse(new_units %in% transects, "transects", new_units)	
#   new_area <- ifelse(new_units %in% transects, NA, new_area)
#   
#   traps <- c("traps", "traps (mistnets)")
#   new_units <- ifelse(new_units %in% traps, "traps", new_units)	
#   new_area <- ifelse(new_units %in% traps, NA, new_area)
#   
#   data$sampled.size.unit <- new_units
#   data$sampled.area <- new_area
#   
#   return(data)
# }

# ############################################################################
# ### standardize yield units (by HP, KG)
# ############################################################################
# 
# convertYieldUnits <- function(data){
#   if(!("yield.unit" %in% names(data))){stop("There must be a column called 'yield.unit'")}
#   if(!("yield.mean" %in% names(data))){stop("There must be a column called 'yield.mean'")}
#   
#   new_units <- data$yield.unit  
#   new_means <- data$yield.mean
#   new_sd <- data$yield.SD
#   
#   ## getting rid of inconsistencies with names
#   new_units <- ifelse(!is.na(new_units) & new_units == "cm grass height", "cm", new_units)
#   new_units <- ifelse(!is.na(new_units) & new_units == "Mg/ha", "t/ha", new_units)
#   new_units <- ifelse(!is.na(new_units) & new_units == "m³/ha/year", "m³/ha", new_units)
#   
#   
#   new_means  <- ifelse(!is.na(new_units) & new_units == "cm", new_means/100, new_means)
#   new_SD <- ifelse(!is.na(new_units) & new_units == "cm", new_SD/100, new_SD)
#   new_units <- ifelse(!is.na(new_units) & new_units == "cm", "m", new_units)	
#   
#   new_means  <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", new_means*10000, new_means)
#   new_SD <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", new_SD*10000, new_SD)
#   new_units <- ifelse(!is.na(new_units) & new_units == "kg/hm²a", "kg/m²", new_units)
#   
#   new_means  <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", new_means*100, new_means)
#   new_SD  <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", new_SD*100, new_SD)
#   new_units <- ifelse(!is.na(new_units) & new_units == "m³/0.01 ha", "m³/ha", new_units)
#   
#   new_means  <- ifelse(!is.na(new_units) & new_units == "g/m²", new_means/1000, new_means)
#   new_SD  <- ifelse(!is.na(new_units) & new_units == "g/m²", new_SD/1000, new_SD)
#   new_units <- ifelse(!is.na(new_units) & new_units == "g/m²", "kg/m²", new_units)
#   new_means  <- ifelse(!is.na(new_units) & new_units == "kg/m²", new_means*10, new_means)
#   new_SD  <- ifelse(!is.na(new_units) & new_units == "kg/m²", new_SD*10, new_SD)
#   new_units <- ifelse(!is.na(new_units) & new_units == "kg/m²", "t/ha", new_units)
#   
#   data$yield.unit <- new_units
#   data$yield.mean <- new_means
#   
#   data$Yield.Unit.Type <- NA
#   counts <- c("% conifer", "% fruit set", "% of trees of original volume removed","fruit/sq.m.", "treecover (%)", "trees/ha", "trees/year")
#   mass <- c("g", "kg / animal / day", "kg/tree", "t/ha")
#   volume <- c("m³/ha")
#   area <- c("m", "m²/ha" )
#   data$Yield.Unit.Type[data$yield.unit %in% counts] <- "Count/area"
#   data$Yield.Unit.Type[data$yield.unit %in% mass] <- "Mass/area"
#   data$Yield.Unit.Type[data$yield.unit %in% volume] <- "Volume/area"
#   data$Yield.Unit.Type[data$yield.unit %in% area] <- "Area/area"
#   
#   return(data)
# }
# 
# ############################################################################
# ### RMA select function (by HP and TN)
# ############################################################################
# RMASelect <- function(model){
#   
#   allTerms <- trim(strsplit(paste(model$call$mods)[2],'[+]')[[1]])
#   
#   stats <- data.frame(terms=allTerms,df=NA,LR=NA,P=NA)
#   
#   currentModel <- model
#   currentTerms <- allTerms
#   
#   while(TRUE){
#     LRTs <- numeric(length(currentTerms))
#     dfs <- character(length(currentTerms))
#     Ps <- numeric(length(currentTerms))
#     
#     t<-1
#     for (term in currentTerms){
#       
#       #newModel<-update(currentModel,paste("~.-",term,sep=""))
#       newModel <- update(currentModel,paste("~.-",term,sep=""))
#       
#       an <- anova(currentModel,newModel)
#       
#       LRTs[t]<-an$LRT
#       dfs[t]<-paste(an$p.r,an$p.f)
#       Ps[t]<-an$pval
#       
#       t<-t+1
#     }
#     
#     if (all(Ps<0.05)) break
#     
#     dropTermPos <- which(LRTs==min(LRTs))
#     dropTerm <- currentTerms[dropTermPos]
#     
#     stats$LR[which(stats$terms==dropTerm)]<-LRTs[which(currentTerms==dropTerm)]
#     stats$df[which(stats$terms==dropTerm)]<-dfs[which(currentTerms==dropTerm)]
#     stats$P[which(stats$terms==dropTerm)]<-Ps[which(currentTerms==dropTerm)]
#     
#     #currentModel <- update(currentModel,paste("~.-",dropTerm,sep=""))
#     currentModel <- update(currentModel,paste("~.-",dropTerm,sep=""))
#     currentTerms <- currentTerms[-dropTermPos]
#     
#   }
#   
#   stats$LR[is.na(stats$LR)] <- LRTs[match(stats$terms[is.na(stats$LR)],currentTerms)]
#   stats$df[is.na(stats$df)] <- dfs[match(stats$terms[is.na(stats$df)],currentTerms)]
#   stats$P[is.na(stats$P)] <- Ps[match(stats$terms[is.na(stats$P)],currentTerms)]
#   
#   return(list(model=currentModel,stats=stats))
#   
# }
# 
