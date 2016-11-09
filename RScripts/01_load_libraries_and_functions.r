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
                 "raster",# for adding map data
                 "rgdal", # for loading map data
                 "rgeos",# dependency for rgdal
                 "plyr",# for joining datasets
                 "VennDiagram",
                 "venneuler",
                 "reshape2",
                 "knitr", # for knitting .Rmd-documents
                 "xtable",  # for saving tables as .doc
                 "ncdf4" # for loading landuse history data
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
convert.log2equidist <- function(logRR.mean,logRR.ci.lb,logRR.ci.ub){
  perc.change <- 100*(exp(logRR.mean)-1)
  perc.change.ci.lb <- 100*(exp(logRR.ci.lb)-1)
  perc.change.ci.ub <- 100*(exp(logRR.ci.ub)-1)
  CI95.perc.change <- "[" %+% round(perc.change.ci.lb,digits=2) %+% "," %+%  round(perc.change.ci.ub,digits=2) %+% "]"

  return(data.frame(perc.change,perc.change.ci.lb,perc.change.ci.ub,CI95.perc.change))
  }


############################################################################
rename.factor.levels <- function(dat){
  if(!is.null(levels(dat$LUI.range.level))){
    levels(dat$LUI.range.level)[levels(dat$LUI.range.level)=="low-low"]  <- "Low-low"
    levels(dat$LUI.range.level)[levels(dat$LUI.range.level)=="low-medium"]  <- "Low-medium"
    levels(dat$LUI.range.level)[levels(dat$LUI.range.level)=="low-high"]  <- "Low-high"
    levels(dat$LUI.range.level)[levels(dat$LUI.range.level)=="medium-medium"]  <- "Medium-medium"
    levels(dat$LUI.range.level)[levels(dat$LUI.range.level)=="medium-high"]  <- "Medium-high"
    levels(dat$LUI.range.level)[levels(dat$LUI.range.level)=="high-high"]  <- "High-high"
    dat$LUI.range.level <- factor(dat$LUI.range.level)[drop=T] # drop unused levels
  }

  if(!is.null(levels(dat$Species.Group))){
    levels(dat$Species.Group)[levels(dat$Species.Group)=="plants"] <- "Plants"
    levels(dat$Species.Group)[levels(dat$Species.Group)=="vertebrates"]<-"Vertebrates"
    levels(dat$Species.Group)[levels(dat$Species.Group)=="invertebrates"]<-"Invertebrates"
    dat$Species.Group <- factor(dat$Species.Group)[drop=T] # drop unused levels
  }
  
  if(!is.null(levels(dat$Product))){
    levels(dat$Product)[levels(dat$Product)=="crop"]  <- "Crop"
    levels(dat$Product)[levels(dat$Product)=="green fodder"]  <- "Green fodder"
    levels(dat$Product)[levels(dat$Product)=="wood"]  <- "Wood"
    dat$Product <- factor(dat$Product)[drop=T] # drop unused levels
  }

  return(dat) 
}


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

