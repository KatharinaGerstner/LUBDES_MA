############################################################################
### Purpose of this skript module 07 is to:
### 
### 06.1. Remove cases with zero variances, pseudo-replicates, redundant LUI.range.level comparisons 
### 06.2. remove columns not needed for the analysis, unify names
### 06.3. save rawdata as table in a word doc
###
### Authors: KG
############################################################################

###########################################################################
### 06.1. Remove cases with zero variances, pseudo-replicates, redundant LUI.range.level comparisons 
##########################################################################

### restrict analysis to study cases with positive variances
ES.frame <- subset(ES.frame, Richness.Log.RR.Var>0 & Yield.Log.RR.Var>0)
ES.frame.richness <- ES.frame.yield <- ES.frame

### Remove pseudo-replicates
ES.frame.richness <- ES.frame.richness[!duplicated(ES.frame.richness[,c("Study.ID","Case.ID","LUI.range.level","Species.Group")]),]
ES.frame.yield <- ES.frame.yield[!duplicated(ES.frame.yield[,c("Study.ID","LUI.range.level","Product")]),]

### remove redundant cases, cases low-high are removed if all three comparisons are available
for(x in unique(ES.frame$Study.Case)){
  subset.richness <- subset(ES.frame.richness,Study.Case==x)
  subset.yield <- subset(ES.frame.yield,Study.Case==x)
  ## remove redundant cases
  if (all(c("Low-medium","Low-high","Medium-high") %in% unique(subset.richness$LUI.range.level))){
    ES.frame.richness <- ES.frame.richness[!(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="Low-high"),]
  }
  if (all(c("Low-medium","Low-high","Medium-high") %in% unique(subset.yield$LUI.range.level))){
    ES.frame.yield <- ES.frame.yield[!(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="Low-high"),]
  }
}  

###########################################################################
### 06.2. remove columns not needed for the analysis, unify names
##########################################################################

ES.frame.richness <- ES.frame.richness[,c("Richness.Log.RR","Richness.Log.RR.Var","Longitude", "Latitude","LUI.range.level","Low.LUI","High.LUI","Study.ID","Case.ID","Study.Case","Species.Group","Product","main_climate","landuse_history","ES.and.BD","Richness.Plot.Size")] #,"npp","time.since.first.use")]
#ES.frame.richness <- ES.frame.richness[,c("Richness.Log.RR","Richness.Log.RR.Var","Longitude", "Latitude","LUI.range.level","Low.LUI","High.LUI","Study.ID","Case.ID","Study.Case","Species.Group","Species.Subgroup","Product","main_climate","landuse_history","ES.and.BD","Richness.Plot.Size")] #,"npp","time.since.first.use")] # adding Species.Subgroup
ES.frame.yield <- ES.frame.yield[,c("Yield.Log.RR","Yield.Log.RR.Var","Longitude", "Latitude","LUI.range.level","Low.LUI","High.LUI","Study.ID","Study.Case","Case.ID","Species.Group","Product","main_climate","landuse_history", "LU.definition.and.ES", "Yield.Unit.Type")] #,"npp","time.since.first.use")]

## remove cases with NA in covariates
ES.frame.richness <- ES.frame.richness[complete.cases(ES.frame.richness[,c("Richness.Log.RR","Richness.Log.RR.Var","Longitude", "Latitude","LUI.range.level","Low.LUI","High.LUI","Study.ID","Case.ID","Study.Case","Species.Group","Product","main_climate","landuse_history")]),] 
ES.frame.yield <- ES.frame.yield[complete.cases(ES.frame.yield[,c("Yield.Log.RR","Yield.Log.RR.Var","Longitude", "Latitude","LUI.range.level","Low.LUI","High.LUI","Study.ID","Study.Case","Case.ID","Species.Group","Product","main_climate","landuse_history")]),] 

names(ES.frame.richness)[1:2] <- c("Log.RR","Log.RR.Var")
names(ES.frame.yield)[1:2] <- c("Log.RR","Log.RR.Var")

ES.frame.richness$Study.ID <- factor(ES.frame.richness$Study.ID)[drop=T] # drop unused study levels
ES.frame.richness$Study.Case <- factor(ES.frame.richness$Study.Case)[drop=T] # drop unused study levels

ES.frame.yield$Study.ID <- factor(ES.frame.yield$Study.ID)[drop=T] # drop unused study levels
ES.frame.yield$Study.Case <- factor(ES.frame.yield$Study.Case)[drop=T] # drop unused study levels

write.table(ES.frame.richness[,c("Study.Case","Longitude", "Latitude","LUI.range.level","Species.Group","Product","main_climate","landuse_history","ES.and.BD","Richness.Plot.Size","Log.RR","Log.RR.Var")], file=path2temp %+% "ES.frame.richness.xls", row.names=F,sep="\t")

write.table(ES.frame.yield[,c("Study.Case","Longitude", "Latitude","LUI.range.level","Product","main_climate","landuse_history", "LU.definition.and.ES","Yield.Unit.Type","Log.RR","Log.RR.Var")], file=path2temp %+% "ES.frame.yield.xls", row.names=F,sep="\t")

###########################################################################
### 06.3. save rawdata as table in a word doc
###########################################################################
print(xtable(ES.frame.richness[,c("Study.Case","Longitude", "Latitude","LUI.range.level","Species.Group","Product","main_climate","landuse_history","ES.and.BD","Richness.Plot.Size","Log.RR","Log.RR.Var")]), type = "html", file=path2temp %+% "ES.frame.richness.doc", rownames=F) # save the HTML table as a .doc file
print(xtable(ES.frame.yield[,c("Study.Case","Longitude", "Latitude","LUI.range.level","Product","main_climate","landuse_history", "LU.definition.and.ES","Yield.Unit.Type","Log.RR","Log.RR.Var")]), type = "html", file=path2temp %+% "ES.frame.yield.doc",, rownames=F) # save the HTML table as a .doc file


