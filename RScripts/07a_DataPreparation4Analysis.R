###########################################################################
### Data Preparation
##########################################################################

ES.frame <- subset(ES.frame, Richness.Log.RR.Var>0 & Yield.Log.RR.Var>0) # restrict analysis to study cases with positive variances
### Remove pseudo-replicates
ES.frame.richness <- ES.frame[!duplicated(ES.frame[,c("Study.ID","Case.ID","LUI.range.level","Species.Group")]),]
ES.frame.yield <- ES.frame[!duplicated(ES.frame[,c("Study.ID","LUI.range.level","Product")]),]

### remove redundant cases, cases low-high are removed if all three comparisons are available
for(x in unique(ES.frame$Study.Case)){
  subset.richness <- subset(ES.frame.richness,Study.Case==x)
  subset.yield <- subset(ES.frame.yield,Study.Case==x)
  ## remove redundant cases
  if (all(c("low-medium","low-high","medium-high") %in% unique(subset.richness$LUI.range.level))){
    ES.frame.richness <- ES.frame.richness[!(ES.frame.richness$Study.Case==x & ES.frame.richness$LUI.range.level=="low-high"),]
  }
  if (all(c("low-medium","low-high","medium-high") %in% unique(subset.yield$LUI.range.level))){
    ES.frame.yield <- ES.frame.yield[!(ES.frame.yield$Study.Case==x & ES.frame.yield$LUI.range.level=="low-high"),]
  }
}  

### remove columns not needed for the analysis, unify names
ES.frame.richness <- ES.frame.richness[,c("Richness.Log.RR","Richness.Log.RR.Var","LUI.range.level","Low.LUI","High.LUI","Study.ID","Case.ID","Study.Case","Species.Group","Product","BIOME","npp","time.since.first.use")]
names(ES.frame.richness)[1:2] <- c("Log.RR","Log.RR.Var")
ES.frame.yield <- ES.frame.yield[,c("Yield.Log.RR","Yield.Log.RR.Var","LUI.range.level","Low.LUI","High.LUI","Study.ID","Study.Case","Case.ID","Species.Group","Product","BIOME","npp","time.since.first.use")]
names(ES.frame.yield)[1:2] <- c("Log.RR","Log.RR.Var")

ES.frame.richness$LUI.range.level <- factor(ES.frame.richness$LUI.range.level,levels=c("low-low","low-medium","low-high","medium-medium","medium-high","high-high")) # reorder factor levels
ES.frame.richness$Study.ID <- factor(ES.frame.richness$Study.ID)[drop=T] # drop unused study levels
ES.frame.richness$Study.Case <- factor(ES.frame.richness$Study.Case)[drop=T] # drop unused study levels

### Scale continuous covariates to reduce the influence of extremes
ES.frame.richness$npp <- scale(ES.frame.richness$npp)
ES.frame.richness$time.since.first.use <- scale(ES.frame.richness$time.since.first.use)
ES.frame.yield$npp <- scale(ES.frame.yield$npp)
ES.frame.yield$time.since.first.use <- scale(ES.frame.yield$time.since.first.use)

### save rawdata as table in a word doc
print(xtable(ES.frame.richness), type = "html", file=path2temp %+% "ES.frame.richness.doc") # save the HTML table as a .doc file
print(xtable(ES.frame.yield), type = "html", file=path2temp %+% "ES.frame.yield.doc") # save the HTML table as a .doc file

###########################################################################
### some functions for the data analysis
###########################################################################

### Covariance Matrix with 0's on the diagonal
M.matrix <- function(dat){
  M <- diag(nrow(dat))
  ## calculate covariance for cases with (low-medium, medium-high), (low-low, low-medium, low-high), (medium-medium, medium- high), (high-high, medium-high)
  for(x in unique(dat$Study.Case)){
    sub.rows <- which(dat$Study.Case==x)
    for (i in sub.rows){
      for(j in sub.rows){
        if(paste(dat$LUI.range.level[i],dat$LUI.range.level[j],sep="_") %in% c("low-medium_medium-high","low-medium_medium-medium", "low-medium_medium-high","low-low_low-medium","low-low_low-high","medium-medium_medium-high","high-high_medium-high")){
          M[i,j] <- 0.5
          M[j,i] <- M[i,j] # to fill the other triangle of the matrix
        }
      }  
    }
  }
  return(M)
}  

