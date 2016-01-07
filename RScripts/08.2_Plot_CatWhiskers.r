############################################################################
### Purpose of this skript module 08.2 is to:
###
### 08.2.1. Plot CatWhisker plots
###
### General comments:
### * added by Ralf, no clue how to use the models data 
### * originates from helens RR-Slope_Plots.R skript, which is now deprictates
### * suggestions for modifications:
###   - separate by producs
###   - use model data
###   - use ggplot
###   - dissolve function into easier manipulated code
###
### Authors: HP, RS ...
############################################################################

############################################################################
### 08.2.1 Plot CatWhisker plots
### 
### 
############################################################################

# load HP's CatWhisker code

CatsWhiskers_plot <- function(data, YieldorRichness = c("yield", "richness"), one=1, two=5, three=9, alpha=100, covariate ="Product", dataType = c("model", "raw"), model){
  
  if(dataType == "raw"){		
    if(YieldorRichness == "yield"){index <- which(names(data) == "Yield.Log.RR")}
    if(YieldorRichness == "richness"){index <- which(names(data) == "Richness.Log.RR")}
    newdat <- data.frame(y=rep(1, nrow(data)), logRR = data[,index], low = data$Low.LUI , high = data$High.LUI, range = data$LUI.range.level, covariate=data[,names(data) == covariate])}
  
  if(dataType == "model"){
    if(missing(model)){stop("expecting a model as well")}
    newdat <- expand.grid(LUI.range.level=levels(data$LUI.range.level), BIOME=levels(data$BIOME), Species.Group =levels(data$Species.Group), Product=levels(data$Product), logRR=NA, y=1)
    
    mm <- model.matrix(model$call$mods, data=newdat)
    mm <- mm[,-which(colnames(mm)=="(Intercept)")]
    preds <- predict.rma(model, newmods = mm)
    newdat$logRR <- preds$pred
    names(newdat)[names(newdat) == "LUI.range.level"] <- "range"
    names(newdat)[names(newdat) == "Product"] <- "covariate"
    
  }
  
  green <-rgb(77,175,74, alpha=alpha, max=255)
  red <- rgb(228,26,28, alpha=alpha, max=255)
  blue <- rgb(55,126,184, alpha=alpha, max=255)
  cols <- data.frame(covariate = levels(newdat$covariate), colour = c(green, red, blue))
  
  newdat$Colour <- cols$colour[match(newdat$covariate, cols$covariate)]
  
  newdat$y[newdat$range == "low-low"] <- one
  newdat$y[newdat$range == "medium-medium"] <- one
  newdat$y[newdat$range == "high-high"] <- one
  newdat$y[newdat$range == "low-medium"] <- two
  newdat$y[newdat$range == "medium-high"] <- two
  newdat$y[newdat$range == "low-high"] <- three
  
  newdat$x1[newdat$range == "low-low"] <- 1
  newdat$x2[newdat$range == "low-low"] <- 2
  newdat$x1[newdat$range == "medium-medium"] <- 3
  newdat$x2[newdat$range == "medium-medium"] <- 4
  newdat$x1[newdat$range == "high-high"] <- 5
  newdat$x2[newdat$range == "high-high"] <- 6
  newdat$x1[newdat$range == "low-medium"] <- 1.5
  newdat$x2[newdat$range == "low-medium"] <- 3.5
  newdat$x1[newdat$range == "medium-high"] <- 3.5
  newdat$x2[newdat$range == "medium-high"] <- 5.5
  newdat$x1[newdat$range == "low-high"] <- 1.5
  newdat$x2[newdat$range == "low-high"] <- 5.5
  
  newdat$h <- newdat$y+newdat$logRR
  
  plot(newdat$x1, newdat$y, pch=19, ylim =c(0, max(newdat$h)), xlim=c(1, 6), ylab="", xlab="", xaxt="n", yaxt="n", col="white")
  abline(v=2.5, b=0, lty=2);abline(v=4.5, b=0, lty=2)
  points(newdat$x2, newdat$h, pch=19, col="white")
  segments(newdat$x1, newdat$y,newdat$x2, newdat$h, col=paste(newdat$Colour))
  axis(1, at=c(1.75, 3.5, 5.50), labels=c("Low", "Medium", "High"), tick = FALSE)
  axis(2, at=c(one-1, one, one+1, two-1, two, two+1, three-1, three, three+1), labels = c(rep(c(0, 1, 2), time=3)), tick=FALSE, las=2)
  abline(h=(one+two)/2, lty=3)
  abline(h=(three+two)/2, lty=3)
  if(YieldorRichness == "yield"){title("log-RR - Yield ("%+%dataType%+%")")}
  if(YieldorRichness == "richness"){title("log-RR - Richness ("%+%dataType%+%")")}
}

CatsWhiskers_plot(data=ES.frame.yield, dataType="raw", YieldorRichness = "yield")
CatsWhiskers_plot(data=ES.frame.richness, dataType="raw", YieldorRichness = "richness")
