############################################################################
### Purpose of this skript module 08.2 is to:
###
### Forest plots per LUI category and covariate
###
### General comments:
### - symbolize covariates by different symbols and richness vs yield by colors
### - offset RRs for yield by +0.1 and for richness by -0.1
###
### Authors: KG, ...
############################################################################

############################################################################
### 08.2
### 
### 
############################################################################

Predict.RR <- function(data,YieldorRichness = c("yield", "richness"), model){

  ## produce matrix with all level combinations, i.e. groups
  if(YieldorRichness == "yield")  newdat <- expand.grid(LUI.range.level=levels(data$LUI.range.level), Product=levels(data$Product))
  if(YieldorRichness == "richness")  newdat <- expand.grid(LUI.range.level=levels(data$LUI.range.level))
  
  ## predict RRs and SDs per group
  mm <- model.matrix(model$call$mods, data=newdat)
  mm <- mm[,-which(colnames(mm)=="(Intercept)")]
  preds <- predict.rma(model, newmods = mm)
  newdat$logRR <- preds$pred
  newdat$logRR.se <- preds$se
  names(newdat)[names(newdat) == "LUI.range.level"] <- "range"
  names(newdat)[names(newdat) == "Product"] <- "covariate"
  return(newdat)
}  
  
newdat.yield <- Predict.RR(data=ES.frame.yield, YieldorRichness = "yield", model=Yield.MA.model[["select"]]$model)
newdat.richness <- Predict.RR(data=ES.frame.richness, YieldorRichness = "richness", model=Richness.MA.model[["select"]]$model)

one=1; two=5; three=9; alpha=100; covariate ="Product"
  
## define colours per group
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
newdat$x1[newdat$range == "medium-medium"] <- 3
newdat$x1[newdat$range == "high-high"] <- 5
newdat$x1[newdat$range == "low-medium"] <- 2
newdat$x1[newdat$range == "medium-high"] <- 4
newdat$x1[newdat$range == "low-high"] <- 3

newdat$h <- newdat$y+newdat$logRR

## forest plot
plot(newdat$x1, newdat$y, pch=19, ylim =c(0, max(newdat$h+2*newdat$logRR.se)), xlim=c(1, 6), ylab="", xlab="", xaxt="n", yaxt="n", col="white")
#abline(v=2.7, b=0, lty=2); abline(v=4.7, b=0, lty=2)
abline(h=one, lty=3)
abline(h=two, lty=3)
abline(h=three, lty=3)
axis(1, at=c(1.75, 3.5, 5.50), labels=c("Low", "Medium", "High"), tick = FALSE)
axis(2, at=c(one,two,three), labels = rep(1, time=3), tick=FALSE, las=2)

## yield
for(i in 1:length(levels(newdat$covariate))){
subdat <- newdat[newdat$covariate==levels(newdat$covariate)[i],]
points(subdat$x1+0.1*i, subdat$h, pch=15+i, col=paste(subdat$Colour))
segments(subdat$x1+0.1*i, subdat$h-1.96*subdat$logRR.se, subdat$x1+0.1*i, subdat$h+1.96*subdat$logRR.se, col=paste(subdat$Colour))
}

## richness
legend()

