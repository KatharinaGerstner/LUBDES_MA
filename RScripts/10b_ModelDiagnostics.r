############################################################################
### Purpose of this skript module 09 is to:
###
### 09.1. model diagnostics
### 09.2. model performances
###
### General comments:
###
### Authors: KG
############################################################################

model.list <- list(Richness.MA.model[["None"]],Richness.MA.model[["LUI"]],Richness.MA.model[["Full"]],Richness.MA.model[["Select"]],Yield.MA.model[["None"]],Yield.MA.model[["LUI"]],Yield.MA.model[["Full"]],Yield.MA.model[["Select"]])
for(model in model.list){
  print(model$model.name)  
  print("Check convergence")
  pdf(file=path2temp %+% "TracePlot" %+% model$model.name %+% ".pdf")
  for(i in c("beta", "sigma.within","sigma.a", "sigma.v")){
    if(!(i %in% names(model$samps))) next
    plot(model$samps[[i]]) ### check convergence visually
  }  
  dev.off()
  
  print("Posterior predictive check")
  png(path2temp %+% "bpPlot.png")
  SSR <- apply(matrix(unlist(model$samps[["SSR"]]),ncol=3,byrow=F),1,mean)
  SSR.new <- apply(matrix(unlist(model$samps[["SSR.new"]]),ncol=3,byrow=F),1,mean)
  plot(SSR,SSR.new, xlim=range(SSR,SSR.new), ylim=range(SSR,SSR.new), xlab = "Discrepancy actual data", ylab = "Discrepancy replicate data",
       main = model$model.name %+% '\n Bp-value: ' %+%   round(mean(unlist(model$samps[["bpvalue"]])),digits=3))
  abline(0, 1, col = "black")
  dev.off()
  
  # Residuals and predictions for all datapoints averaged over 3 MCMC chains and all simulations
  residuals.mean <- summary(model$samps[["residuals"]])$statistics[,"Mean"]
  predictions.mean <- summary(model$samps[["predictions"]])$statistics[,"Mean"]

  png(path2temp %+% "residuals_vs_predictions_" %+% model$model.name %+% ".png")
  plot(residuals.mean~predictions.mean, main=paste(model.name),xlab="Residual",ylab="Predicted value") # should look like the sky at night
  abline(0, 0, col = "black")
  dev.off()
  
}
