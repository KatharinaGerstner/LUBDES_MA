############################################################################
### Purpose of this skript module 10b is to:
###
### 10b.1. model diagnostics, i.e. convergence check and posterior predictive check
### 10b.2. model performances, i.e. residuals vs predictions
###
### General comments:
###
### Authors: KG
############################################################################

model.list <- data.frame(complexity_level=c(rep(1,8),rep(2,8),rep(3,8)), model.name = apply(expand.grid(c("Richness","Yield"),c("GrandMean","LUI.range.level","FullModel","SelectModel"),1:3),1,function(x) paste0(x,collapse="_")), preds = rep(c("1", "1","LUI.range.level-1", "LUI.range.level-1","LUI.range.level * (Species.Group + Product + BIOME)", "LUI.range.level * (Product + BIOME)","",""),3))
model.list <- model.list[-which(model.list$model.name %in% c("Richness_SelectModel_1","Richness_SelectModel_2","Yield_SelectModel_1","Yield_SelectModel_2")),]
n.models <- nrow(model.list)
fit.tab <- data.frame(DIC=numeric(length=n.models),R2LMM=numeric(length=n.models),R2GH=numeric(length=n.models),bpvalue=numeric(length=n.models))
MA.model <- vector("list", length=n.models)

for (model.name in model.list$model.name){
  
  row.select <- which(model.list$model.name==model.name)
  
  load(path2temp %+% model.name %+% ".Rdata") 
  
  print("Fit statistics")
  
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
  plot(residuals.mean~predictions.mean, main=paste(model.name),ylab="Residual",xlab="Predicted value") # should look like the sky at night
  abline(0, 0, col = "black")
  dev.off()
  
}
