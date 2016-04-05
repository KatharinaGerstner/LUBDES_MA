###########################################################################
### Bayesian Analysis with fixed effects and random study- and study-case effects 
### and non-independence from relatedness of LUI comparisons within one study-case
##########################################################################

### MODEL SELECTION
# fixed effects with Variable selection based on random effects and a prior on the inclusion probability (Kuo & Mallick, cf. O'Hara & Sillanpää 2009)
cat("model{
    ### 1. priors 
    # variances and covariances between related sub-cases within the same study-case due to multiple LUI comparisons
    for(i in 1:N.obs){
    sigma.a[i] ~ dunif(0,5)
    }
    
    # study-case-specific effects 
    for(i in 1:N.Case){
    sigma.v[i] ~ dunif(0,5)  
    }
    
    # study-specific effects
    for(i in 1:N.Study){
    sigma.u[i] ~ dunif(0,5)
    }
    
    # fixed effects
    for(i in 1:N.colX){  
      beta[i] ~ dnorm(0,0.001)
    }
    
    ### 2. likelihood
    for(i in 1:N.obs){
    var.sum[i] <- Log.RR.Var[i] + sum(pow(sigma.a[i],2) * A[i,]) + pow(sigma.u[Study[i]],2) + pow(sigma.v[Study.Case[i]],2) # sum of within- + between-sample variance, cf. Nakagawa & Santos (2012) eqn 31
    tau[i] <- pow(var.sum[i],-1)
    Log.RR[i] ~ dnorm(mu[i], tau[i])
    mu[i] <- X[i,] %*% beta
    }
    
    ### 3. # Assess model fit using a sums-of-squares-type discrepancy (cf. p 106 Kery&Royle)
    for(i in 1:N.obs){
    residuals[i] <- Log.RR[i]-mu[i] # Residuals for observed data
    predictions[i] <- mu[i] # Predicted values
    }
}",file=path2temp %+% "bayesianMA_4.txt")

run.analysis <- function(model.name,X.matrix,ES.frame.richness,long.names){
  
  print("Prepare the data")
  dat2fit.richness <- list(
    Log.RR=ES.frame.richness$Log.RR, 
    Log.RR.Var=ES.frame.richness$Log.RR.Var,
    Study = ES.frame.richness$Study.ID, 
    Study.Case = ES.frame.richness$Study.Case,
    N.Study = nlevels(ES.frame.richness$Study.ID), # number of studies
    N.Case = nlevels(ES.frame.richness$Study.Case), # number of cases within studies
    N.obs = nrow(ES.frame.richness),
    A = M.matrix(ES.frame.richness),
    X = X.matrix,
    N.colX = ncol(X.matrix))
  
  print(str(dat2fit.richness))
  
  print("Fit the model")
  params2monitor <- c("beta","betaT","ind", "sigma.a","sigma.u", "sigma.v", "predictions","residuals")
  model.fit <- jags.model(data=dat2fit.richness, file=path2temp %+% "bayesianMA_4.txt", 
                          n.chains = 3, n.adapt=1000) # n.adapt for sampling the parameter space and conclude on one value
  update(model.fit, n.iter=2000) # start from this value
  samps <- vector("list",length(params2monitor))
  names(samps) <- params2monitor
  for(i in params2monitor){
    print("Monitor " %+% i)
    samps[[i]] <- coda.samples(model.fit, i, n.iter=8000, thin=4) # coda controls the chains, saves the samples  
  }
  
  print("Estimate goodness-of-fit")
  #  R2.LMM <- round(mean(1- unlist(lapply(samps[["residuals"]], function(x) mean(apply(x,1,var)))) / var(dat2fit.richness$Log.RR)),digits=3) # cf. Gelman, A. & Hill, J. (2007) Data Analysis Using Regression and Multilevel/Hierarchical Models
  var.f <- mean(unlist(lapply(samps[["predictions"]], function(x) apply(x,1,var))))
  sigma.a <- mean(unlist(lapply(samps[["sigma.a"]], function(x) apply(x,1,mean))))
  sigma.u <- mean(unlist(lapply(samps[["sigma.u"]], function(x) apply(x,1,mean))))
  sigma.v <- mean(unlist(lapply(samps[["sigma.v"]], function(x) apply(x,1,mean))))
  R2.LMM <- round(var.f / (var.f + sigma.a^2 + sigma.u^2 + sigma.v^2),digits=3) # cf. Nakagawa & Schielzeth (2012) eqn 26  
  dic.samps <- dic.samples(model.fit, n.iter=8000,thin=4)
  DIC <- round(sum(dic.samps[["deviance"]]) + sum(dic.samps[["penalty"]]),digits=3) # model deviance information criterion "deviance"=mean deviance, "penalty" = 2*pD
  bpvalue <- round(mean(unlist(samps[["bpvalue"]])),digits=3)
  goodness.of.fit <- data.frame(DIC=DIC,R2.LMM=R2.LMM,bpvalue=bpvalue)
  print(xtable(goodness.of.fit), type = "html", file=path2temp %+% model.name %+% "goodness.of.fit.doc") # save the HTML table as a .doc file
  save(model.name,model.fit,samps,goodness.of.fit, file=path2temp %+% model.name %+% ".Rdata")
  
  print("Check convergence")
  pdf(file=path2temp %+% "TracePlot" %+% model.name %+% ".pdf")
  for(i in c("beta","betaT","ind", "sigma.a","sigma.u", "sigma.v")){
    plot(samps[[i]]) ### check convergence visually
  }  
  dev.off()
  
  print("Posterior predictive check")
  residuals.mean <- summary(samps[["residuals"]])$statistics[,"Mean"]
  predictions.mean <- summary(samps[["predictions"]])$statistics[,"Mean"]
  plot(residuals.mean~predictions.mean, main=paste(model.name)) # should look like the sky at night
  
  print("Caterpillar plot")
  if(dat2fit.richness$N.colX==1){
    beta.mean <- summary(samps[["beta"]])$statistics["Mean"]
    beta.lb <- summary(samps[["beta"]])$quantiles["2.5%"]
    beta.ub <- summary(samps[["beta"]])$quantiles["97.5%"]    
  }
  if(dat2fit.richness$N.colX>1) {
    beta.mean <- summary(samps[["beta"]])$statistics[,"Mean"]
    beta.lb <- summary(samps[["beta"]])$quantiles[,"2.5%"]
    beta.ub <- summary(samps[["beta"]])$quantiles[,"97.5%"]    
  }
  
  plot.dat <- data.frame(beta.mean, beta.lb, beta.ub, long.names)
  p <- ggplot(data = plot.dat, aes(x = beta.mean, y = long.names)) + 
    geom_point() + 
    geom_segment(aes(x = beta.lb, xend = beta.ub, y = long.names, yend = long.names)) +
    geom_vline(xintercept = 0, linetype = 2) + 
    scale_x_continuous(labels=trans_format("exp",comma_format(digits=3))) +
    xlab("Response Ratio") + ylab("") +
    theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
  print(p)
  ggsave(p, file = path2temp %+% "CaterpillarPlot_" %+% model.name %+% ".png", width = 20, height = 8, type = "cairo-png")
  
  # save table of fixed effects as a .doc file
  dat.table <- xtable(plot.dat)
  print(dat.table, type = "html", file=path2temp %+% "model.output" %+% model.name %+% ".doc") 
  
  return(list(model.name,model.fit,samps))
}

### FULL MODEL
model.name <- "FullModel4"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level * (Species.Group + Product + BIOME -1), data=ES.frame.richness))
#names(X.matrix) <- c(levels(ES.frame.richness$LUI.range.level), levels(ES.frame.richness$Species.Group), levels(ES.frame.richness$Product), levels(ES.frame.richness$BIOME))# add interactions 
long.names <- colnames(X.matrix)
FullMA <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
