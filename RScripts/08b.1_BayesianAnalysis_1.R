###########################################################################
### Bayesian Analysis with fixed effects only
##########################################################################

## define the model

cat("model{
  ### 1. priors 
    # fixed effects
    for(i in 1:N.colX){  
    beta[i] ~ dnorm(0,0.001)
    }
    
    ### 2. likelihood (cf. Koricheva et al. (2013) eqn 11.3)
    for(i in 1:N.obs){
    tau[i] <- pow(Log.RR.Var[i],-1)
    Log.RR[i] ~ dnorm(mu[i], tau[i])
    mu[i] <- X[i,] %*% beta
    }
    
    ### 3. # Assess model fit using a sums-of-squares-type discrepancy (cf. p 106 Kery&Royle)
    for(i in 1:N.obs){
    residuals[i] <- Log.RR[i]-mu[i] # Residuals for observed data
    predictions[i] <- mu[i] # Predicted values
    sq.res[i] <- pow(residuals[i],2) # Squared residuals for observed data
    Log.RR.new[i] ~ dnorm(mu[i],tau[i]) # one new data set at each MCMC iteration
    sq.res.new[i] <- pow(Log.RR.new[i]-predictions[i],2) # Squared residuals for new data
    }    
    SSR <- sum(sq.res[]) # Sum of squared residuals for actual data set
    SSR.new <- sum(sq.res.new[]) # Sum of squared residuals for new data set
    test <- step(SSR-SSR.new) # Test whether new data set more extreme, step() tests for x > 0
    bpvalue <- mean(test) # Bayesian p-value, cf. Kery (2010) Introduction to WinBUGS for ecologists, p106ff 
  }",file=path2temp %+% "bayesianMA_1.txt")

run.analysis <- function(model.name,X.matrix,ES.df,long.names){

  print("Prepare the data")
  dat2fit <- list(
    Log.RR=ES.df$Log.RR, 
    Log.RR.Var=ES.df$Log.RR.Var,
    N.obs = nrow(ES.df),
    X = X.matrix,
    N.colX = ncol(X.matrix))
  
  print(str(dat2fit))

  print("Fit the model")
  params2monitor <- c("beta","SSR","SSR.new","bpvalue", "predictions","residuals")
  model.fit <- jags.model(data=dat2fit, file=path2temp %+% "bayesianMA_1.txt", 
                          n.chains = Nchains, n.adapt=Nadapt) # n.adapt for sampling the parameter space and conclude on one value
  update(model.fit, n.iter=Nstart) # start from this value
  samps <- vector("list",length(params2monitor))
  names(samps) <- params2monitor
  for(i in params2monitor){
    print("Monitor " %+% i)
    samps[[i]] <- coda.samples(model.fit, i, n.iter=Niter, thin=Nthin) # coda controls the chains, saves the samples  
  }

  print("Estimate goodness-of-fit")
  # R² for multilevel models cf. Gelman, A. & Hill, J. (2007) Data Analysis Using Regression and Multilevel/Hierarchical Models, p. 474 eqn (21.8)
  var.res.sims <- matrix(unlist(lapply(samps[["residuals"]], function(x) apply(x,1,var))),byrow=F,ncol=3)
  var.res <- numeric(length=dat2fit$N.obs)
  var.Log.RR <- numeric(length=dat2fit$N.obs)
  for(k in 1:dat2fit$N.colX){
    var.res.k <- var.res.sims[X.matrix[,k]==1,]
    var.res[k] <- var(apply(var.res.k,1,mean)) # average over 3 MCMC chains and calculate the variance
    var.Log.RR[k] <- var(dat2fit$Log.RR[X.matrix[,k]==1])
  }
  R2.GH <- 1-mean(var.res)/mean(var.Log.RR)
  dic.samps <- dic.samples(model.fit, n.iter=Niter,thin=Nthin)
  DIC <- round(mean(dic.samps[["deviance"]] + dic.samps[["penalty"]]),digits=3) # model deviance information criterion "deviance"=mean deviance, "penalty" = 2*pD
  bpvalue <- round(mean(unlist(samps[["bpvalue"]])),digits=3)
  goodness.of.fit <- data.frame(DIC=DIC,R2.GH=R2.GH,bpvalue=bpvalue)
  print(xtable(goodness.of.fit), type = "html", file=path2temp %+% model.name %+% "goodness.of.fit.doc") # save the HTML table as a .doc file
  save(model.name,model.fit,samps,goodness.of.fit, file=path2temp %+% model.name %+% ".Rdata")
  
  print("Check convergence")
  pdf(file=path2temp %+% "TracePlot" %+% model.name %+% ".pdf")
  for(i in c("beta")){
    plot(samps[[i]]) ### check convergence visually
  }  
  dev.off()

  print("Posterior predictive check")
  residuals.mean <- summary(samps[["residuals"]])$statistics[,"Mean"]
  predictions.mean <- summary(samps[["predictions"]])$statistics[,"Mean"]
  plot(residuals.mean~predictions.mean, main=paste(model.name)) # should look like the sky at night
     
  print("Caterpillar plot")
  if(dat2fit$N.colX==1){
    beta.mean <- summary(samps[["beta"]])$statistics["Mean"]
    beta.lb <- summary(samps[["beta"]])$quantiles["2.5%"]
    beta.ub <- summary(samps[["beta"]])$quantiles["97.5%"]    
  }
  if(dat2fit$N.colX>1) {
    beta.mean <- summary(samps[["beta"]])$statistics[,"Mean"]
    beta.lb <- summary(samps[["beta"]])$quantiles[,"2.5%"]
    beta.ub <- summary(samps[["beta"]])$quantiles[,"97.5%"]    
  }
  
  plot.dat <- data.frame(beta.mean, beta.lb, beta.ub, long.names, sortvar=letters[1:length(beta.mean)])
  p <- ggplot(data = plot.dat, aes(x = beta.mean, y = sortvar)) + 
    geom_point() + 
    geom_segment(aes(x = beta.lb, xend = beta.ub, y = sortvar, yend = sortvar)) +
    geom_vline(xintercept = 0, linetype = 2) + 
    scale_x_continuous(labels=trans_format("exp",comma_format(digits=3))) +
    scale_y_discrete(labels=rev(long.names)) +
    xlab("Response Ratio") + ylab("") +
    theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
  print(p)
  ggsave(p, file = path2temp %+% "CaterpillarPlot_" %+% model.name %+% ".png", width = 20, height = 8, type = "cairo-png")
  
  # save table of fixed effects as a .doc file
  print(xtable(plot.dat,digits=4), type = "html", file=path2temp %+% "model.output" %+% model.name %+% ".doc") 

  return(list(model.name,model.fit,samps))
}

fit.tab <- data.frame(DIC=numeric(length=6),R2LMM=numeric(length=6),R2GH=numeric(length=6),bpvalue=numeric(length=6))
Richness.MA.model <- Yield.MA.model <- vector("list", length=4)
names(Richness.MA.model) <- names(Yield.MA.model) <- c("None","LUI","Full")

### 1. within-study intensification
model.name <- "Richness_GrandMean1"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ 1, data=ES.frame.richness))
Richness.MA.model[["None"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,"Grand Mean")
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[1,] <- goodness.of.fit
rownames(fit.tab)[1] <- model.name

### 2. Analysis with covariates
### LUI.RANGE.LEVEL
model.name <- "Richness_LUI.range.level1"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level-1, data=ES.frame.richness))
names(X.matrix) <- levels(ES.frame.richness$LUI.range.level)
long.names <- colnames(X.matrix)
Richness.MA.model[["LUI"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[2,] <- goodness.of.fit
rownames(fit.tab)[2] <- model.name

### FULL MODEL
model.name <- "Richness_FullModel1"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level * (Species.Group + Product + BIOME -1), data=ES.frame.richness))
long.names <- colnames(X.matrix)
Richness.MA.model[["Full"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[3,] <- goodness.of.fit
rownames(fit.tab)[3] <- model.name

##################################
### 1. within-study intensification
model.name <- "Yield_GrandMean1"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ 1, data=ES.frame.yield))
Yield.MA.model[["None"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,"Grand Mean")
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[4,] <- goodness.of.fit
rownames(fit.tab)[4] <- model.name

### 2. Analysis with covariates
### LUI.RANGE.LEVEL
model.name <- "Yield_LUI.range.level1"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level-1, data=ES.frame.yield))
names(X.matrix) <- levels(ES.frame.yield$LUI.range.level)
long.names <- colnames(X.matrix)
Yield.MA.model[["LUI"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[5,] <- goodness.of.fit
rownames(fit.tab)[5] <- model.name

### FULL MODEL
model.name <- "Yield_FullModel1"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level * (Product + BIOME) - 1, data=ES.frame.yield))
long.names <- colnames(X.matrix)
Yield.MA.model[["Full"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[6,] <- goodness.of.fit
rownames(fit.tab)[6] <- model.name

write.csv(fit.tab, file=path2temp %+% "fit.tab1.csv") 
