############################################################################
### Purpose of this skript module 08b is to:
###
### 08 08b BayesianAnalysis
### 08b.1. Bayesian analysis of fixed effects only
### 08b.2. Bayesian analysis of fixed and random effects of study and study-case
### 08b.3. Bayesian analysis of fixed and random effects of study and study-case, and non-independence from relatedness of LUI comparisons within one study-case
### 08b.4. run model selection using BIC: 08b.4_BMASelect.R for fixed and random effects of study and study-case, and non-independence from relatedness of LUI comparisons within one study-case AND bayesian model selection using DIC
### General comments:
### * 
###
### Authors: KG ...
############################################################################

############################################################################
### Bayesian model setup
############################################################################

## define the models
### 08b.1. Bayesian analysis of fixed effects only
cat("model{
  ### 1. priors 
    # fixed effects
    for(i in 1:N.colX){  
    beta[i] ~ dnorm(0,0.001)
    }
    
    # within-study-case variances  
    for(i in 1:N.obs){
      sigma.within[i] ~ dunif(0,100)      
    }

    ### 2. likelihood (cf. Koricheva et al. (2013) eqn 11.3)
    for(i in 1:N.obs){
    var.sum[i] <- Log.RR.Var[i] + pow(sigma.within[i],2) # sum of within- + between-sample variance, cf. Nakagawa & Santos (2012) eqn 31
    tau[i] <- pow(var.sum[i],-1)
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

### 08b.2. Bayesian analysis of fixed and random effects of study and study-case
cat("model{
  ### 1. priors 
  # study-case-specific effects 
  for(i in 1:N.Case){
    sigma.v[i] ~ dunif(0,5)  
  }
  
  # within-study-case variances  
  for(i in 1:N.obs){
    sigma.within[i] ~ dunif(0,100)      
  }

# fixed effects
  for(i in 1:N.colX){  
    beta[i] ~ dnorm(0,0.001)
  }
  
  ### 2. likelihood (cf. Koricheva et al. (2013) eqn 11.3)
  for(i in 1:N.obs){
    var.sum[i] <- Log.RR.Var[i] + pow(sigma.within[i],2) + pow(sigma.v[Study.Case[i]],2) # sum of within- + between-sample variance, cf. Nakagawa & Santos (2012) eqn 31
    tau[i] <- pow(var.sum[i],-1)
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
}",file=path2temp %+% "bayesianMA_2.txt")

### 08b.3. Bayesian analysis of fixed and random effects of study and study-case, and non-independence from relatedness of LUI comparisons within one study-case
cat("model{
  ### 1. priors 
    # study-case-specific effects 
    for(i in 1:N.Case){
    sigma.v[i] ~ dunif(0,100)  
    }
      
    # within-study-case variances  
    for(i in 1:N.obs){
      sigma.within[i] ~ dunif(0,100)      
    }

    # variances and covariances between related sub-cases within the same study-case due to multiple LUI comparisons
    for(i in 1:N.obs){
      sigma.a[i] ~ dunif(0,100)
    }
    
    # fixed effects
    for(i in 1:N.colX){  
     beta[i] ~ dnorm(0,0.001)
    }
    
    ### 2. likelihood
    for(i in 1:N.obs){
    var.sum[i] <- Log.RR.Var[i] + pow(sigma.within[i],2) + sum(pow(sigma.a[i],2) * A[i,]) + pow(sigma.v[Study.Case[i]],2) # sum of within- + between-sample variance, cf. Nakagawa & Santos (2012) eqn 31
    tau[i] <- pow(var.sum[i],-1)
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
    }",file=path2temp %+% "bayesianMA_3.txt")

############################################################################
### function for calling 
############################################################################

bayesMA <- function(BayesianModel){
  dat2fit <- NULL
  paramsderived <- c("SSR","SSR.new","bpvalue", "predictions","residuals")
  if(BayesianModel=="bayesianMA_1"){
    dat2fit <- list(
      Log.RR=ES.df$Log.RR, 
      Log.RR.Var=ES.df$Log.RR.Var,
      N.obs = nrow(ES.df),
      X = X.matrix,
      N.colX = ncol(X.matrix))
    params2monitor <- c("beta","sigma.within")
  }
  if(BayesianModel=="bayesianMA_2"){
    dat2fit <- list(
      Log.RR=ES.df$Log.RR, 
      Log.RR.Var=ES.df$Log.RR.Var,
      Study.Case = ES.df$Study.Case,
      N.Case = nlevels(ES.df$Study.Case), # number of cases within studies
      N.obs = nrow(ES.df),
      X = X.matrix,
      N.colX = ncol(X.matrix))
    params2monitor <- c("beta","sigma.within","sigma.v")
  }
  if(BayesianModel=="bayesianMA_3"){
    dat2fit <- list(
      Log.RR=ES.df$Log.RR, 
      Log.RR.Var=ES.df$Log.RR.Var,
      N.obs = nrow(ES.df),
      Study.Case = ES.df$Study.Case,
      N.Case = nlevels(ES.df$Study.Case), # number of cases within studies
      A = M.matrix(ES.df),
      X = X.matrix,
      N.colX = ncol(X.matrix))    
    params2monitor <- c("beta","sigma.within","sigma.a","sigma.v")
  }
  if(is.null(dat2fit)){ 
    print("Model name is not valid.")
    break
  }
  
  print(str(dat2fit))
  print("Fit the model")
  model.fit <- jags.model(data=dat2fit, file=path2temp %+% BayesianModel %+% ".txt", 
                          n.chains = Nchains, n.adapt=Nadapt) # n.adapt for sampling the parameter space and conclude on one value
  update(model.fit, n.iter=Nstart) # start from this value
  samps <- vector("list",length(params2monitor))
  names(samps) <- params2monitor
  for(i in c(params2monitor,paramsderived)){
    print("Monitor " %+% i)
    samps[[i]] <- coda.samples(model.fit, i, n.iter=Niter, thin=Nthin) # coda controls the chains, saves the samples  
  }
  
  print("Estimate goodness-of-fit")
  # R? for multilevel models cf. Gelman, A. & Hill, J. (2007) Data Analysis Using Regression and Multilevel/Hierarchical Models, p. 474 eqn (21.8)
  var.res.sims <- matrix(unlist(lapply(samps[["residuals"]], function(x) apply(x,1,var))),byrow=F,ncol=3)
  var.res <- numeric(length=dat2fit$N.obs)
  var.Log.RR <- numeric(length=dat2fit$N.obs)
  for(k in 1:dat2fit$N.colX){
    var.res.k <- var.res.sims[X.matrix[,k]==1,]
    var.res[k] <- var(apply(var.res.k,1,mean)) # average over 3 MCMC chains and calculate the variance
    var.Log.RR[k] <- var(dat2fit$Log.RR[X.matrix[,k]==1])
  }
  R2.GH <- 1-mean(var.res,na.rm=T)/mean(var.Log.RR,na.rm=T)
  R2.LMM.marginal <- NA
  R2.LMM.conditional <- NA
  if(length(params2monitor>1)){
    var.f <- mean(unlist(lapply(samps[["predictions"]], function(x) apply(x,1,var))))
    sigma.within <- mean(unlist(lapply(samps[["sigma.within"]], function(x) apply(x,1,mean))))
    sigma.a <- ifelse("sigma.a" %in% params2monitor,mean(unlist(lapply(samps[["sigma.a"]], function(x) apply(x,1,mean)))),0)
    sigma.v <- ifelse("sigma.v" %in% params2monitor,mean(unlist(lapply(samps[["sigma.v"]], function(x) apply(x,1,mean)))),0)
    R2.LMM.marginal <- round(var.f / (var.f + sigma.a^2 + sigma.v^2 + sigma.within^2),digits=3) # cf. Nakagawa & Schielzeth (2012) eqn 26    
    R2.LMM.conditional <- round((var.f + sigma.a^2 + sigma.v^2) / (var.f + sigma.a^2 + sigma.v^2 + sigma.within^2),digits=3) # cf. Nakagawa & Schielzeth (2012) eqn 30    
  }
  dic.samps <- dic.samples(model.fit, n.iter=Niter,thin=Nthin)
  DIC <- round(mean(dic.samps[["deviance"]] + dic.samps[["penalty"]]),digits=3) # model deviance information criterion "deviance"=mean deviance, "penalty" = 2*pD
  bpvalue <- round(mean(unlist(samps[["bpvalue"]])),digits=3)
  goodness.of.fit <- data.frame(DIC=DIC,R2.LMM.m=R2.LMM.marginal,R2.LMM.c=R2.LMM.conditional,R2.GH,bpvalue=bpvalue)
  print(xtable(goodness.of.fit,digits=4), type = "html", file=path2temp %+% model.name %+% "goodness.of.fit.doc") # save the HTML table as a .doc file
  
  save(model.name,model.fit,samps,goodness.of.fit, file=path2temp %+% model.name %+% ".Rdata")
#    return(list(model.name=model.name,model.fit=model.fit,samps=samps))

}

model.list <- data.frame(complexity_level=c(rep(1,8),rep(2,8),rep(3,8)), model.name = apply(expand.grid(c("Richness","Yield"),c("GrandMean","LUI.range.level","FullModel","SelectModel"),1:3),1,function(x) paste0(x,collapse="_")), preds = rep(c("1", "1","LUI.range.level-1", "LUI.range.level-1","LUI.range.level * (Species.Group + Product + BIOME)", "LUI.range.level * (Product + BIOME)",1,1),3))
model.list <- model.list[-which(model.list$model.name %in% c("Richness_SelectModel_1","Richness_SelectModel_2","Yield_SelectModel_1","Yield_SelectModel_2")),]
n.models <- nrow(model.list)
fit.tab <- data.frame(DIC=numeric(length=n.models),R2.LMM.m=numeric(length=n.models),R2.LMM.c=numeric(length=n.models),R2GH=numeric(length=n.models),bpvalue=numeric(length=n.models))

### 08b.4. run model selection using BIC: 08b.4_BMASelect.R for fixed and random effects of study and study-case, and non-independence from relatedness of LUI comparisons within one study-case AND bayesian model selection using DIC
source(path2wd %+% "08b.4_BMA_Select.R")

############################################################################
### extract fit statistics
############################################################################
for (model.name in model.list$model.name[19:20]){
  row.select <- which(model.list$model.name==model.name)
  print(model.name)
  BayesianModel <- "bayesianMA_" %+% model.list$complexity_level[row.select]
  if(length(grep("Richness",model.name))>0) ES.df <- ES.frame.richness
  if(length(grep("Yield",model.name))>0) ES.df <- ES.frame.yield
  X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR ~", model.list$preds[row.select])), data=ES.df))
  if (model.name == "Richness_SelectModel_3") {
    X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR~",paste(reduced.model.richness$terms,collapse="+"))), data=ES.df))
  }  
  if (model.name == "Yield_SelectModel_3"){
    X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR~",paste(reduced.model.yield$terms,collapse="+"))), data=ES.df))
  }
  long.names <- ifelse(length(grep("GrandMean",model.name))==1,"Grand Mean",colnames(X.matrix))
  bayesMA(BayesianModel)
  load(path2temp %+% model.name %+% ".Rdata")
  fit.tab[row.select,] <- goodness.of.fit
  rownames(fit.tab)[row.select] <- model.name
}

write.csv(fit.tab, file=path2temp %+% "fit.tab.csv") 
