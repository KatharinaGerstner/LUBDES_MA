###########################################################################
### Bayesian Analysis with fixed effects and random study- and study-case effects 
### and non-independence from relatedness of LUI comparisons within one study-case
##########################################################################

###########################################################################
### run a bayesian model
##########################################################################
## define the model
cat("model{
  ### 1. priors 
  # study-case-specific effects 
  for(i in 1:N.Case){
    sigma.v[i] ~ dunif(0,5)  
  }
    
  # variances and covariances between related sub-cases within the same study-case due to multiple LUI comparisons
  for(i in 1:N.obs){
    sigma.a[i] ~ dunif(0,5)
  }
  
  # fixed effects
  for(i in 1:N.colX){  
    beta[i] ~ dnorm(0,0.001)
  }
  
  ### 2. likelihood
  for(i in 1:N.obs){
    var.sum[i] <- Log.RR.Var[i] + sum(pow(sigma.a[i],2) * A[i,]) + pow(sigma.v[Study.Case[i]],2) # sum of within- + between-sample variance, cf. Nakagawa & Santos (2012) eqn 31
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

run.analysis <- function(model.name,X.matrix,ES.df,long.names){

  print("Prepare the data")
  dat2fit.richness <- list(
    Log.RR=ES.df$Log.RR, 
    Log.RR.Var=ES.df$Log.RR.Var,
    N.obs = nrow(ES.df),
    Study.Case = ES.df$Study.Case,
    N.Case = nlevels(ES.df$Study.Case), # number of cases within studies
    A = M.matrix(ES.df),
    X = X.matrix,
    N.colX = ncol(X.matrix))
  
  print(str(dat2fit.richness))

  print("Fit the model")
  params2monitor <- c("beta", "sigma.a","sigma.v","bpvalue", "predictions","residuals")
  model.fit <- jags.model(data=dat2fit.richness, file=path2temp %+% "bayesianMA_3.txt", 
                          n.chains = 3, n.adapt=1000) # n.adapt for sampling the parameter space and conclude on one value
  update(model.fit, n.iter=2000) # start from this value
  samps <- vector("list",length(params2monitor))
  names(samps) <- params2monitor
  for(i in params2monitor){
    print("Monitor " %+% i)
    samps[[i]] <- coda.samples(model.fit, i, n.iter=10000, thin=4) # coda controls the chains, saves the samples  
  }

  print("Estimate goodness-of-fit")
  R2.GH <- round(1- mean(unlist(lapply(samps[["residuals"]], function(x) apply(x,1,var)))) / var(dat2fit.richness$Log.RR),digits=3) # cf. Gelman, A. & Hill, J. (2007) Data Analysis Using Regression and Multilevel/Hierarchical Models, p. 473ff
  var.f <- mean(unlist(lapply(samps[["predictions"]], function(x) apply(x,1,var))))
  sigma.a <- mean(unlist(lapply(samps[["sigma.a"]], function(x) apply(x,1,mean))))
  sigma.v <- mean(unlist(lapply(samps[["sigma.v"]], function(x) apply(x,1,mean))))
  R2.LMM <- round(var.f / (var.f + sigma.a^2 + sigma.v^2),digits=3) # cf. Nakagawa & Schielzeth (2012) eqn 26  
  dic.samps <- dic.samples(model.fit, n.iter=10000,thin=4)
  DIC <- round(mean(dic.samps[["deviance"]]) + mean(dic.samps[["penalty"]]),digits=3) # model deviance information criterion "deviance"=mean deviance, "penalty" = 2*pD
  bpvalue <- round(mean(unlist(samps[["bpvalue"]])),digits=3)
  goodness.of.fit <- data.frame(DIC=DIC,R2.LMM=R2.LMM,R2.GH,bpvalue=bpvalue)
  print(xtable(goodness.of.fit), type = "html", file=path2temp %+% model.name %+% "goodness.of.fit.doc") # save the HTML table as a .doc file
  save(model.name,model.fit,samps,goodness.of.fit, file=path2temp %+% model.name %+% ".Rdata")
  
  print("Check convergence")
  pdf(file=path2temp %+% "TracePlot" %+% model.name %+% ".pdf")
  for(i in c("beta", "sigma.a", "sigma.v")){
    plot(samps[[i]]) ### check convergence visually
  }  
  dev.off()

  print("Posterior predictive check")
  residuals.mean <- summary(samps[["residuals"]])$statistics[,"Mean"]
  predictions.mean <- summary(samps[["predictions"]])$statistics[,"Mean"]
  png(path2temp %+% "residuals_vs_predictions_" %+% model.name %+% ".png")
  plot(residuals.mean~predictions.mean, main=paste(model.name)) # should look like the sky at night
  dev.off()

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
  
  plot.dat <- data.frame(beta.mean, beta.lb, beta.ub, long.names, sortvar=letters[1:length(beta.mean)])
  p <- ggplot(data = plot.dat, aes(x = beta.mean, y = sortvar)) + 
    geom_point() + 
    geom_segment(aes(x = beta.lb, xend = beta.ub, y = sortvar, yend = sortvar)) +
    geom_vline(xintercept = 0, linetype = 2) + 
    scale_x_continuous(labels=trans_format("exp",comma_format(digits=3))) +
    scale_y_discrete(labels=long.names) +
    xlab("Response Ratio") + ylab("") +
    theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
  print(p)
  ggsave(p, file = path2temp %+% "CaterpillarPlot_" %+% model.name %+% ".png", width = 20, height = 8, type = "cairo-png")
  
  # save table of fixed effects as a .doc file
  dat.table <- xtable(plot.dat)
  print(dat.table, type = "html", file=path2temp %+% "model.output_" %+% model.name %+% ".doc") 
  
  return(list(model.name=model.name,model.fit=model.fit,samps=samps))
}

fit.tab <- data.frame(DIC=numeric(length=8),R2LMM=numeric(length=8),R2GH=numeric(length=8),bpvalue=numeric(length=8))

Richness.MA.model <- Yield.MA.model <- vector("list", length=4)
names(Richness.MA.model) <- names(Yield.MA.model) <- c("None","LUI","Full","Select")


### 1. within-study intensification
model.name <- "Richness_GrandMean3"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ 1, data=ES.frame.richness))
Richness.MA.model[["None"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,"Grand Mean")
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[1,] <- goodness.of.fit
rownames(fit.tab)[1] <- model.name

### 2. Analysis with covariates
### LUI.RANGE.LEVEL
model.name <- "Richness_LUI.range.level3"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level-1, data=ES.frame.richness))
names(X.matrix) <- levels(ES.frame.richness$LUI.range.level)
long.names <- colnames(X.matrix)
Richness.MA.model[["LUI"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[2,] <- goodness.of.fit
rownames(fit.tab)[2] <- model.name

### FULL MODEL
model.name <- "Richness_FullModel3"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level * (Species.Group + Product + BIOME -1), data=ES.frame.richness))
long.names <- colnames(X.matrix)
Richness.MA.model[["Full"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[3,] <- goodness.of.fit
rownames(fit.tab)[3] <- model.name

##################################
### 1. within-study intensification
model.name <- "Yield_GrandMean3"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ 1, data=ES.frame.yield))
Yield.MA.model[["None"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,"Grand Mean")
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[5,] <- goodness.of.fit
rownames(fit.tab)[5] <- model.name

### 2. Analysis with covariates
### LUI.RANGE.LEVEL
model.name <- "Yield_LUI.range.level3"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level-1, data=ES.frame.yield))
names(X.matrix) <- levels(ES.frame.yield$LUI.range.level)
long.names <- colnames(X.matrix)
Yield.MA.model[["LUI"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[6,] <- goodness.of.fit
rownames(fit.tab)[6] <- model.name

### FULL MODEL
model.name <- "Yield_FullModel3"
X.matrix <- as.data.frame(model.matrix(Log.RR ~ LUI.range.level * (Product + BIOME) - 1, data=ES.frame.yield))
long.names <- colnames(X.matrix)
Yield.MA.model[["Full"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,long.names)
load(path2temp %+% model.name %+% ".Rdata")
fit.tab[7,] <- goodness.of.fit
rownames(fit.tab)[7] <- model.name

### Model
source(path2wd %+% "08b.4_BMA_Select.R")
model.name <- "Richness_SelectModel3"
X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR~",paste(reduced.model.richness$terms,collapse="+"),-1)), data=ES.frame.richness))
long.names <- colnames(X.matrix)
Richness.MA.model[["Select"]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
fit.tab[4,] <- goodness.of.fit
rownames(fit.tab)[4] <- model.name

model.name <- "Yield_SelectModel3"
X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR~",paste(reduced.model.yield$terms,collapse="+"),-1)), data=ES.frame.yield))
long.names <- colnames(X.matrix)
Yield.MA.model[["Select"]] <- run.analysis(model.name,X.matrix,ES.frame.yield,long.names)
fit.tab[8,] <- goodness.of.fit
rownames(fit.tab)[8] <- model.name

print(xtable(fit.tab), type = "html", file=path2temp %+% "fit.tab.doc") 
