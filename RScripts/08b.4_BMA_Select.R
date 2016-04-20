### Model selection

run.analysis4select <- function(model.name,X.matrix,ES.df){
  
  print("Prepare the data")
  dat2fit <- list(
    Log.RR=ES.df$Log.RR, 
    Log.RR.Var=ES.df$Log.RR.Var,
    N.obs = nrow(ES.df),
    Study.Case = ES.df$Study.Case,
    N.Case = nlevels(ES.df$Study.Case), # number of cases within studies
    A = M.matrix(ES.df),
    X = X.matrix,
    N.colX = ncol(X.matrix))
  
  print(str(dat2fit))
  
  model.fit <- jags.model(data=dat2fit, file=path2temp %+% BayesianModel %+% ".txt", 
                          n.chains = Nchains, n.adapt=Nadapt) # n.adapt for sampling the parameter space and conclude on one value
  update(model.fit, n.iter=Nstart) # start from this value
  dic.samps <- dic.samples(model.fit, n.iter=Niter,thin=Nthin)
  DIC <- round(mean(dic.samps[["deviance"]] + dic.samps[["penalty"]]),digits=3) # model deviance information criterion "deviance"=mean deviance, "penalty" = 2*pD
  return(list(model.fit=model.fit,DIC=DIC))
}


BMASelect <- function(model, ES.df){
  
  allTerms <- trim(strsplit(paste(model),'[+]')[[3]])
  
  X.matrix <- as.data.frame(model.matrix(model, data=ES.df))
  currentModel <- run.analysis4select(model.name="FullModel",X.matrix,ES.df)
  currentTerms <- allTerms
  stats.list <- vector("list",length=length(allTerms))
  i <- 1
  
  while(ncol(X.matrix)>1){ # as long as we can drop variables
    model.new <- vector("list",length=length(currentTerms))
    newModel <- vector("list",length=length(currentTerms))
    deltaDIC <- numeric(length(currentTerms))
    var2drop <- character(length(currentTerms))
    dropTerm <- NULL
        
    t<-1
    for (term in currentTerms){ # loop through all terms
      
      if(length(unlist(grep(term,currentTerms)))>1) next # skip dropping the ter, to avoid excluding main effects when interactions are still in
      print(term)
      model.new[[t]] <- as.formula(paste(model[2],model[1],model[3],"-",term))
      X.matrix <- as.data.frame(model.matrix(model.new[[t]], data=ES.df))
      newModel[[t]] <- run.analysis4select(model.name=paste(model.new[[t]]),X.matrix,ES.df)
      
      var2drop[t] <- term
      deltaDIC[t] <- try(newModel[[t]]$DIC - currentModel$DIC, silent=T)
      if(inherits(deltaDIC[t], "try-error")) return(newModel)
      
      t<-t+1
    }
    
    stats <- data.frame(var2drop,deltaDIC)
    if(length(which(stats$var2drop==""))>0) stats <- stats[-which(stats$var2drop==""),]
    
    stats.list[[i]] <- stats
    print(stats.list[[i]])
    i <- i+1
    
    if(min(stats$deltaDIC)<0){
      dropt <- which.min(stats$deltaDIC)
      dropTerm <- stats$var2drop[dropt]
    }
    if (is.null(dropTerm)) {
      stats.list <- stats.list[lapply(stats.list,length)>0] # remove empty items from stats.list
      return(list(terms=currentTerms,model=currentModel,stats.list=stats.list)) # end model selection if no improvement is achieved
    }
    print(dropTerm)
    
    model <- model.new[[dropt]]
    X.matrix <- as.data.frame(model.matrix(model, data=ES.df))
    currentModel <- newModel[[dropt]]
    currentTerms <- currentTerms[-which(currentTerms==dropTerm)]
    print(currentTerms)
    
  }
  
  if(length(currentTerms)==1){
    X.matrix <- as.data.frame(model.matrix(as.formula(Log.RR~1), data=ES.df))
    newModel <- run.analysis4select(model.name="Intercept",X.matrix,df)
    deltaDIC <- try(newModel$DIC - currentModel$DIC, silent=T)
    if(inherits(deltaDIC[t], "try-error")) return(currentModel)
    if(deltaDIC < 0){
      currentModel <- newModel
    }  
  }
  stats.list <- stats.list[lapply(stats.list,length)>0] # remove empty items from stats.list
  return(list(terms=currentTerms,model=currentModel,stats.list=stats.list))
  
}

reduced.model.richness <- BMASelect(model=as.formula(Log.RR ~ LUI.range.level + Species.Group + Product + BIOME + LUI.range.level:Species.Group + LUI.range.level:Product + LUI.range.level:BIOME),ES.df=ES.frame.richness)

reduced.model.yield <- BMASelect(model=as.formula(Log.RR ~ LUI.range.level + Product + BIOME + LUI.range.level:Species.Group + LUI.range.level:Product + LUI.range.level:BIOME),ES.df=ES.frame.yield)

