### Model selection

run.analysis <- function(model.name,X.matrix,df){
  
  print("Prepare the data")
  dat2fit.richness <- list(
    Log.RR=ES.frame.richness$Log.RR, 
    Log.RR.Var=ES.frame.richness$Log.RR.Var,
    N.obs = nrow(ES.frame.richness),
    Study.Case = ES.frame.richness$Study.Case,
    N.Case = nlevels(ES.frame.richness$Study.Case), # number of cases within studies
    A = M.matrix(ES.frame.richness),
    X = X.matrix,
    N.colX = ncol(X.matrix))
  
  print(str(dat2fit))
  
  model.fit <- jags.model(data=dat2fit, file=path2temp %+% "bayesianMA_3.txt", 
                          n.chains = 3, n.adapt=1000) # n.adapt for sampling the parameter space and conclude on one value
  update(model.fit, n.iter=1000) # start from this value
  dic.samps <- dic.samples(model.fit, n.iter=10000,thin=4)
  DIC <- round(sum(dic.samps[["deviance"]]) + sum(dic.samps[["penalty"]]),digits=3) # model deviance information criterion "deviance"=mean deviance, "penalty" = 2*pD
  return(list(model.fit=model.fit,DIC=DIC))
}


BMASelect <- function(model, df){
  
  allTerms <- trim(strsplit(paste(model),'[+]')[[3]])
  
  X.matrix <- as.data.frame(model.matrix(model, data=df))
  currentModel <- run.analysis(model.name="FullModel",X.matrix,df)
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
      X.matrix <- as.data.frame(model.matrix(model.new[[t]], data=df))
      newModel[[t]] <- run.analysis(model.name=paste(model.new[[t]]),X.matrix,df)
      
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
    if (is.null(dropTerm)) return(list(model=currentTerms,stats.list=stats.list)) # end model selection if no improvement is achieved
    print(dropTerm)
    
    model <- model.new[[dropt]]
    X.matrix <- as.data.frame(model.matrix(model, data=df))
    currentModel <- newModel[[dropt]]
    currentTerms <- currentTerms[-which(currentTerms==dropTerm)]
    print(currentTerms)
    
  }
  
  if(length(currentTerms)==1){
    X.matrix <- as.data.frame(model.matrix(as.formula(Log.RR~1), data=df))
    newModel <- run.analysis(model.name="Intercept",X.matrix,df)
    deltaDIC <- try(newModel$DIC - currentModel$DIC, silent=T)
    if(inherits(deltaDIC[t], "try-error")) return(currentModel)
    if(deltaDIC < 0){
      currentModel <- newModel
    }  
  }
  stats.list <- stats.list[lapply(stats.list,length)>0] # remove empty items from stats.list
  return(list(terms=currentTerms,model=currentModel,stats.list=stats.list))
  
}

# full model
full.model <- as.formula(Log.RR ~ LUI.range.level + Species.Group + Product + BIOME + LUI.range.level:Species.Group + LUI.range.level:Product + LUI.range.level:BIOME)
reduced.model <- BMASelect(model=full.model,df=ES.frame.richness)



# X.matrix <- as.data.frame(model.matrix(full.model, data=ES.frame.richness))
# MA[1] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
# 
# # first round, remove one of the interaction terms
# MA <- list()
# reduced.model <- list()
# var.dropped <- c("LUI.range.level:Species.Group", "LUI.range.level:Product", "LUI.range.level:BIOME")
# for(i in 1:length(var.dropped)){
#   reduced.model[[i]] <- as.formula(paste("Log.RR ~ LUI.range.level + Species.Group + Product + BIOME + LUI.range.level:Species.Group + LUI.range.level:Product + LUI.range.level:BIOME - 1 -", var.dropped[i]))
#   X.matrix <- as.data.frame(model.matrix(reduced.model[[i]], data=ES.frame.richness))
#   long.names <- colnames(X.matrix)
#   MA[[i+1]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
# }
# 
# results <- list()
# results[[1]] <- data.frame(c("-" %+% var.dropped),DIC=unlist(MA))
# 
# # second round, remove one of the remaining interaction terms or the single effect without interaction
# var2remove <- which.min(results[[1]]$DIC)
# var.dropped <- c(var.dropped,strsplit(var.dropped[var2remove],":")[[1]][2])
# var.dropped <- var.dropped[-var2remove]
# 
# for(i in 1:length(var.dropped)){
#   reduced.model[[3+i]] <- as.formula(paste(reduced.model[[var2remove]][2],reduced.model[[var2remove]][1],reduced.model[[var2remove]][3], "-", var.dropped[i]))
#   X.matrix <- as.data.frame(model.matrix(reduced.model[[3+i]], data=ES.frame.richness))
#   long.names <- colnames(X.matrix)
#   MA[[3+1+i]] <- run.analysis(model.name,X.matrix,ES.frame.richness,long.names)
# }
# 
# results[[2]] <- data.frame(c(full.model,reduced.model),DIC=MA)
