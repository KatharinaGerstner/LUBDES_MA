
RMASelect <- function(model){
  
  allTerms <- trim(strsplit(paste(model$call$mods)[2],'[+]')[[1]])
  
  stats <- data.frame(terms=allTerms,df=NA,LR=NA,P=NA)
  
  currentModel <- model
  currentTerms <- allTerms
  
  while(TRUE){
    LRTs <- numeric(length(currentTerms))
    dfs <- character(length(currentTerms))
    Ps <- numeric(length(currentTerms))
    
    t<-1
    for (term in currentTerms){
      
      newModel<-update(currentModel,paste("~.-",term,sep=""))
      
      an <- anova(currentModel,newModel)
      
      LRTs[t]<-an$LRT
      dfs[t]<-paste(an$p.r,an$p.f)
      Ps[t]<-an$pval
      
      t<-t+1
    }
    
    if (all(Ps<0.05)) break
    
    dropTermPos <- which(LRTs==min(LRTs))
    dropTerm <- currentTerms[dropTermPos]
    
    stats$LR[which(stats$terms==dropTerm)]<-LRTs[which(currentTerms==dropTerm)]
    stats$df[which(stats$terms==dropTerm)]<-dfs[which(currentTerms==dropTerm)]
    stats$P[which(stats$terms==dropTerm)]<-Ps[which(currentTerms==dropTerm)]
    
    currentModel <- update(currentModel,paste("~.-",dropTerm,sep=""))
    currentTerms <- currentTerms[-dropTermPos]
    
  }
  
  stats$LR[is.na(stats$LR)] <- LRTs[match(stats$terms[is.na(stats$LR)],currentTerms)]
  stats$df[is.na(stats$df)] <- dfs[match(stats$terms[is.na(stats$df)],currentTerms)]
  stats$P[is.na(stats$P)] <- Ps[match(stats$terms[is.na(stats$P)],currentTerms)]
  
  return(stats)
  
}
