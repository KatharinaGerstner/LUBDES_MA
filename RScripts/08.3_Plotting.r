############################################################################
### 08.3. forest plots for each LUI-comparison class
### 
### Stefan Kambach, Annapolis Workshop
### Ralf Seppelt: generated seperate r-file
############################################################################


LUI.range.level = c("low-low","medium-medium","high-high","low-medium","medium-high","low-high")


LUI.level.to.plot = LUI.range.level[1]

for(LUI.level.to.plot in LUI.range.level){
  data.to.plot = subset(ES.frame, LUI.range.level %in% LUI.level.to.plot)
  data.to.plot = melt(data.to.plot[,c("Study.ID","Case.ID","LUI.range.level","Richness.Log.RR","Richness.Log.RR.Var","Yield.Log.RR","Yield.Log.RR.Var",
                                      "Yield.SD.is.imputed.low","Yield.SD.is.imputed.high", "Richness.SD.is.imputed.low","Richness.SD.is.imputed.high")],
                      id.vars=c("Study.ID","Case.ID","LUI.range.level","Richness.Log.RR.Var","Yield.Log.RR.Var","Yield.SD.is.imputed.low","Yield.SD.is.imputed.high", "Richness.SD.is.imputed.low","Richness.SD.is.imputed.high"),
                      measure.vars=c("Richness.Log.RR", "Yield.Log.RR"))
  data.to.plot = data.to.plot[order(data.to.plot$value),]
  data.to.plot$Richness.Log.RR.Var[which(data.to.plot$variable %in% "Yield.Log.RR")] =   data.to.plot$Yield.Log.RR.Var[which(data.to.plot$variable %in% "Yield.Log.RR")] 
  data.to.plot = data.to.plot[,-(which(names(data.to.plot) %in% "Yield.Log.RR.Var"))]
  names(data.to.plot)[which(names(data.to.plot) %in% "Richness.Log.RR.Var")] = "Log.RR.Var"
  names(data.to.plot)[which(names(data.to.plot) %in% "variable")] = "RR.ID"
  names(data.to.plot)[which(names(data.to.plot) %in% "value")] = "RR.value"
  
  data.to.plot$uniqueID = paste(as.character(data.to.plot$Study.ID),as.character((data.to.plot$Case.ID)))
  data.to.plot$uniqueID = paste(as.character(data.to.plot$uniqueID),as.character((data.to.plot$RR.ID)))
  
  # sort the table so that Yield and Richness of the same study are together
  data.to.plot.ordered = subset(data.to.plot, RR.ID %in% "Yield.Log.RR")
  data.to.fill.with.ordered = data.to.plot.ordered[0,]
  for(sort.run in 1:nrow(data.to.plot.ordered)){
    data.to.fill.with.ordered = rbind(data.to.fill.with.ordered,data.to.plot.ordered[sort.run,])
    data.to.fill.with.ordered = rbind(data.to.fill.with.ordered,
                                      data.to.plot[which(data.to.plot$Study.ID %in% data.to.plot.ordered$Study.ID[sort.run] &
                                                           data.to.plot$Case.ID %in% data.to.plot.ordered$Case.ID[sort.run] &  
                                                           data.to.plot$RR.ID %in% "Richness.Log.RR"),])
    
  }
  data.to.plot = data.to.fill.with.ordered
  data.to.plot$uniqueID = factor(data.to.plot$uniqueID,levels= rev(data.to.plot$uniqueID))
  
  
  #add a column with the study name and case id + remove every second to make axis better readable
  #data.to.plot$axes.naming = paste(data.to.plot$Study.ID,data.to.plot$Case.ID)
  data.to.plot$axes.naming = as.character(data.to.plot$uniqueID)
  data.to.plot$axes.naming[which(seq(1:nrow(data.to.plot)) %% 2 == 0)] = " "
  
  #rename Richness.Log.RR and Yield.Log.RR
  data.to.plot$RR.ID = as.character(data.to.plot$RR.ID)
  data.to.plot$RR.ID[which(data.to.plot$RR.ID %in% "Yield.Log.RR")] = "Yield"
  data.to.plot$RR.ID[which(data.to.plot$RR.ID %in% "Richness.Log.RR")] = "Species Richness"
  data.to.plot$RR.ID = factor(data.to.plot$RR.ID, levels =c("Yield", "Species Richness"))
  
  
  # get axes length to center the plot 
  max.values = sqrt(max(data.to.plot$RR.value^2,na.rm=TRUE)) + 0.5
  
  # create a data is imputed column
  data.to.plot$is.SD.imputed = "no"
  data.to.plot$is.SD.imputed[which(data.to.plot$Yield.SD.is.imputed.low %in% "yes" & data.to.plot$RR.ID %in% "Yield")] = "yes"
  data.to.plot$is.SD.imputed[which(data.to.plot$Yield.SD.is.imputed.high %in% "yes" & data.to.plot$RR.ID %in% "Yield")] = "yes"
  data.to.plot$is.SD.imputed[which(data.to.plot$Richness.SD.is.imputed.low %in% "yes"& data.to.plot$RR.ID %in% "Species Richness")] = "yes"
  data.to.plot$is.SD.imputed[which(data.to.plot$Richness.SD.is.imputed.high %in% "yes" & data.to.plot$RR.ID %in% "Species Richness")] = "yes"
  
  #find exact replicated that come from a single imputation
  data.to.plot$is.duplicate = duplicated(data.to.plot[,c("Study.ID","RR.value","Log.RR.Var","RR.ID")])
  data.to.plot$is.duplicate[which(data.to.plot$is.duplicate)] = "duplicate"
  data.to.plot$is.duplicate[which(data.to.plot$is.duplicate %in% "FALSE")] = "value"
  data.to.plot$is.duplicate = factor(data.to.plot$is.duplicate,levels=c("value","duplicate"))
  
  
  #### if ervery runs smooth - cut away this plot  
  #cut away the information of Yield.Log.RR from the unique Study ID to have them not displayed in the graph
  #  data.to.plot$uniqueID.for.axis.lables = seq(nrow(data.to.plot):1)
  #  data.to.plot$uniqueID.for.axis.lables = paste(paste(paste(data.to.plot$uniqueID.for.axis.lables,". ",sep=""),data.to.plot$Study.ID,sep=""),data.to.plot$Case.ID,sep="")
  #  data.to.plot$uniqueID.for.axis.lables = factor(data.to.plot$uniqueID.for.axis.lables,levels=(data.to.plot$uniqueID.for.axis.lables))
  #####
  
  data.to.plot$uniqueID = factor(data.to.plot$uniqueID,levels=rev(data.to.plot$uniqueID))
  data.to.plot$axes.naming = gsub(" Yield.Log.RR","",data.to.plot$axes.naming)
  data.to.plot$colouring = paste(data.to.plot$RR.ID,data.to.plot$is.duplicate)
  data.to.plot$colouring = factor(data.to.plot$colouring,levels = c("Yield value","Yield duplicate","Species Richness value","Species Richness duplicate"))
  
  
  plot.forest =
    ggplot(data=data.to.plot) +
    
    geom_linerange(aes(x=uniqueID,ymin=RR.value	- (1.96*Log.RR.Var), ymax=RR.value	+ (1.96*Log.RR.Var),colour=colouring,alpha=is.SD.imputed,linetype=is.SD.imputed),size=1.5) +
    geom_point(aes(x=uniqueID, y=RR.value,colour=colouring),size=5) +
    geom_hline(x=0,linetype ="twodash")  +
    
    #scale manually to get the legend correct
    scale_colour_manual(values=c("#FF6633","grey","#00CC00","grey")) +
    scale_x_discrete("Study ID",breaks= as.character(data.to.plot$uniqueID),labels=data.to.plot$axes.naming)  +
    scale_alpha_discrete(range = c(1,0.4)) + 
    
    #white background + flip 90 degrees
    theme(axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black"),
          axis.text.y = element_text(vjust=1),legend.key.width=unit(3,"line")) +
    coord_flip(ylim=c(0 - max.values,max.values)) +
    geom_vline(xintercept=seq(from=0.5, to=nrow(data.to.plot)-0.5,by=2),colour="grey") +
    
    #axes labels
    xlab("Study ID") +
    ylab("Log Response Ration")+
    ggtitle(paste("Forest Plot of study case effect sizes\n- ",LUI.level.to.plot)) 
  
  print(plot.forest)
  ggsave(plot.forest, file = paste(c(path2temp, "Forest_plot_",LUI.level.to.plot,".png"), collapse=""), width = 15, height = nrow(data.to.plot) / 5, type = "cairo-png")
  
}

