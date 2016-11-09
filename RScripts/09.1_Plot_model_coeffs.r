############################################################################
### Purpose of this skript module 09.1 is to:
###
### 09.1.1. plot raw data + grand mean
### 09.1.2. plot LUI forest plots
###
### Authors: KG, TN,...
############################################################################

seqBreaks <- log(sapply(-2:7,function(x) 2^x))
seqLabels <- 100*(exp(seqBreaks)-1) # labelling with percentage change

############################################################################
### 09.1.1. plot raw data + grand mean
############################################################################
newdat <- data.frame(1)

model <- Richness.MA.model[["None"]]
newdat$logRR.richness <- model$b
newdat$logRR.richness.se <- model$se

model <- Yield.MA.model[["None"]]
newdat$logRR.yield <- model$b
newdat$logRR.yield.se <- model$se

plot <- ggplot() +
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR),color="grey",alpha=0.5,size=2) +
  geom_point(data=newdat, aes(x=logRR.yield, y=logRR.richness), color="black", size=1.1) +
  geom_pointrange(data=newdat, aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se),
                      ymax=logRR.richness + (1.96*logRR.richness.se)),color="black", size=1.2) +
  geom_segment(data=newdat, aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness), color="black",size=1.2) +
  scale_y_continuous(labels=seqLabels,breaks=seqBreaks) +
  scale_x_continuous(labels=seqLabels,breaks=seqBreaks) +
  ylab("% Richness difference") + xlab("% Yield difference") + labs(color='') +
  theme_lubdes()
ggsave(plot, file = path2temp %+% "Rawdata+GrandMean_rma.png", width = 16, height = 8, type = "cairo-png")

############################################################################
### 09.1.2. plot LUI forest plots
############################################################################
### predict for each covariate combination
newdat <- expand.grid(LUI.range.level=levels(ES.frame$LUI.range.level))
#newdat$level <- factor(newdat$level, levels = rev(levels(newdat$level)))

model <- Richness.MA.model[["LUI"]]
mm <- model.matrix(~LUI.range.level-1, data=newdat)
preds <- predict.rma(model, newmods = mm)
newdat[,c("logRR.richness","logRR.richness.se","logRR.richness.ci.lb","logRR.richness.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)

newdat$n.richness <- as.numeric(table(modelDataRichness$LUI.range.level))

model <- Yield.MA.model[["LUI"]]
mm <- model.matrix(~LUI.range.level-1, data=newdat)
preds <- predict.rma(model, newmods = mm)
newdat[,c("logRR.yield","logRR.yield.se","logRR.yield.ci.lb","logRR.yield.ci.ub")] <- cbind(preds$pred,preds$se,preds$ci.lb,preds$ci.ub)
newdat$n.yield <- as.numeric(table(modelDataYield$LUI.range.level))

model1 <- Richness.MA.model[["None"]]
model2 <- Yield.MA.model[["None"]]
newdat.GM <- data.frame(LUI.range.level="Grand mean",
                        logRR.richness= model1$b, logRR.richness.se= model1$se, 
                        logRR.richness.ci.lb=model1$b-1.96*model1$se,logRR.richness.ci.ub=model1$b+1.96*model1$se,
                        n.richness=nrow(modelDataRichness),
                        logRR.yield= model2$b, logRR.yield.se= model2$se, 
                        logRR.yield.ci.lb=model2$b-1.96*model2$se,logRR.yield.ci.ub=model2$b+1.96*model2$se,
                        n.yield=nrow(modelDataYield))

newdat <- join_all(list(newdat.GM,newdat),type="full")
newdat$CI95.richness <- "[" %+% round(newdat$logRR.richness.ci.lb,digits=2) %+% "," %+%  round(newdat$logRR.richness.ci.ub,digits=2) %+% "]"
newdat$CI95.yield <- "[" %+% round(newdat$logRR.yield.ci.lb,digits=2) %+% "," %+%  round(newdat$logRR.yield.ci.ub,digits=2) %+% "]" 

newdat$perc.rich.change <- 100*(exp(newdat$logRR.richness)-1)
newdat$perc.rich.change.ci.lb <- 100*(exp(newdat$logRR.richness.ci.lb)-1)
newdat$perc.rich.change.ci.ub <- 100*(exp(newdat$logRR.richness.ci.ub)-1)
newdat$CI95.perc.rich.change <- "[" %+% round(newdat$perc.rich.change.ci.lb,digits=2) %+% "," %+%  round(newdat$perc.rich.change.ci.ub,digits=2) %+% "]"

newdat$perc.yield.change <- 100*(exp(newdat$logRR.yield)-1)
newdat$perc.yield.change.ci.lb <- 100*(exp(newdat$logRR.yield.ci.lb)-1)
newdat$perc.yield.change.ci.ub <- 100*(exp(newdat$logRR.yield.ci.ub)-1)
newdat$CI95.perc.yield.change <- "[" %+% round(newdat$perc.yield.change.ci.lb,digits=2) %+% "," %+%  round(newdat$perc.yield.change.ci.ub,digits=2) %+% "]"

newdat <- rename.factor.levels(newdat)
write.csv(newdat[,c("LUI.range.level", 
                    "n.richness", "perc.rich.change",  "CI95.perc.rich.change",
                    "n.yield", "perc.yield.change","CI95.perc.yield.change")],
          file=path2temp %+% "preds.LUI.csv",row.names=F)

## calculation of 95%CI in forest() from metafor
#vi <- sei^2
#ci.lb <- yi - qnorm(alpha/2, lower.tail=FALSE) * sei
#ci.ub <- yi + qnorm(alpha/2, lower.tail=FALSE) * sei

seqBreaks <- log(c(0.6,0.8,0.9,1,1.25,1.5,1.75,2))
seqLabels <- 100*(exp(seqBreaks)-1)

plot.richness <- ggplot(data=newdat) + 
  geom_vline(aes(xintercept=0), linetype="twodash",size=0.6) +
  geom_errorbarh(aes(x=perc.rich.change, y=LUI.range.level, 
                     xmin=perc.rich.change.ci.lb,xmax=perc.rich.change.ci.ub, color=LUI.range.level),size=1.5,height=0.3) +
  geom_point(aes(x=perc.rich.change,y=LUI.range.level, color=LUI.range.level),size=6) +
  scale_y_discrete("",limits=rev(newdat$LUI.range.level)) +#c("7"="Grand Mean","6"="low-low","5"="medium-medium","4"="high-high","5"="low-medium","6"="medium-high","7"="low-high")) + 
  scale_x_continuous("% Richness difference",breaks=seqLabels,limits=c(-50,25)) +
  scale_color_manual(name="",values=c("Low-low"='#2b83ba',"Medium-medium"='#008837',"High-high"='#abdda4',"Low-medium"='#fdae61',"Medium-high"='#d7191c',"Low-high"='#7b3294',"Grand mean"="black"),labels=levels(newdat$LUI.range.level)) +
  theme_lubdes(rel.text.size=1.5) +
  guides(color="none",fill="none")

plot.yield <- ggplot(data=newdat) + 
  geom_vline(aes(xintercept=0), linetype="twodash",size=0.6) +
  geom_errorbarh(aes(x=perc.yield.change, y=LUI.range.level, 
                     xmin=perc.yield.change.ci.lb,xmax=perc.yield.change.ci.ub, color=LUI.range.level),size=1.5,height=0.3) +
  geom_point(aes(x=perc.yield.change,y=LUI.range.level, color=LUI.range.level),size=6) +
  scale_y_discrete("",limits=rev(newdat$LUI.range.level),labels=NULL) +
  scale_x_continuous("% Yield difference",breaks=seqLabels,limits=c(-20,110)) +
  scale_color_manual(name="",values=c("Low-low"='#2b83ba',"Medium-medium"='#008837',"High-high"='#abdda4',"Low-medium"='#fdae61',"Medium-high"='#d7191c',"Low-high"='#7b3294',"Grand mean"="black"),labels=levels(newdat$LUI.range.level)) +
  theme_lubdes(rel.text.size=1.5) +
  guides(color="none",fill="none")
png(file = path2temp %+% "LUI_rma.png", width = 800, height = 300)
grid.arrange(plot.richness,plot.yield,ncol=2,nrow=1,widths=c(1,1))
dev.off()
