############################################################################
### Purpose of this skript module 09.1 is to:
###
### 09.1. plot model parameter estimates
### 09.1.1. plot cross diagrams
### 09.1.2. plot Panel for LUIrangelevel
### 09.1.3. Plot model coeeficients + SE relative to the intercept (cf. Fig1 in Newbold et al. 2015)
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
### 09.1.2. plot LUI cross diagrams
############################################################################
# predict for each covariate combination
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
newdat.GM <- data.frame(LUI.range.level="Grand Mean",
                        logRR.richness= model1$b, logRR.richness.se= model1$se, 
                        logRR.richness.ci.lb=model1$b-1.96*model1$se,logRR.richness.ci.ub=model1$b+1.96*model1$se,
                        n.richness=nrow(modelDataRichness),
                        logRR.yield= model2$b, logRR.yield.se= model2$se, 
                        logRR.yield.ci.lb=model2$b-1.96*model2$se,logRR.yield.ci.ub=model2$b+1.96*model2$se,
                        n.yield=nrow(modelDataYield))

newdat <- join_all(list(newdat,newdat.GM),type="full")
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

write.csv(newdat[,c("LUI.range.level", 
                    "n.richness", "perc.rich.change",  "CI95.perc.rich.change",
                    "n.yield", "perc.yield.change","CI95.perc.yield.change")],
          file=path2temp %+% "preds.LUI.csv",row.names=F)

## calculation of 95%CI in forest() from metafor
#vi <- sei^2
#ci.lb <- yi - qnorm(alpha/2, lower.tail=FALSE) * sei
#ci.ub <- yi + qnorm(alpha/2, lower.tail=FALSE) * sei

### plot rawdata
plot <- ggplot(data=ES.frame) +
  geom_point(aes(x=Yield.Log.RR, y=Richness.Log.RR,color=LUI.range.level),alpha=0.5) +
  geom_pointrange(aes(x=Yield.Log.RR, y=Richness.Log.RR, ymin=Richness.Log.RR - (1.96*sqrt(Richness.Log.RR.Var)), ymax=Richness.Log.RR + (1.96*sqrt(Richness.Log.RR.Var)),color=LUI.range.level),alpha=0.5) +
  geom_segment(aes(x=Yield.Log.RR - (1.96*sqrt(Yield.Log.RR.Var)), xend=Yield.Log.RR + (1.96*sqrt(Yield.Log.RR.Var)), y = Richness.Log.RR, yend = Richness.Log.RR, color=LUI.range.level),alpha=0.5) +
  scale_y_continuous(labels=seqLabels,breaks=seqBreaks) + 
  scale_x_continuous(labels=seqLabels,breaks=seqBreaks) +
  ylab("% Richness difference") + xlab("% Yield difference") + labs(color='') + 
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="darkgrey"),breaks=c(levels(ES.frame$LUI.range.level))) +
  theme_lubdes(legend.position="bottom") +
  guides(color=guide_legend(nrow=3))
ggsave(plot, file = path2temp %+% "rawdata_LUI.png", width = 16, height = 8, type = "cairo-png")


# plot crosses for each covariate combination
seqBreaks <- log(c(0.6,0.8,0.9,1,1.25,1.5))
seqLabels <- 100*(exp(seqBreaks)-1)

plot <- ggplot(data=newdat) + 
  geom_hline(aes(yintercept=0), linetype="twodash",size=1.05) + geom_vline(aes(xintercept=0), linetype="twodash",size=1.05) +
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=LUI.range.level), size=3) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness.ci.lb, 
                                   ymax=logRR.richness.ci.ub,color=LUI.range.level), size=1.2) +
  geom_segment(aes(x=logRR.yield.ci.lb, xend=logRR.yield.ci.ub, y = logRR.richness, yend = logRR.richness, color=LUI.range.level),size=1.2) +
  scale_y_continuous(labels=seqLabels,breaks=seqBreaks) + 
  scale_x_continuous(labels=seqLabels,breaks=seqBreaks) +
  ylab("% Richness difference") + xlab("% Yield difference") + labs(color='') + 
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand Mean"="black"),breaks=levels(newdat$LUI.range.level)) +
  theme_lubdes(legend.position="bottom",rel.text.size=2) +
  guides(color=guide_legend(nrow=3))
ggsave(plot, file = path2temp %+% "CrossPlot_LUI_rma.png", width = 16, height = 8, type = "cairo-png")


############################################################################
### 09.1.2. plot Panel for LUIrangelevel
############################################################################
# one=3; two=5; three=7; alpha=100
# y.offset <- 0.2
# height <- (two-one)/2
# 
# ## define colours per group
# red <- rgb(228,26,28, alpha=alpha, max=255) # for SR
# blue <- rgb(55,126,184, alpha=alpha, max=255) # for Yield
# 
# newdat <- newdat[-which(newdat$level=="Grand mean"),]
# 
# newdat$y[newdat$level == "low-low"] <- one
# newdat$y[newdat$level == "medium-medium"] <- one
# newdat$y[newdat$level == "high-high"] <- one
# newdat$y[newdat$level == "low-medium"] <- two
# newdat$y[newdat$level == "medium-high"] <- two
# newdat$y[newdat$level == "low-high"] <- three
# 
# newdat$x1[newdat$level == "low-low"] <- 1
# newdat$x1[newdat$level == "medium-medium"] <- 3
# newdat$x1[newdat$level == "high-high"] <- 5
# newdat$x1[newdat$level == "low-medium"] <- 2
# newdat$x1[newdat$level == "medium-high"] <- 4
# newdat$x1[newdat$level == "low-high"] <- 3
# 
# range <- max(abs(range(newdat$logRR.richness+1.96*newdat$logRR.richness.se, newdat$logRR.yield+1.96*newdat$logRR.yield.se)))
# newdat[,c("logRR.richness", "logRR.richness.se", "logRR.yield", "logRR.yield.se")] <- newdat[,c("logRR.richness", "logRR.richness.se", "logRR.yield", "logRR.yield.se")]/range
# 
# ## forest plot
# png(path2temp %+% "ForestPlot_LUI_Panel_rma.png")
# # panel set up
# par(oma = c(3, 0, 0, 0),xpd=T)
# plot(newdat$x1, newdat$y, pch=19, ylim =c(1+0.2, three-0.2), xlim=c(0, 6), ylab="", xlab="", xaxt="n", yaxt="n", col="white")
# legend("bottom", inset=c(0,-0.5),legend=c("Richness","Yield"),title="Log-Response Ratios",col=c(red,blue),pch=16, horiz =T, bty="n")
# segments(x0=0, x1=6,y0=one,lty=3)
# segments(x0=0, x1=6,y0=two,lty=3)
# segments(x0=1, y0=1, y1 = one,lty=3)
# segments(x0=3, y0=1, y1 = one,lty=3)
# segments(x0=5, y0=1, y1 = one,lty=3)
# segments(x0=2, y0=one, y1 = two,lty=3)
# segments(x0=4, y0=one, y1 = two,lty=3)
# segments(x0=3, y0=two, y1 = three,lty=3)
# axis(1, at=c(1, 3, 5),pos=1.2, labels=c("Low", "Medium", "High"), tick = FALSE)
# 
# par(cex=1.2)
# # plot means as points
# points(newdat$x1+newdat$logRR.richness,newdat$y-height+y.offset,col=red,pch=16)
# points(newdat$x1+newdat$logRR.yield,newdat$y-height-y.offset,col=blue,pch=16)
# 
# # plot 95%CI
# for(i in newdat$level){
#   temp <- subset(newdat,level==i)
#   segments(x0=temp$x1+temp$logRR.richness-1.96*temp$logRR.richness.se,x1=temp$x1+temp$logRR.richness+1.96*temp$logRR.richness.se,y0=temp$y-height+y.offset,col=red,lwd=2)
#   segments(x0=temp$x1+temp$logRR.yield-1.96*temp$logRR.yield.se,x1=temp$x1+temp$logRR.yield+1.96*temp$logRR.yield.se,y0=temp$y-height-y.offset,col=blue,lwd=2)  
# }
# dev.off()
# 
# par(par(no.readonly = T))

############################################################################
### 09.1.3. Plot model coeeficients + SE relative to the intercept (cf. Fig1 in Newbold et al. 2015)
############################################################################
# model <- Richness.MA.model[["Select"]]
# plot <- ggplot() +
#   geom_point(aes(x=rownames(model$b), y=model$b), size=4) +
#   geom_pointrange(aes(x=rownames(model$b), y=model$b, ymin=model$b - (1.96*model$se), ymax=model$b + (1.96*model$se)), size=1.5) +
#   geom_hline(aes(yintercept=0), linetype="twodash") +
#   scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#   scale_x_discrete(limits=rownames(model$b)) +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ylab("RR (Species Richness)") + xlab("")
# print(plot)
# ggsave(plot, file = path2temp %+% "CatPlotSelect_richness_rma.png", width = 20, height = 8, type = "cairo-png")
# 
# model <- Yield.MA.model[["Select"]]
# plot <- ggplot() +
#   geom_point(aes(x=rownames(model$b), y=model$b), size=4) +
#   geom_pointrange(aes(x=rownames(model$b), y=model$b, ymin=model$b - (1.96*model$se), ymax=model$b + (1.96*model$se)), size=1.5) +
#   geom_hline(aes(yintercept=0), linetype="twodash") +
#   scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#   scale_x_discrete(limits=rownames(model$b)) +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   ylab("RR (Yield)") + xlab("")
# print(plot)
# ggsave(plot, file = path2temp %+% "CatPlotSelect_yield_rma.png", width = 20, height = 8, type = "cairo-png")
# 
