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

############################################################################
### 09.1.1. plot cross diagrams
############################################################################
# predict for each covariate combination
newdat <- expand.grid(LUI.range.level=levels(ES.frame$LUI.range.level))
newdat$level <- newdat$LUI.range.level
#newdat$level <- factor(newdat$level, levels = rev(levels(newdat$level)))

model <- Richness.MA.model[["LUI"]]
mm <- model.matrix(~LUI.range.level-1, data=newdat)
preds <- predict.rma(model, newmods = mm)
newdat$logRR.richness <- preds$pred
newdat$logRR.richness.se <- preds$se

model <- Yield.MA.model[["LUI"]]
mm <- model.matrix(~LUI.range.level-1, data=newdat)
preds <- predict.rma(model, newmods = mm)
newdat$logRR.yield <- preds$pred
newdat$logRR.yield.se <- preds$se

model1 <- Richness.MA.model[["None"]]
model2 <- Yield.MA.model[["None"]]
newdat[7,] <- data.frame(LUI.range.level=NA, level=NA,
                         logRR.richness= model1$b, logRR.richness.se= model1$se,
                         logRR.yield= model2$b, logRR.yield.se= model2$se)
levels(newdat$level) <- c(levels(newdat$level),"Grand mean")
newdat$level[7] <- "Grand mean"

modelData <- subset(ES.frame, Richness.Log.RR.Var>0 & Yield.Log.RR.Var>0)
## calculation of 95%CI in forest() from metafor
#vi <- sei^2
#ci.lb <- yi - qnorm(alpha/2, lower.tail=FALSE) * sei
#ci.ub <- yi + qnorm(alpha/2, lower.tail=FALSE) * sei

### plot rawdata
plot <- ggplot() +
  geom_point(aes(x=modelData$Yield.Log.RR, y=modelData$Richness.Log.RR,color=modelData$LUI.range.level),alpha=0.5) +
  geom_pointrange(aes(x=modelData$Yield.Log.RR, y=modelData$Richness.Log.RR, ymin=modelData$Richness.Log.RR - (1.96*sqrt(modelData$Richness.Log.RR)), ymax=modelData$Richness.Log.RR + (1.96*sqrt(modelData$Richness.Log.RR)),color=modelData$LUI.range.level),alpha=0.5) +
  geom_segment(aes(x=modelData$Yield.Log.RR - (1.96*sqrt(modelData$Yield.Log.RR.Var)), xend=modelData$Yield.Log.RR + (1.96*sqrt(modelData$Yield.Log.RR.Var)), y = modelData$Richness.Log.RR, yend = modelData$Richness.Log.RR, color=modelData$LUI.range.level),alpha=0.5) +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand mean"="darkgrey"),breaks=c(levels(modelData$LUI.range.level),"Grand mean")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) 
print(plot)
ggsave(plot, file = path2temp %+% "rawdata_LUI.png", width = 20, height = 8, type = "cairo-png")


# plot crosses for each covariate combination
plot <- ggplot() + 
  geom_hline(aes(yintercept=0), linetype="twodash") + geom_vline(aes(xintercept=0), linetype="twodash") +
  geom_point(aes(x=newdat$logRR.yield, y=newdat$logRR.richness, color=newdat$level), size=4) +
  geom_pointrange(aes(x=newdat$logRR.yield, y=newdat$logRR.richness, ymin=newdat$logRR.richness - (1.96*newdat$logRR.richness.se), 
                                   ymax=newdat$logRR.richness + (1.96*newdat$logRR.richness.se),color=newdat$level), size=1.5) +
  geom_segment(aes(x=newdat$logRR.yield - (1.96*newdat$logRR.yield.se), xend=newdat$logRR.yield + (1.96*newdat$logRR.yield.se), y = newdat$logRR.richness, yend = newdat$logRR.richness, color=newdat$level),size=1.5) +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_manual(values=c("low-low"='#d0d1e6',"medium-medium"="#a6bddb","high-high"="#045a8d","low-medium"='#fee090',"medium-high"='#fc8d59',"low-high"="#d73027","Grand mean"="darkgrey"),breaks=c(levels(modelData$LUI.range.level),"Grand mean")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) 
print(plot)
ggsave(plot, file = path2temp %+% "CrossPlot_LUI_rma.png", width = 20, height = 8, type = "cairo-png")

############################################################################
### 09.1.2. plot Panel for LUIrangelevel
############################################################################
one=3; two=5; three=7; alpha=100
y.offset <- 0.2
height <- (two-one)/2

## define colours per group
red <- rgb(228,26,28, alpha=alpha, max=255) # for SR
blue <- rgb(55,126,184, alpha=alpha, max=255) # for Yield

newdat <- newdat[-which(newdat$level=="Grand mean"),]

newdat$y[newdat$level == "low-low"] <- one
newdat$y[newdat$level == "medium-medium"] <- one
newdat$y[newdat$level == "high-high"] <- one
newdat$y[newdat$level == "low-medium"] <- two
newdat$y[newdat$level == "medium-high"] <- two
newdat$y[newdat$level == "low-high"] <- three

newdat$x1[newdat$level == "low-low"] <- 1
newdat$x1[newdat$level == "medium-medium"] <- 3
newdat$x1[newdat$level == "high-high"] <- 5
newdat$x1[newdat$level == "low-medium"] <- 2
newdat$x1[newdat$level == "medium-high"] <- 4
newdat$x1[newdat$level == "low-high"] <- 3

range <- max(abs(range(newdat$logRR.richness+1.96*newdat$logRR.richness.se, newdat$logRR.yield+1.96*newdat$logRR.yield.se)))
newdat[,c("logRR.richness", "logRR.richness.se", "logRR.yield", "logRR.yield.se")] <- newdat[,c("logRR.richness", "logRR.richness.se", "logRR.yield", "logRR.yield.se")]/range

## forest plot
png(path2temp %+% "ForestPlot_LUI_Panel_rma.png")
# panel set up
par(oma = c(3, 0, 0, 0),xpd=T)
plot(newdat$x1, newdat$y, pch=19, ylim =c(1+0.2, three-0.2), xlim=c(0, 6), ylab="", xlab="", xaxt="n", yaxt="n", col="white")
legend("bottom", inset=c(0,-0.5),legend=c("Richness","Yield"),title="Log-Response Ratios",col=c(red,blue),pch=16, horiz =T, bty="n")
segments(x0=0, x1=6,y0=one,lty=3)
segments(x0=0, x1=6,y0=two,lty=3)
segments(x0=1, y0=1, y1 = one,lty=3)
segments(x0=3, y0=1, y1 = one,lty=3)
segments(x0=5, y0=1, y1 = one,lty=3)
segments(x0=2, y0=one, y1 = two,lty=3)
segments(x0=4, y0=one, y1 = two,lty=3)
segments(x0=3, y0=two, y1 = three,lty=3)
axis(1, at=c(1, 3, 5),pos=1.2, labels=c("Low", "Medium", "High"), tick = FALSE)

par(cex=1.2)
# plot means as points
points(newdat$x1+newdat$logRR.richness,newdat$y-height+y.offset,col=red,pch=16)
points(newdat$x1+newdat$logRR.yield,newdat$y-height-y.offset,col=blue,pch=16)

# plot 95%CI
for(i in newdat$level){
  temp <- subset(newdat,level==i)
  segments(x0=temp$x1+temp$logRR.richness-1.96*temp$logRR.richness.se,x1=temp$x1+temp$logRR.richness+1.96*temp$logRR.richness.se,y0=temp$y-height+y.offset,col=red,lwd=2)
  segments(x0=temp$x1+temp$logRR.yield-1.96*temp$logRR.yield.se,x1=temp$x1+temp$logRR.yield+1.96*temp$logRR.yield.se,y0=temp$y-height-y.offset,col=blue,lwd=2)  
}
dev.off()

par(par(no.readonly = T))

############################################################################
### 09.1.3. Plot model coeeficients + SE relative to the intercept (cf. Fig1 in Newbold et al. 2015)
############################################################################
model <- Richness.MA.model[["Select"]]
plot <- ggplot() +
  geom_point(aes(x=rownames(model$b), y=model$b), size=4) +
  geom_pointrange(aes(x=rownames(model$b), y=model$b, ymin=model$b - (1.96*model$se), ymax=model$b + (1.96*model$se)), size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_discrete(limits=rownames(model$b)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("RR (Species Richness)") + xlab("")
print(plot)
ggsave(plot, file = path2temp %+% "CatPlotSelect_richness_rma.png", width = 20, height = 8, type = "cairo-png")

model <- Yield.MA.model[["Select"]]
plot <- ggplot() +
  geom_point(aes(x=rownames(model$b), y=model$b), size=4) +
  geom_pointrange(aes(x=rownames(model$b), y=model$b, ymin=model$b - (1.96*model$se), ymax=model$b + (1.96*model$se)), size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_discrete(limits=rownames(model$b)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("RR (Yield)") + xlab("")
print(plot)
ggsave(plot, file = path2temp %+% "CatPlotSelect_yield_rma.png", width = 20, height = 8, type = "cairo-png")

