############################################################################
### Purpose of this skript module 08.4 is to:
###
### 08.4.1. plot cross diagrams
###
###
### Authors: KG, ...
############################################################################

############################################################################
### 08.4.1. plot cross diagrams
############################################################################
# predict for each covariate combination
beta <- data.frame(level=character(6),richness.mean=numeric(6),richness.lb=numeric(6),richness.ub=numeric(6),yield.mean=numeric(6),yield.lb=numeric(6),yield.ub=numeric(6))
beta$level <- levels(ES.frame.richness$LUI.range.level)

# list(model.name,model.fit,samps)
samps <- Richness.MA.model[["LUI"]]$samps
beta$richness.mean <- summary(samps[["beta"]])$statistics[,"Mean"]
beta$richness.lb <- summary(samps[["beta"]])$quantiles[,"2.5%"]
beta$richness.ub <- summary(samps[["beta"]])$quantiles[,"97.5%"]    

samps <- Yield.MA.model[["LUI"]]$samps
beta$yield.mean <- summary(samps[["beta"]])$statistics[,"Mean"]
beta$yield.lb <- summary(samps[["beta"]])$quantiles[,"2.5%"]
beta$yield.ub <- summary(samps[["beta"]])$quantiles[,"97.5%"]    

samps1 <- Richness.MA.model[["None"]]$samps
samps2 <- Yield.MA.model[["None"]]$samps
beta[7,] <- data.frame(level ="GrandMean",
                         richness.mean = summary(samps1[["beta"]])$statistics["Mean"],
                         richness.lb = summary(samps1[["beta"]])$quantiles["2.5%"],
                         richness.ub = summary(samps1[["beta"]])$quantiles["97.5%"],
                         yield.mean = summary(samps2[["beta"]])$statistics["Mean"],
                         yield.lb = summary(samps2[["beta"]])$quantiles["2.5%"],
                         yield.ub = summary(samps2[["beta"]])$quantiles["97.5%"])
beta[7,"level"] <- "GrandMean"

# plot crosses for each covariate combination
plot <- ggplot(data=beta) + 
  geom_point(aes(x=yield.mean, y=richness.mean, color=factor(level)), size=4) +
  geom_pointrange(aes(x=yield.mean, y=richness.mean, ymin=richness.lb, 
                                   ymax=richness.ub,color=factor(level)), size=1.5) +
  geom_segment(aes(x=yield.lb, xend=yield.ub, y = richness.mean, yend = richness.mean, color=factor(level)),size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") + geom_vline(aes(xintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='level') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = path2temp %+% "CrossPlot_LUI.png", width = 20, height = 8, type = "cairo-png")


############################################################################
### 08.4.2. plot Panel for LUIrangelevel
############################################################################
one=3; two=5; three=7; alpha=100
y.offset <- 0.2
height <- (two-one)/2

## define colours per group
red <- rgb(228,26,28, alpha=alpha, max=255) # for SR
blue <- rgb(55,126,184, alpha=alpha, max=255) # for Yield

newdat <- beta[-which(beta$level=="GrandMean"),]
newdat[,-1] <- newdat[,-1]/max(abs(newdat[,-1]))

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

## forest plot
png(path2temp %+% "ForestPlot_LUI_Panel.png")
# panel set up
par(oma = c(3, 0, 0, 0),xpd=T)
plot(newdat$x1, newdat$y, pch=19, ylim =c(1+0.2, three-0.2), xlim=c(0, 6), ylab="", xlab="", xaxt="n", yaxt="n", col="white")
legend("bottom", inset=c(0,-0.25),legend=c("Richness","Yield"),title="Log-Response Ratios",col=c(red,blue),pch=16, horiz =T, bty="n")
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
points(newdat$x1+newdat$richness.mean,newdat$y-height+y.offset,col=red,pch=16)
points(newdat$x1+newdat$yield.mean,newdat$y-height-y.offset,col=blue,pch=16)

# plot 95%CI
for(i in newdat$level){
  temp <- subset(newdat,level==i)
  segments(x0=temp$x1+temp$richness.lb,x1=temp$x1+temp$richness.ub,y0=temp$y-height+y.offset,col=red,lwd=2)
  segments(x0=temp$x1+temp$yield.lb,x1=temp$x1+temp$yield.ub,y0=temp$y-height-y.offset,col=blue,lwd=2)  
}
dev.off()

par(..., no.readonly = T)
############################################################################
### 08.4.3 Plot model coefficients + 95%CI (cf. Fig1 in Newbold et al. 2015)
############################################################################
samps <- Richness.MA.model[["Select"]]$samps
X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR~",paste(reduced.model.richness$terms,collapse="+"),-1)), data=ES.frame.richness))
n.coeff <- length(reduced.model.richness$terms)
beta <- data.frame(coeffs=colnames(X.matrix))
beta$richness.mean <- summary(samps[["beta"]])$statistics[,"Mean"]
beta$richness.lb <- summary(samps[["beta"]])$quantiles[,"2.5%"]
beta$richness.ub <- summary(samps[["beta"]])$quantiles[,"97.5%"]    

ggplot(data=beta) +
  geom_point(aes(x=beta$coeffs, y=richness.mean), size=4) +
  geom_pointrange(aes(x=beta$coeffs, y=richness.mean, ymin=richness.lb, ymax=richness.ub)) +
  geom_hline(aes(yintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_discrete(limits=beta$coeffs) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("RR (Species Richness)") + xlab("")
  
samps <- Yield.MA.model[["Select"]]$samps
X.matrix <- as.data.frame(model.matrix(as.formula(paste("Log.RR~",paste(reduced.model.yield$terms,collapse="+"),-1)), data=ES.frame.yield))
n.coeff <- length(reduced.model.yield$terms)
beta <- data.frame(coeffs=colnames(X.matrix))
beta$yield.mean <- summary(samps[["beta"]])$statistics[,"Mean"]
beta$yield.lb <- summary(samps[["beta"]])$quantiles[,"2.5%"]
beta$yield.ub <- summary(samps[["beta"]])$quantiles[,"97.5%"]    

ggplot(data=beta) +
  geom_point(aes(x=beta$coeffs, y=yield.mean), size=4) +
  geom_pointrange(aes(x=beta$coeffs, y=yield.mean, ymin=yield.lb, ymax=yield.ub)) +
  geom_hline(aes(yintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_discrete(limits=beta$coeffs) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("RR (Yield)") + xlab("")
