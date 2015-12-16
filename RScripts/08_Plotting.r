############################################################################
### Purpose of this skript module 08 is to:
###
### 08.2. Plot cross-diagrams
### 08.2.1 Plot cross-diagrams by species
### 08.1.2 6 map by intensity classes
### 08.4. Forest plots for noLU vs low/medium/high LU
### 08.5  Forest plots for all data
### 08.6. CatWhiskerPlots 
###
### General comments:
### * TO DO: This needs cleanin, lots of importnat fingures are gone, some need to be recoded!
### * TO DO: change palette for scale_color_brewer() to get rid of the warning "In RColorBrewer::brewer.pal(n, pal) : n too large, allowed maximum for palette Set1 is 9 Returning the palette you asked for with that many colors"
###
### Authors: KG, MB, SK ...
###
### Ralf: I suggest using 08_Plotting.r in as main scrtipt for plotting, calling 
### various scripts which generate figures we need for 
### - Main Paper
### - Appendix
### and other analysis, what ever we need.


############################################################################
### Plot figure for Main Paper
###
### Figure 1 
### Text in paper: "cross-diagram including the grand mean + one covariate 
### (e.g. species group), photos in each quadrant fitting to ++,--,+-,-+ (maybe 
### only 3 and put legend in 4th position)

### missing

### Figure 2: cat Whisker-Plois
### TExt in paper: cat-whisker plot, two panels (BD, ES) without showing the 
### low-high. greyed-out uncertainty area + grand mean thick lines for products (rgb) 
source(path2wd %+% "08.2_plotting.r")

############################################################################
### Plot figure for Appendix
###

### Figure SA.1: enn Diagram showing the numbers of manually screened study 
### abstracts. (Michael)

### Figure SA.4: World Map
source(path2wd %+% "08.1_plotting.r")

### Figure SA.5: Forets Plots
source(path2wd %+% "08.3_plotting.r")


############################################################################
### 08.2. Plot cross-diagrams
### 
############################################################################
# 
# ### Define predFrame for all possible combinations of one varying covariable m and all other fixed
# sign.terms <- RichnessModel$stats$terms[RichnessModel$stats$P<0.05]
# print(sign.terms)
# comb.terms <- levels(factor(paste(modelDataRichness[,paste(sign.terms[1])],modelDataRichness[,paste(sign.terms[2])],modelDataRichness[,paste(sign.terms[3])],sep="/")))
# 
# div.terms <- sapply(comb.terms,function(x){strsplit(x,"/")})
# predFrameR <- data.frame(Species.Group=unlist(lapply(div.terms,function(x) x[1])), 
#                         LUI.range.level=unlist(lapply(div.terms,function(x) x[2])),
#                         BIOME=unlist(lapply(div.terms,function(x) x[3])))
# 
# modelDataRichness$LUI.range.level <- factor(modelDataRichness$LUI.range.level)
# modelDataYield$LUI.range.level <- factor(modelDataYield$LUI.range.level)
# 
# ES.frame$LUI.range.level <- factor(paste(ES.frame$LUI.range.level),levels=levels(modelDataYield$LUI.range.level))
# 
# predsR <- predict.rma(RichnessModel$model,newmods = newMods)
# predFrameR <- cbind(predFrameR,predsR=predsR$pred,predsR.se=predsR$se)
# 
# 
# ### Define predFrame for all possible combinations of one varying covariable m and all other fixed
# sign.terms <- YieldModel$stats$terms[YieldModel$stats$P<0.05]
# print(sign.terms)
# comb.terms <- levels(factor(paste(modelDataY[,paste(sign.terms[1])],modelDataY[,paste(sign.terms[2])],sep="/")))
# 
# div.terms <- sapply(comb.terms,function(x){strsplit(x,"/")})
# predFrameY <- data.frame(LUI.range.level=unlist(lapply(div.terms,function(x) x[1])), 
#                          Product=unlist(lapply(div.terms,function(x) x[2])))
# 
# newMods <- model.matrix(~ LUI.range.level + Product, data=predFrameY)
# newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]
# 
# predsY <- predict.rma(YieldModel$model,newmods = newMods)
# predFrameY <- cbind(predFrameY,predsY=predsY$pred,predsY.se=predsY$se)
# 
# predFrame <- join(predFrameR,predFrameY,type="full")
# 
# for (lui in levels(predFrameR$LUI.range.level)){
#   predFrame.subset <- subset(predFrame, LUI.range.level == lui)
#   predFrame.subset$Class <- paste(predFrame.subset$Species.Group,predFrame.subset$BIOME,predFrame.subset$Product,sep="/")
#   plot.lui <- ggplot(data=predFrame.subset) +
#     geom_pointrange(aes(x=predsY, y=predsR, ymin=predsR - (1.96*predsR.se), ymax=predsR + (1.96*predsR.se), color=Class), size=1.5) +
#     geom_segment(aes(x=predsY - (1.96*predsY.se), xend=predsY + (1.96*predsY.se), y = predsR, yend = predsR, color=Class),size=1.5) +
#     geom_hline(x=0, linetype="twodash") + geom_vline(y=0, linetype="twodash") +
#     scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#     scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
#     scale_colour_brewer(palette="Set1",labels=paste(predFrame.subset$Class)) +
#     ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=paste(lui)) +
#     theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
#   save
# }
# plot <- ggplot() + 
#   geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,'Species.Group'])), size=4, alpha=.5) +
#   geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
#                                   ymax=pred + (1.96*se),color=Species.Group), size=1.5) +
#   geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=Species.Group),size=1.5) +
#   geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
#   scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#   scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
#   scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$Species.Group)," (",table(factor(modelDataR[,'Species.Group'])), ")", sep="")) +
#   ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='Species.Group') +
#   theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
# print(plot)
# ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_Species.Group.png",sep=""), width = 20, height = 8, type = "cairo-png")

modelDataR <- modelDataRichness
modelDataY <- modelDataYield


predFrame <- data.frame(Species.Group=factor("invertebrates",levels=levels(modelDataR$Species.Group)),
                        LUI.range.level=factor(levels(modelDataR$LUI.range.level),levels=levels(modelDataR$LUI.range.level)),
                        BIOME=factor("Tropical_Forests",levels=levels(modelDataR$BIOME)))

newMods <- model.matrix(~Species.Group + LUI.range.level + BIOME,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsR <- predict.rma(RichnessModel$model,newmods = newMods)
predsR<-data.frame(pred=predsR$pred,se=predsR$se)
predsR$LUI.range.level <- factor(levels(modelDataR$LUI.range.level),levels = levels(modelDataR$LUI.range.level))

predFrame <- data.frame(LUI.range.level=factor(levels(modelDataR$LUI.range.level),levels=levels(modelDataR$LUI.range.level)),
                        Product=factor("crop",levels=levels(modelDataR$Product)))

newMods <- model.matrix(~LUI.range.level + Product,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsY <- predict.rma(YieldModel$model,newmods = newMods)
predsY<-data.frame(pred=predsY$pred,se=predsY$se)
predsY$LUI.range.level <- factor(levels(modelDataY$LUI.range.level),levels = levels(modelDataY$LUI.range.level))

plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=factor(ES.frame[,'LUI.range.level'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                   ymax=pred + (1.96*se),color=LUI.range.level), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=LUI.range.level),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$LUI.range.level)," (",table(factor(modelDataR[,'LUI.range.level'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='LUI.range.level') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_LUI.range.level.png",sep=""), width = 20, height = 8, type = "cairo-png")


predFrame <- data.frame(LUI.range.level=factor("medium-medium",levels=levels(modelDataR$LUI.range.level)),
                        Product=factor(levels(modelDataR$Product),levels=levels(modelDataR$Product)))

newMods <- model.matrix(~LUI.range.level + Product,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsY <- predict.rma(YieldModel$model,newmods = newMods)
predsY<-data.frame(pred=predsY$pred,se=predsY$se)
predsY$Product <- factor(levels(modelDataY$Product),levels = levels(modelDataY$Product))

predsR <- data.frame(pred=rep(coefficients(Richness.MA.fit),dim(predsY)[1]),
                     se=rep(Richness.MA.fit$se,dim(predsY)[1]))
predsR$Product <- factor(levels(modelDataR$Product),levels = levels(modelDataR$Product))


plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,'Product'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                   ymax=pred + (1.96*se),color=Product), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=Product),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$Product)," (",table(factor(modelDataR[,'Product'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='Product') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_Product.level.png",sep=""), width = 20, height = 8, type = "cairo-png")

predFrame <- data.frame(Species.Group=factor("invertebrates",levels = levels(modelDataR$Species.Group)),
                        LUI.range.level=factor("high-high",levels=levels(modelDataR$LUI.range.level)),
                        BIOME=factor(levels(modelDataR$BIOME),levels=levels(modelDataR$BIOME)))

newMods <- model.matrix(~Species.Group + LUI.range.level + BIOME,data=predFrame)
newMods <- newMods[,-which(colnames(newMods)=="(Intercept)")]

predsR <- predict.rma(RichnessModel$model,newmods = newMods)
predsR<-data.frame(pred=predsR$pred,se=predsR$se)
predsR$BIOME <- factor(levels(modelDataR$BIOME),levels = levels(modelDataR$BIOME))

predsY <- data.frame(pred=rep(coefficients(Yield.MA.fit),dim(predsR)[1]),
                     se=rep(Yield.MA.fit$se,dim(predsR)[1]))

predsY$BIOME <- factor(levels(modelDataR$BIOME),levels = levels(modelDataR$BIOME))

plot <- ggplot() + 
  geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,'BIOME'])), size=4, alpha=.5) +
  geom_pointrange(data=predsR, aes(x=predsY$pred, y=pred, ymin=pred - (1.96*se), 
                                   ymax=pred + (1.96*se),color=BIOME), size=1.5) +
  geom_segment(data=predsY, aes(x=pred - (1.96*se), xend=pred + (1.96*se), y = predsR$pred, yend = predsR$pred, color=BIOME),size=1.5) +
  geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  scale_colour_brewer(palette="Set1",labels=paste(levels(predsR$BIOME)," (",table(factor(modelDataR[,'BIOME'])), ")", sep="")) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='BIOME') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),
        legend.title=element_text(size = rel(1.5)))
print(plot)
ggsave(plot, file = paste(path2temp,"/New_Cross_diagram_BIOME.png",sep=""), width = 20, height = 8, type = "cairo-png")

# 
# 
# ### plot cross diagrams for categorical moderators
# for(choose.moderator in as.character(unique(MA.coeffs.cat$Moderator))){
#   ES.moderator.subset <- subset(MA.coeffs.cat, Moderator %in% choose.moderator)
#   ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
#   ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
#   
#   ### plot cross diagrams
#   if(nrow(ES.moderator.subset) >= 2){
#     plot <- ggplot() + 
#       geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR, color=as.factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), size=4, alpha=.5) +
#       geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness - (1.96*se.Richness), ymax=mean.Richness + (1.96*se.Richness),color=factor(levels)), size=1.5) +
#       geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness, color=factor(levels)),size=1.5) +
#       geom_hline(data=ES.frame, x=0, linetype="twodash") + geom_vline(data=ES.frame, y=0, linetype="twodash") +
#       scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#       scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
#       scale_colour_brewer(palette="Set1",labels=paste(levels(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)]))," (",table(factor(ES.frame[,which(names(ES.frame) %in% choose.moderator)])), ")", sep="")) +
#       ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color=choose.moderator) +
#       theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
#     print(plot)
#   }
#   
#   if(nrow(ES.moderator.subset) == 1){
#    plot <- ggplot() + 
#       geom_point(data=ES.frame, aes(x=Yield.Log.RR, y=Richness.Log.RR), color="grey", size=3.5, alpha=.5) +
#       geom_pointrange(data=ES.moderator.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness)), color="green", size=1) +
#       geom_segment(data=ES.moderator.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness), color="green", size=1) +
#       geom_hline(data=ES.frame, x=0, linetype="twodash") + 
#       geom_vline(data=ES.frame, y=0, linetype="twodash") +
#       scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#       scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
#       ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
#       theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))    
#    
#    print(plot)
#   }
# 
# ggsave(plot, file = paste(path2temp,"/Cross_diagram_",gsub(".","",choose.moderator,fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
# 
# }
# 
# ### plot scatterplots for continuous moderators
# for(i in 2:nrow(MA.coeffs.cont)){
#   mods <- MA.coeffs.cont$Moderator[i]
#   print(mods)
#   ES.moderator.subset <- MA.coeffs.cont[i,]
#   if(all(is.na(ES.moderator.subset[-1]))) next
#   
#   ## Transform prediction list from rma to dataframe
#   richness.preds.df <- data.frame(slab=preds.richness[[mods]][[1]]$slab, pred.richness=preds.richness[[mods]][[1]]$pred, ci.lb.richness=preds.richness[[mods]][[1]]$ci.lb, ci.ub.richness=preds.richness[[mods]][[1]]$ci.ub)
#   yield.preds.df <- data.frame(slab=preds.yield[[mods]][[1]]$slab, pred.yield=preds.yield[[mods]][[1]]$pred, ci.lb.yield=preds.yield[[mods]][[1]]$ci.lb, ci.ub.yield=preds.yield[[mods]][[1]]$ci.ub)
#    
#   Richness.reg.line <- function(xvar){
#     ES.moderator.subset$Richness.intercept+ES.moderator.subset$Richness.slope*xvar
#   }
#   Yield.reg.line <- function(xvar){
#     ES.moderator.subset$Yield.intercept+ES.moderator.subset$Yield.slope*xvar
#   }
#   
#   plot1 <- ggplot() + 
#     geom_point(aes(x=ES.frame.richness[,paste(mods)],y=ES.frame.richness$Richness.Log.RR),color="red", size=3.5, alpha=.5) +
#     geom_point(aes(x=ES.frame.yield[,paste(mods)],y=ES.frame.yield$Yield.Log.RR),color="blue", size=3.5, alpha=.5) +
#     geom_abline(x=ES.frame.richness[,paste(mods)],intercept=ES.moderator.subset$Richness.intercept, slope=ES.moderator.subset$Richness.slope,color="red") +
#     geom_abline(aes(x=ES.frame.yield[,paste(mods)]),intercept=ES.moderator.subset$Yield.intercept, slope=ES.moderator.subset$Yield.slope,color="blue") +
#     geom_ribbon(aes(x=ES.frame.richness[!is.na(ES.frame.richness[,paste(mods)]),paste(mods)],ymin=richness.preds.df$ci.lb.richness,ymax=richness.preds.df$ci.ub.richness),fill="red",alpha=0.2) +
#     geom_ribbon(aes(x=ES.frame.yield[!is.na(ES.frame.yield[,paste(mods)]),paste(mods)],ymin=yield.preds.df$ci.lb.yield,ymax=yield.preds.df$ci.ub.yield),fill="blue",alpha=0.2) +
#     geom_hline(y=0, linetype="twodash") + 
#     scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
#     scale_colour_manual(values=c("red","blue"),labels=c("Richness","Yield")) +
#     ylab("RR")  + xlab(paste(mods)) +
#     theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
#   
#   print(plot1)
#   ggsave(plot1, file = paste(path2temp, "/Scatterplot_",gsub(".","",MA.coeffs.cont$Moderator[i],fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
# }
# 
# ############################################################################
# ### 08.3. Forest plots for noLU vs low/medium/high LU
# ### 
# ############################################################################
# 
# ### plot all in one rush
# #TO DO: Plot for no moderators
# for(choose.moderator in as.character(unique(MA.coeffs.noLU$Moderator))[-1]){
#   ES.moderator.subset <- subset(MA.coeffs.noLU, Moderator %in% choose.moderator)
#   ES.moderator.subset$Moderator <- factor(ES.moderator.subset$Moderator)
#   ES.moderator.subset$levels <- factor(ES.moderator.subset$levels)
#   
#   if(nrow(ES.moderator.subset) >= 2){
#     ifelse(choose.moderator=="Product:High.LUI",n.levels <- table(paste(ES.frame.noLU$Product,ES.frame.noLU$High.LUI,sep=":"))[length(n.levels):1], n.levels <- table(factor(ES.frame.noLU[,which(names(ES.frame.noLU) %in% choose.moderator)])))
#     plot <- ggplot(ES.moderator.subset, aes(x=paste(levels," (",n.levels, ")", sep=""), y=mean.Richness, ymin=mean.Richness-1.96*se.Richness, ymax=mean.Richness+1.96*se.Richness)) + 
#       geom_pointrange(size=1.2) + 
#       coord_flip() +
#       geom_hline(x=0, linetype="twodash") + # weird: draws the line at x=0!!
#       scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#       ylab("Response Ratio") +
#       xlab(ES.moderator.subset$Moderator)  + #switch because of the coord_flip() above 
#       theme(axis.title = element_text(size = rel(1.8)), axis.text = element_text(size = rel(1.8)),legend.text=element_text(size = rel(1.8)),legend.title=element_text(size = rel(1.8)))
#     print(plot)
#   }
#   
#   ggsave(plot, file = paste(path2temp, "/ForestPlot",gsub(":","",gsub(".","",choose.moderator,fixed=T),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
#   
# }
# 
# 
# 
# ############################
# ### SK Annapolis WS 2015 ###
# ### cross diagramm with only 3 groups: low-low, medium-medium, high-high
# ############################
# ### ------------------------
# 
# # select dataset and moderator grand means to plot
# data.to.plot = ES.frame[which(ES.frame$Low.LUI == ES.frame$High.LUI),]
# moderator.grand.mean.subset = MA.coeffs.cat[which(MA.coeffs.cat$Moderator %in% "LUI.range.level" & MA.coeffs.cat$levels %in% c("low-low","medium-medium","high-high")),]
# 
# # order levels of moderators so they are displayed in same colour
# moderator.grand.mean.subset$levels = factor(moderator.grand.mean.subset$levels, levels=c("low-low","medium-medium","high-high"))
# data.to.plot$LUI.range.level = factor(data.to.plot$LUI.range.level, levels=c("low-low","medium-medium","high-high"))
# 
# #set colour scheme here ! 3 for points and 3 for crosses
# colours.to.plot = c("#33FF00","#FFCC00","#FF0000","#33FF00","#FFCC00","#FF0000")
# 
# # get axes length to center the plot 
# if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
#   y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
# }else{
#   y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
# }
# 
# if(0 - min(data.to.plot$Yield.Log.RR) > max(data.to.plot$Yield.Log.RR)){
#   x.range.for.plot = c(0 -max(data.to.plot$Yield.Log.RR) - 0.5,max(data.to.plot$Yield.Log.RR) + 0.5 )
# }else{
#   x.range.for.plot = c(min(data.to.plot$Yield.Log.RR)-0.5,0-min(data.to.plot$Yield.Log.RR) + 0.5)
# }
# 
# plot.within.groups.cross.diagramm =  ggplot() + 
# 
#   # study cases as points
#   geom_point(data= data.to.plot, aes(x=Yield.Log.RR, y=Richness.Log.RR,colour=LUI.range.level), size=3.5,alpha = 0.5) +
#   
#   #grand mean for moderators
#   geom_pointrange(data=moderator.grand.mean.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness),colour=levels), size=1) +
#   geom_segment(data=moderator.grand.mean.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness,colour=levels), size=1) +
# 
#   # colouring
#   scale_colour_manual(values=colours.to.plot,
#                       breaks=c("low-low","medium-medium","high-high"),
#                       labels=c("low-low","medium-medium","high-high")) +
#   
#   # lines for zero response rations
#   geom_hline(data=ES.frame, x=0, linetype="twodash",colour="grey") + 
#   geom_vline(data=ES.frame, y=0, linetype="twodash",colour="grey") +
#     
#   #white background
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
#   
#   scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#   scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
#   ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
#   theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +
# 
#   #legend
#   theme(legend.position="top",legend.title=element_blank())   +
# 
#   # place the zero-zero lines in the middle of the plot
#   coord_cartesian(ylim= y.range.for.plot, xlim=x.range.for.plot)
# 
# print(plot.within.groups.cross.diagramm)
# ggsave(plot.within.groups.cross.diagramm, file = paste(path2temp, "/Cross_diagram_within_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")
# 
# ############################
# ### SK Annapolis WS 2015 ###
# ### cross diagramm with only 3 groups: low-medium, low-high, medium-high
# ############################
# ### ------------------------
# 
# 
# 
# # select dataset and moderator grand means to plot
# data.to.plot = ES.frame[which(!(ES.frame$Low.LUI == ES.frame$High.LUI)),]
# moderator.grand.mean.subset = MA.coeffs.cat[which(MA.coeffs.cat$Moderator %in% "LUI.range.level" & MA.coeffs.cat$levels %in% c("low-medium","medium-high","low-high")),]
# 
# # order levels of moderators so they are displayed in same colour
# moderator.grand.mean.subset$levels = factor(moderator.grand.mean.subset$levels, levels=c("low-medium","medium-high","low-high"))
# data.to.plot$LUI.range.level = factor(data.to.plot$LUI.range.level, levels=c("low-medium","medium-high","low-high"))
# 
# #set colour scheme here ! 3 for points and 3 for crosses
# colours.to.plot = c("#3399FF","#FFCC00","#FF0000","#3399FF","#FFCC00","#FF0000")
# 
# # get axes length to center the plot 
# if(0 - min(data.to.plot$Richness.Log.RR) > max(data.to.plot$Richness.Log.RR)){
#   y.range.for.plot = c(0 -max(data.to.plot$Richness.Log.RR)-0.5,max(data.to.plot$Richness.Log.RR)+0.5)
# }else{
#   y.range.for.plot = c(min(data.to.plot$Richness.Log.RR)-0.5,0-min(data.to.plot$Richness.Log.RR)+0.5)
# }
# 
# if(0 - min(data.to.plot$Yield.Log.RR) > max(data.to.plot$Yield.Log.RR)){
#   x.range.for.plot = c(0 -max(data.to.plot$Yield.Log.RR) - 0.5,max(data.to.plot$Yield.Log.RR) + 0.5 )
# }else{
#   x.range.for.plot = c(min(data.to.plot$Yield.Log.RR)-0.5,0-min(data.to.plot$Yield.Log.RR) + 0.5)
# }
# 
# plot.across.groups.cross.diagramm =  ggplot() + 
#   
#   # study cases as points
#   geom_point(data= data.to.plot, aes(x=Yield.Log.RR, y=Richness.Log.RR,colour=LUI.range.level), size=3.5,alpha = 0.5) +
#   
#   #grand mean for moderators
#   geom_pointrange(data=moderator.grand.mean.subset, aes(x=mean.Yield, y=mean.Richness, ymin=mean.Richness	- (1.96*se.Richness), ymax=mean.Richness	+ (1.96*se.Richness),colour=levels), size=1) +
#   geom_segment(data=moderator.grand.mean.subset, aes(x=mean.Yield - (1.96*se.Yield), xend=mean.Yield + (1.96*se.Yield), y = mean.Richness, yend = mean.Richness,colour=levels), size=1) +
#   
#   # colouring
#   scale_colour_manual(values=colours.to.plot,
#                       breaks=c("low-medium","medium-high","low-high"),
#                       labels=c("low-medium","medium-high","low-high")) +
#   
#   # lines for zero response rations
#   geom_hline(data=ES.frame, x=0, linetype="twodash",colour="grey") + 
#   geom_vline(data=ES.frame, y=0, linetype="twodash",colour="grey") +
#   
#   #white background
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
#   
#   scale_y_continuous(labels=trans_format("exp", comma_format(digits=2))) + 
#   scale_x_continuous(labels=trans_format("exp", comma_format(digits=2))) +
#   ylab("RR (Species Richness)") + xlab("RR (Yield)") + guides(fill=FALSE) +
#   theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +
#   
#   #legend
#   theme(legend.position="top",legend.title=element_blank())   +
#   
#   # place the zero-zero lines in the middle of the plot
#   coord_cartesian(ylim= y.range.for.plot, xlim=x.range.for.plot)
# 
# print(plot.across.groups.cross.diagramm)
# ggsave(plot.across.groups.cross.diagramm, file = paste(path2temp, "Cross_diagram_across_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")
# 
# 
# ############################
# ### SK Annapolis WS 2015 ###
# ### forest plots for each LUI-comparison class
# ############################
# ### ------------------------
# 
# LUI.range.level = c("low-low","medium-medium","high-high","low-medium","medium-high","low-high")
# 
# 
# LUI.level.to.plot = LUI.range.level[1]
# 
# for(LUI.level.to.plot in LUI.range.level){
#   data.to.plot = subset(ES.frame, LUI.range.level %in% LUI.level.to.plot)
#   data.to.plot = data.to.plot[order(data.to.plot$Yield.Log.RR,decreasing=T),]
#   data.to.plot$uniqueID = factor(paste(data.to.plot$Study.ID,data.to.plot$Case.ID),levels=paste(data.to.plot$Study.ID,data.to.plot$Case.ID))
# 
#   
#   # get axes length to center the plot 
#   min.values = sqrt(min(c(data.to.plot$Yield.Log.RR,data.to.plot$Richness.Log.RR))^2)
#   max.values = sqrt(max(c(data.to.plot$Yield.Log.RR,data.to.plot$Richness.Log.RR))^2)
#   
#   if(min.values > max.values){
#     y.range.for.plot = c(0 - min.values -0.5,min.values + 0.5)
#   }else{
#     y.range.for.plot = c(0 - max.values -0.5,max.values + 0.5)
#   }
#   
#   
# plot.forest = ggplot(data=data.to.plot) +
#   
#   geom_pointrange(aes(x=uniqueID, y=Yield.Log.RR, ymin=Yield.Log.RR	- (1.96*Yield.Log.RR.Var), ymax=Yield.Log.RR	+ (1.96*Yield.Log.RR.Var),colour="Yield"), size=1) +
#   geom_pointrange(aes(x=uniqueID, y=Richness.Log.RR, ymin=Richness.Log.RR	- (1.96*Richness.Log.RR.Var), ymax=Richness.Log.RR	+ (1.96*Richness.Log.RR.Var),colour="Species Richness"), size=1) +
#   geom_hline(x=0,linetype ="twodash")  +
# 
#   
#   #scale manually to get the legend correct
#   scale_colour_manual(values  =c("#00CC00","#FF6633")) +
#   
#   #white background + flip 90 degrees
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(), 
#       panel.background = element_rect(colour = "black", size=1,fill=NA), axis.line = element_line(colour = "black")) +
#   coord_flip(ylim=y.range.for.plot)
# 
#   print(plot.forest)
#   ggsave(plot.forest, file = paste(c(path2temp, "Forest_plot_",LUI.level.to.plot,".png"), collapse=""), width = 20, height = 8, type = "cairo-png")
# 
# }

# print(plot.across.groups.cross.diagramm)
# ggsave(plot.across.groups.cross.diagramm, file = paste(path2temp, "Cross_diagram_across_LUI_range_levels.png",sep=""), width = 20, height = 8, type = "cairo-png")

