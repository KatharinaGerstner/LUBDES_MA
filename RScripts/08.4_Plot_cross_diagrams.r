############################################################################
### Purpose of this skript module 08.4 is to:
###
### 08.4.1. plot cross diagrams
###
###
### Authors: KG, ...
############################################################################

# predict for each covariate combination
newdat <- expand.grid(LUI.range.level=levels(ES.frame$LUI.range.level), Product=levels(ES.frame$Product))
newdat$level <- paste(newdat$LUI.range.level,newdat$Product)

model <- Richness.MA.model[["select"]]$model
mm <- model.matrix(model$call$mods, data=newdat)
mm <- mm[,-which(colnames(mm)=="(Intercept)")]
preds <- predict.rma(model, newmods = mm)
newdat$logRR.richness <- preds$pred
newdat$logRR.richness.se <- preds$se

model <- Yield.MA.model[["select"]]$model
mm <- model.matrix(model$call$mods, data=newdat)
mm <- mm[,-which(colnames(mm)=="(Intercept)")]
preds <- predict.rma(model, newmods = mm)
newdat$logRR.yield <- preds$pred
newdat$logRR.yield.se <- preds$se

# plot crosses for each covariate combination
plot <- ggplot() + 
  geom_point(data=newdat, aes(x=logRR.yield, y=logRR.richness, color=factor(level)), size=4) +
  geom_pointrange(data=newdat, aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                                   ymax=logRR.richness + (1.96*logRR.richness.se),color=factor(level)), size=1.5) +
  geom_segment(data=newdat, aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=factor(level)),size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") + geom_vline(aes(xintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='level') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5)))
print(plot)

