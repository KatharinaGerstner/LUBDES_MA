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
newdat <- expand.grid(LUI.range.level=levels(ES.frame$LUI.range.level), BIOME=levels(ES.frame$BIOME))
newdat$level <- paste(newdat$LUI.range.level,newdat$BIOME)

model <- Richness.MA.model[["select"]]$model
mm <- model.matrix(model$call$mods, data=newdat)
#mm <- mm[,-which(colnames(mm)=="(Intercept)")]
preds <- predict.rma(model, newmods = mm)
newdat$logRR.richness <- preds$pred
newdat$logRR.richness.se <- preds$se

model <- Yield.MA.model[["select"]]$model
mm <- model.matrix(model$call$mods, data=newdat)
#mm <- mm[,-which(colnames(mm)=="(Intercept)")]
preds <- predict.rma(model, newmods = mm)
newdat$logRR.yield <- preds$pred
newdat$logRR.yield.se <- preds$se

# plot crosses for each covariate combination
plot <- ggplot(data=newdat) + 
  geom_point(aes(x=logRR.yield, y=logRR.richness, color=factor(level)), size=4) +
  geom_pointrange(aes(x=logRR.yield, y=logRR.richness, ymin=logRR.richness - (1.96*logRR.richness.se), 
                                   ymax=logRR.richness + (1.96*logRR.richness.se),color=factor(level)), size=1.5) +
  geom_segment(aes(x=logRR.yield - (1.96*logRR.yield.se), xend=logRR.yield + (1.96*logRR.yield.se), y = logRR.richness, yend = logRR.richness, color=factor(level)),size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") + geom_vline(aes(xintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_continuous(labels=trans_format("exp",comma_format(digits=2))) +
  ylab("RR (Species Richness)") + xlab("RR (Yield)") + labs(color='level') +
  theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.5)),legend.text=element_text(size = rel(1.5)),legend.title=element_text(size = rel(1.5))) +
  facet_grid(BIOME~LUI.range.level)
print(plot)

############################################################################
### 08.4.2 Plot model coeeficients + SE relative to the intercept (cf. Fig1 in Newbold et al. 2015)
############################################################################
# do we really need the intercept?? We only have additive effects and I fin it harder to interpret
model <- Richness.MA.model[["select"]]$model
ggplot() +
  geom_point(aes(x=rownames(model$b), y=model$b), size=4) +
  geom_pointrange(aes(x=rownames(model$b), y=model$b, ymin=model$b - (1.96*model$se), ymax=model$b + (1.96*model$se)), size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_discrete(limits=rownames(model$b)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("RR (Species Richness)") + xlab("")
  
model <- Yield.MA.model[["select"]]$model
ggplot() +
  geom_point(aes(x=rownames(model$b), y=model$b), size=4) +
  geom_pointrange(aes(x=rownames(model$b), y=model$b, ymin=model$b - (1.96*model$se), ymax=model$b + (1.96*model$se)), size=1.5) +
  geom_hline(aes(yintercept=0), linetype="twodash") +
  scale_y_continuous(labels=trans_format("exp",comma_format(digits=2))) + 
  scale_x_discrete(limits=rownames(model$b)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ylab("RR (Yield)") + xlab("")
