############################################################################
### Purpose of this skript module 09 is to:
###
### 09.1. model diagnostics
### 09.2. model performances
###
### General comments:
###
### Authors: KG
############################################################################

### run loop over all models
# for(mods in mods){}
mods <- "LUI.range.level"

############################################################################
### 09.1. model diagnostics
############################################################################
### relationship residuals vs model fit, non-linear?, homogeneity of variances? 
### residuals against fitted values, 
plot(rstandard(Richness.MA.model[[mods]])$z~fitted(Richness.MA.model[[mods]]), xlab="Fitted values", ylab="Standardized Residuals", main="Residuals vs. Fitted")
abline(h=0,lty="dashed")

### alternatively a Scale-Location plot of sqrt(| residuals |) against fitted values, 
plot(sqrt(abs(rstandard(Richness.MA.model[[mods]])$z))~fitted(Richness.MA.model[[mods]]), xlab="Fitted values", ylab=expression(sqrt("Standardized Residuals")), main="Scale-Location")
abline(h=0,lty="dashed")

### normality of resiuals
hist(residuals(Richness.MA.model[[mods]]))
# test the assumption of a normal distribution of the residuals
# H0: data is drawn from a normal distribution, reject if p<0.05
shapiro.test (residuals(Richness.MA.model[[mods]]))

### a Normal Q-Q plot,
qqnorm(y=residuals(Richness.MA.model[[mods]]))
qqline(y=residuals(Richness.MA.model[[mods]]))

### influential points
### a plot of Cook's distances against leverage/(1-leverage). 
# influence(Richness.MA.model[[mods]]) is not yet implemented in metafor for rma.mv objects. TO DO: Ask Wolfgang Viechtbauer whether implementation is planned or if he can suggest some workaround.
# an alternative approach from http://people.stern.nyu.edu/jsimonof/classes/2301/pdf/diagnost.pdf
h <- hatvalues(Richness.MA.model[[mods]])
x.seq <- seq(min(rstandard(Richness.MA.model[[mods]])$z),max(rstandard(Richness.MA.model[[mods]])$z),length.out=100)
y.seq <- seq(min(h),max(h),length.out=100)
D <- (x.seq^2*y.seq)/((length(Richness.MA.model[[mods]]$b)+1)*(1-y.seq))

plot(rstandard(Richness.MA.model[[mods]])$z~h,xlab="Leverage",ylab="Standardized Residuals", main="Residuals vs. Leverage")
#contour(x.seq,y.seq,D) # not working

### publication bias
# Egger's regression test (regtest() function),
# the trim and fill method (trimfill() function),

############################################################################
### 09.2. model performances
############################################################################
# model fit criteria (logLik() and deviance() functions), information criteria (AIC(), BIC(), and fitstats() functions),
fitstats(Richness.MA.model[[mods]])
fitstats(Yield.MA.model[[mods]])

# test against null model, H0: Moderator does explain the same amount of ES variation than the null model 
anova.rma(Richness.MA.model[["None"]],Richness.MA.model[[mods]])
anova.rma(Yield.MA.model[["None"]],Yield.MA.model[[mods]])
##Warning message:
# In anova.rma(Richness.MA.model[["None"]], Richness.MA.model[[mods]]) :
#   Models with different fixed effects. REML comparisons are not meaningful.     