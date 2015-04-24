## ?????????????????????????????????????????????????
### use full model with all levels simultaneously or separate models for each level

### implement or not the variance-covariance matrix: 
# is the information content of the covariance of the ES worth to include at the harm of model robustness?
# theoretically: as was suggested by Wolfgang Viechtbauer
# empirically: calculate correlation matrix of ES for the entire data set and check whether there is a structure, i.e. correlations of ES within one study.case are generally higher
# if yes (how to determine?): apply theoretic calculations of covariance
# if no: there is no need to account for non-independen as it comes with increasing number of degrees of freedoms and makes the whole model less robust 
data.frame(data$Study.Case,data$LUI.comparison)

## Model selection for MA models
## Account for scale dependency in biodiversity data
## Effects of various yield indices on the effect sizes (RR vs SMD)