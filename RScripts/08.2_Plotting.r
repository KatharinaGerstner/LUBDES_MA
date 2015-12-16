############################################################################
### 08.2. Cat Whisker Plots
### 
### added by Ralf, no clue how to use the models data or 
###
### suggestions for modifications:
### - separate by producs
### - use model data
### - use ggplot
############################################################################

# load Helens code
source("RR-Slope_Plots.R")

CatsWhiskers_plot(data=ES.frame.yield, dataType="raw", YieldorRichness = "yield")
CatsWhiskers_plot(data=ES.frame.richness, dataType="raw", YieldorRichness = "richness")



dev.new()