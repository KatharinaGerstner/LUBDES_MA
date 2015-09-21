############################################################################
### Purpose of this skript -01 is to:
###
### -01.1. set the working directory
### -01.2. load all libraries needed for subsequent analysis
### -01.3. load data directly from google docs
############################################################################

############################################################################
### -01.1. set the working directory
###
### all lines should stay commeted out
############################################################################

#setwd("C:\\Users\\hoppek\\Documents\\GitHub\\LUBDES_MA") #KG
#setwd("C:\\Users\\kambach\\Desktop\\aktuelle Arbeiten\\SESYNC\\myAnalysis") #SK
#setwd("~/Dropbox/SESYNC-UFZ-sDiv-Call Biodiversity and Ecosystem Services/Meta-Analysis/DataAnalysis") #MB
#setwd("~/git/LUBDES_MA") #MB

# install.packages("devtools")
library(devtools)
devtools::install_github("jennybc/googlesheets") # documentation at:http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html and https://github.com/jennybc/googlesheets
library(googlesheets)

# authorize() #go to browser and enter your login credentials #does not work any more, use:

gs_ls() #follow the displayed url, go to browser and enter your login credentials click accept and copy key back into R
gs_ls() # once authorized, this will list the files you have in GS
#gs_auth(new_user = TRUE) # you get an unauthorized error

LUBDES_gsheet<- gs_title("LUBDES coding table v2") # load LUBDES  coding table, this crashes sometimes but seems to work as of April 22 2015
data <- gs_read(LUBDES_gsheet, ws = "1. Coding Table version 2") # consume data from sheet 1
# data <- get_via_cf(LUBDES_gsheet, ws = "1. Coding Table version 2") #works but it seems to be confused by our first four lines # old function

# ### OBSOLETE since empty rows have been deleted
# #try again with copy of first few rows without empty rows
# list_sheets()
# LUBDES_gsheet<-register_ss("LUBDES_coding_copy")
# data <- get_via_lf(LUBDES_gsheet, ws = "Sheet1") #seems ok
# 
# #try again with copy of entire sheet without without empty rows
# gs_ls()
# LUBDES_gsheet<-register_ss("LUBDES_coding_copy")
# data <- get_via_lf(LUBDES_gsheet, ws = "Sheet2") #seems ok
# 
# # so the problem is in the first few empty rows ...

write.csv(data, "Input/LUBDES coding table v2 - 1. Coding Table version 2.csv")

library(maps) # For map data
library(ggplot2)
world_map <- map_data("world")
p <- ggplot(legend=FALSE) +
  geom_polygon(data=world_map, aes(x=long, y=lat)) + 
  geom_point(data=data, aes(x=longitude..E..W., y=latitude..N..S.), color="blue")
p ## looks weird, the reason is the max latitude in data = 2011! - check!
ggsave("Output/CaseDistribution.pdf", width=8, height=8, units="cm")
