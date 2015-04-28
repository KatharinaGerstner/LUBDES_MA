####
install.packages("devtools")
library(devtools)
devtools::install_github("jennybc/googlesheets") # documentation at:http://htmlpreview.github.io/?https://raw.githubusercontent.com/jennybc/googlesheets/master/vignettes/basic-usage.html and https://github.com/jennybc/googlesheets
library(googlesheets)

authorize() #go to browser and enter your login credentials

list_sheets()
LUBDES_gsheet<-register_ss("LUBDES coding table v2") # load LUBDES  coding table, this crashes sometimes but seems to work as of April 22 2015
data <- get_via_lf(LUBDES_gsheet, ws = "1. Coding Table version 2") #does not seem to work
data <- get_via_cf(LUBDES_gsheet, ws = "1. Coding Table version 2") #works but it seems to be confused by our first four lines

#try again with copy of first few rows without empty rows
list_sheets()
LUBDES_gsheet<-register_ss("LUBDES_coding_copy")
data <- get_via_lf(LUBDES_gsheet, ws = "Sheet1") #seems ok

#try again with copy of entire sheet without without empty rows
list_sheets()
LUBDES_gsheet<-register_ss("LUBDES_coding_copy")
data <- get_via_lf(LUBDES_gsheet, ws = "Sheet2") #seems ok

# so the problem is in the first few empty rows ...

#try if plotting works to see if data structure is the same as in csv
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, main="Distribution of LUBDES studies included in the meta analysis")
points(data$lon, data$lat, col = "blue", cex = .6, pch=2)
