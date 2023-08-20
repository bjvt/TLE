

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle")
#setwd("C:/Users/bvt/Dropbox/eng_science/R/wd/tle")
# https://rdrr.io/github/justinmooney/ecefR/ - ECEF to LLA converter, package remotes
# https://rdrr.io/github/justinmooney/ecefR/man/xyz2lla.html
#remotes::install_github("justinmooney/ecefR")
# verify site, https://www.ngs.noaa.gov/cgi-bin/xyz_getgp.prl it has a cacultator xyz2lla
# mass converter, https://www.ngs.noaa.gov/NCAT/
# test wgs 10 

#Load required packages
#library(gridExtra)
#library(moments) # needed for skewness
library(ggplot2)
library(plyr)
library(ecefR)
library(jsonlite)
#library(rjson)
library(tidyr)
library(dplyr) # part of the tidyverse modules


# read the file with the names
satcat.df <- fromJSON("json.14Jul20.json") %>% as.data.frame
# satcat.list <- fromJSON(file= "json.14Jul20.json")
# satcat.df <- as.data.frame(satcat.list)
hist(satcat.df$NORAD_CAT_ID, xlim=c(0,100000), breaks = 10, las = 1)
summary(satcat.df$NORAD_CAT_ID)
#