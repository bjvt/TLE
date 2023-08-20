

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle")
#setwd("C:/Users/bvt/Dropbox/eng_science/R/wd/tle")
#for the TLE use #cat temp.txt | paste - - > g.txt #cat g.txt | tr -d '\r' > h.txt #cat h.txt | tr -d '\t' > i.txt
# cat 3le.4Apr19.txt | paste - - - > g.txt, cat h.txt | tr -d '\r' > i.txt
 
#Load required packages
#library(gridExtra)
#library(moments) # needed for skewness
library(ggplot2)

library(dplyr) # part of the tidyverse modules

G <- 6.67408E-11
M <- 5.972E24
EarthRadius <- 6.371E6
EarthVolume <- (EarthRadius^3 * 4 * pi)/3 # in meters^3

#
tle_master <- read.fwf("two.txt", widths = c(1,1,22,45,1,1,5,1,1,2,3,3,1,2,12,1,10,1,8,1,8,1,1,1,4,1,1,1,5,1,8,1,8,1,7,1,8,1,8,1,11,5,1), header = FALSE, 
                       col.names=c('LineNumber0','','SatelliteName','','LineNumber1','','SatelliteNumber1','Classification','','ID1','ID2','ID3','','EpochYear','EpochDay','',
                                    'FirstTimeDerivative','','SecondTimeDerivative','','BSTAR','','Ephemeris','','ElementSetNumber','Checksum1',
                                    'LineNumber2','','SatelliteNumber2','','Inclination','','RightAscension','','Eccentricity','','Perigee','',
                                     'MeanAnomaly','','MeanMotion','Revolution','Checksum2'))

tle_master$altitude <- ((((1/tle_master$MeanMotion*86400)^2/((2*pi))^2)*G*M)^(1/3)-EarthRadius)
#convert to kilometers
tle_master$altitude <- tle_master$altitude/1000

hist <- hist(tle_master$altitude, breaks = 25, main = paste("Histogram of Number of Satellites Per Band of Orbit"),
     xlab = "Altitude in Kilometers")




ghist <- ggplot(tle_master, aes(x=altitude)) +
      geom_histogram(binwidth = 20, color="green") +
      labs(title="Histogram of Number of Satellites Per Band of Orbit",
           x = "Altitude in bins of 20 Kilometers",
           y = "Object Count")
ghist


       