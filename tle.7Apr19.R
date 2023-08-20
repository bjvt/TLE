

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle")
#setwd("C:/Users/bvt/Dropbox/eng_science/R/wd/tle")
# take the 3 line element file and make two files, 1st one is Satellite Name, 2nd is the modified TLE ready for R
# sed '/^1/d' 3le.4Apr19.txt | sed '/^2/d' | sed 's/^.\{2\}//' > one.txt
# sed '/^0/d' 3le.4Apr19.txt | paste - - | tr -d '\r' | tr -d '\t' > two.txt

#Load required packages
#library(gridExtra)
#library(moments) # needed for skewness
library(ggplot2)
library(plyr)

library(dplyr) # part of the tidyverse modules

G <- 6.67408E-11
M <- 5.972E24
EarthRadius <- 6.371E6
EarthVolume <- (EarthRadius^3 * 4 * pi)/3 # in meters^3

# read the file with the names
satname <- read.fwf("one.txt", widths = c(24), header = FALSE)

tle_master <- read.fwf("two.txt", widths = c(1,1,5,1,1,2,3,3,1,2,12,1,10,1,8,1,8,1,1,1,4,1,1,1,5,1,8,1,8,1,7,1,8,1,8,1,11,5,1), header = FALSE, 
                       col.names=c('LineNumber1','','SatelliteNumber1','Classification','','ID1','ID2','ID3','','EpochYear','EpochDay','',
                                    'FirstTimeDerivative','','SecondTimeDerivative','','BSTAR','','Ephemeris','','ElementSetNumber','Checksum1',
                                    'LineNumber2','','SatelliteNumber2','','Inclination','','RightAscension','','Eccentricity','','Perigee','',
                                     'MeanAnomaly','','MeanMotion','Revolution','Checksum2'))
# merge back the names into the TLE
tle_master$satname <- satname$V1
# perform caculations
tle_master$altitude <- ((((1/tle_master$MeanMotion*86400)^2/((2*pi))^2)*G*M)^(1/3)-EarthRadius)
#convert to kilometers
tle_master$altitude <- tle_master$altitude/1000

hist <- hist(tle_master$altitude, breaks = 25, main = paste("Histogram of Number of Satellites Per Band of Orbit"),
     xlab = "Altitude in Kilometers")


# vary the xlim for LEO to GEO or all

ghist <- ggplot(tle_master, aes(x=altitude)) +
      geom_histogram(binwidth = 20, color="green") +
      labs(title="Histogram of Number of Satellites Per Band of Orbit",
           x = "Altitude in bins of 20 Kilometers",
           y = "Object Count") +
           xlim(100, 2200)
ghist

tle_master$mark <- NA
write.csv(tle_master, file = "tle_master.csv")
#write.csv(tle_selection, file = "tle_selection.csv")

# selection area -
# open file that has been marked for analysis, tle_mark.csv has the mark column where we put in JPN by 
# individual entry, 0 is all, 1 is JPN, others?
tle_selection <- read.csv("tle_mark.csv", header=TRUE, sep=",")
# factor the mark column
tle_selection$mark <- factor(tle_selection$mark)
#tle_selection2 <- subset(tle_selection, mark == 1)

ghist <- ggplot(tle_selection$mark == 1, aes(x=altitude)) +
  geom_histogram(binwidth = 20, color="green") +
  labs(title="Histogram of JPN owned Satellites Per Band of Orbit",
       x = "Altitude in bins of 20 Kilometers",
       y = "Object Count") +
  xlim(500, 2000)
ghist
#the xlim for everything is 100, 40,000, customize for home in like LEO
# this one uses hte subset function to parse out the 1 and 0 in 'mark'
ghist <- ggplot(tle_selection, aes(x=altitude)) +
  geom_histogram(data=subset(tle_selection, mark == '1'),fill = "red", binwidth = 20, alpha = 0.9) +
  geom_histogram(data=subset(tle_selection, mark == '0'),fill = "green", binwidth = 20, alpha = 0.2) +
    xlim(35500,36000) +
  labs(title="Histogram of JPN owned Satellites Per all others, Per Band of Orbit",
    x = "Altitude in bins of 20 Kilometers",
    y = "Object Count")+
    theme(legend.position = "right")
ghist
## ideas from ggplot2 book
ghist <- ggplot(tle_selection, aes(x=altitude, fill=mark)) +
  geom_histogram(binwidth = 20, color="green", fill = "blue", position = "identity") +
  facet_grid(mark ~ .) +
  xlim(500, 2000)
ghist
# revalue the 0 and 1 in mark to 'all but' and 'jpn'
tle_selection$mark <- revalue(tle_selection$mark, c("0"= "All Others", "1" = "JPN"))
## other way
ghist <- ggplot(tle_selection, aes(x=altitude, fill=mark)) +
  geom_histogram(position="identity", alpha = 0.4, binwidth = 20) +
  facet_grid(mark ~ ., scales = "free") +
  theme(legend.position = "none") +
  labs(title="Histogram of Japan owned GEO Satellites Per all Others",
       x = "Altitude in bins of 20 Kilometers",
       y = "Object Count")+
  xlim(35000, 37000) 
ghist
############# another way, from tidyverse
forcats::fct_rev(tle_selection$mark)
p <- ghist <- ggplot(tle_selection, aes(x=altitude, fill = mark)) +
  geom_histogram(binwidth = 20, position = mark) + 
  xlim(500, 2000) +
  guides(fill = guide_legend(reverse = TRUE))
p


plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density") +
    xlim(500, 2000)
    
  plt + guides(fill=guide_legend(title=label_column))
}

plot_multi_histogram(tle_selection, 'altitude', 'mark')

#### REFERENCE
#https://space.stackexchange.com/questions/4211/calculate-satellite-coordinates-from-tle-data
#https://stackoverflow.com/questions/6957549/overlaying-histograms-with-ggplot2-in-r
http://www.stltracker.com/resources/tle - explaings tle_master
http://www.golombek.com/ - GEO look angle caculator
http://www.satsig.net/sslist.htm - list of GEO satellites