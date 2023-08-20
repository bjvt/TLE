

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle/cdm")
#setwd("C:/Users/bvt/Dropbox/eng_science/R/wd/tle")
# https://www.space-track.org/basicspacedata/query/class/cdm_public/orderby/CDM_ID asc/format/csv/emptyresult/show
# https://www.space-track.org/basicspacedata/query/class/gp/EPOCH/%3Enow-30/orderby/NORAD_CAT_ID,EPOCH/format/csv
# https://www.guru99.com/r-for-loop.html
# https://www.r-bloggers.com/2015/08/how-to-use-lists-in-r/

#Load required packages
#library(gridExtra)
#library(moments) # needed for skewness
library(ggplot2)
library(tidyverse)
library(plyr)
library(forcats)
library(rbin)

library(dplyr) # part of the tidyverse modules

G <- 6.67408E-11
M <- 5.972E24
EarthRadius <- 6.371E6
EarthVolume <- (EarthRadius^3 * 4 * pi)/3 # in meters^3

# read the two csv files

cdm_public <- read.csv("cdm_public.07Jan21.csv")
gp_catalog <- read.csv("gp_catalog.07Jan21.csv",fill = TRUE, na.strings = "NA")

gp_catalog$altitude <- (((((1/gp_catalog$MEAN_MOTION*86400)^2/((2*pi))^2)*G*M)^(1/3)-EarthRadius))/1000
alt.bin <- seq(100, 2200, 100)
#gp_catalog$altitude <- as.integer(gp_catalog$altitude)
gp_catalog$count <- gp_catalog
gp_density <- table(cut(gp_catalog$altitude, seq(100, 2200, 20), labels = FALSE))
#gp_catalog <- as_tibble(gp_catalog)
bins <- rbin_manual(gp_catalog, count, altitude, c(100,200, 2000))
mbank_bins <- rbin_manual(mbank, y, age, c(29, 31, 34, 36, 39, 42, 46, 51, 56))

# vary the xlim for LEO to GEO or all
hist <- hist(gp_catalog$altitude, breaks = 100, plot = False)

#

ghist <- ggplot(gp_catalog, aes(x=altitude)) +
      geom_histogram(binwidth = 25, color="green") +
      labs(title="Histogram of Number of Satellites Per Band of Orbit",
           x = "Altitude in bins of 100 Kilometers",
           y = "Object Count") +
           xlim(100, 2200)
ghist
# create a column calle 'mark' that will help sort out specific assets
tle_master$mark <- NA
write.csv(tle_master, file = "tle_master.2Oct19.csv")
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

# this one gives us Density
  
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

