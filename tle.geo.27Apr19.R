

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle")
#setwd("C:/Users/bvt/Dropbox/eng_science/R/wd/tle")
# take the 3 line element file and make two files, 1st one is Satellite Name, 2nd is the modified TLE ready for R
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
library(qdap)
library(tidyr)
library(dplyr) # part of the tidyverse modules


# read the file with the names
geo_master <- read.csv("geo.csv", sep = ",", header = TRUE, na.strings = " ")

#wgs10 <- xyz2lla(16500, 41800, 55.3) #WGS-10
#jcsat13 <- xyz2lla(34006.5217, 24912.72847, 9.5569) # JSCAT-13
# beidou9 <- xyz2lla(17571.055, -20598.1727, 32453.38863)
### this section merges
sat_master <- read.csv("SATCAT_02052019.csv", sep = ",", header = TRUE, na.strings = " ")
geo_master <- merge.data.frame(geo_master, sat_master, by="ID")

##### This section converts the ECEF to Lat/Long/Altitude(km)
lla_df <- data.frame()
for(i in 1:nrow(geo_master))
{
lla <- xyz2lla(geo_master$x[i], geo_master$y[i], geo_master$z[i])
# transpose and make the lla function output from a list to a dataframe
lla <- t(as.data.frame(lla))
# append the lla[i] to the dataframe
lla_df <- rbind(lla_df, lla) 


# print(i)
# print(lla)
}
# give the column the names, and add the 'mark' column
colnames(lla_df) <- c("lat","long","alt") 
lla_df$mark <- NA
lla_df$ID <- geo_master$ID
lla_df$Name <- geo_master$Name
# reorder the columns for easier reading
lla_df <- lla_df[c(5,6,4,1,2,3)]

write.csv(lla_df, file = "lla_df.csv")
########                         ##########


hist <- hist(lla_df$alt, breaks = 100, main = paste("Histogram of Number of Satellites Per Band of Orbit"),
     xlab = "Altitude in Kilometers")


# vary the xlim for LEO to GEO or all
# lla_df$alt <- factor(lla_df$alt)
ghist <- ggplot(lla_df, aes(x=alt)) +
      geom_histogram(binwidth = 5, color="green") +
      labs(title="Histogram of Number of GEO Satellites Per Band of Orbit",
           x = "Altitude in bins of 5 Kilometers",
           y = "Object Count") +
           xlim(35500, 36000)
ghist
# logitude one
ghist <- ggplot(lla_df, aes(x=long)) +
  geom_histogram(binwidth = 2, color="green") +
  labs(title="Histogram of Number of GEO Satellites Per Orbital Slot",
       x = "Longitude in bins of 2 degrees",
       y = "Object Count") +
       xlim(0, 360)
ghist


#write.csv(tle_selection, file = "tle_selection.csv")

# selection area -
# open file that has been marked for analysis, tle_mark.csv has the mark column where we put in JPN by 
# individual entry, 0 is all, 1 is JPN, others?
geo_select <- read.csv("geo_mark.csv", header=TRUE, sep=",")
# factor the mark column
geo_select$mark <- factor(geo_select$mark)
#tle_selection2 <- subset(tle_selection, mark == 1)

ghist <- ggplot(geo_select$mark == 1, aes(x=long)) +
  geom_histogram(binwidth = 2, color="green") +
  labs(title="Histogram of JPN owned Satellites Per Band of Orbit",
       x = "Altitude in bins of 20 Kilometers",
       y = "Object Count") +
  xlim(0, 360)
ghist
#the xlim for everything is 100, 40,000, customize for home in like LEO
# this one uses hte subset function to parse out the 1 and 0 in 'mark'
ghist <- ggplot(geo_select, aes(x=long)) +
  geom_histogram(data=subset(geo_select$mark == '1'),fill = "red", binwidth = 20, alpha = 0.9) +
  geom_histogram(data=subset(geo_select, mark == '0'),fill = "green", binwidth = 20, alpha = 0.2) +
    xlim(0,360) +
  labs(title="Histogram of JPN owned Satellites Per all others, Per Band of Orbit",
    x = "Altitude in bins of 20 Kilometers",
    y = "Object Count")+
    theme(legend.position = "right")
ghist
## ideas from ggplot2 book
ghist <- ggplot(geo_select, aes(x=long, fill=mark)) +
  geom_histogram(binwidth = 22, color="green", fill = "blue", position = "identity") +
  facet_grid(mark ~ .) +
  xlim(0, 360)
ghist
# revalue the 0 and 1 in mark to 'all but' and 'jpn'
geo_select$mark <- revalue(geo_select$mark, c("0"= "All Others", "1" = "JPN"))
## other way
ghist <- ggplot(geo_select, aes(x=long, fill=mark)) +
  geom_histogram(position="identity", alpha = 0.6, binwidth = 2) +
  facet_grid(mark ~ ., scales = "free") +
  theme(legend.position = "none") +
  labs(title="Histogram of Japan owned GEO Satellites Per all Others",
       x = "Longitude in bins of 2 degrees",
       y = "Object Count")+
  xlim(0, 360) 
ghist
############# another way, from tidyverse
forcats::fct_rev(geo_select$mark)
p <- ghist <- ggplot(geo_select, aes(x=long, fill = mark)) +
  geom_histogram(binwidth = 2, position = mark) + 
  xlim(0, 360) +
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

plot_multi_histogram(geo_select, 'long', 'mark')



#### REFERENCE
#https://space.stackexchange.com/questions/4211/calculate-satellite-coordinates-from-tle-data
#https://stackoverflow.com/questions/6957549/overlaying-histograms-with-ggplot2-in-r
http://www.stltracker.com/resources/tle - explaings tle_master
http://www.golombek.com/ - GEO look angle caculator
http://www.satsig.net/sslist.htm - list of GEO satellites