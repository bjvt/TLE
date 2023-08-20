

# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle/cdm")
#setwd("C:/Users/bvt/Dropbox/eng_science/R/wd/tle")
# https://www.space-track.org/basicspacedata/query/class/cdm_public/orderby/CDM_ID asc/format/csv/emptyresult/show
# https://www.space-track.org/basicspacedata/query/class/gp/EPOCH/%3Enow-30/orderby/NORAD_CAT_ID,EPOCH/format/csv
# https://www.guru99.com/r-for-loop.html
# https://www.r-bloggers.com/2015/08/how-to-use-lists-in-r/
###-----------###
options(scipen=999)
#Load required packages
#library(gridExtra)
#library(moments) # needed for skewness
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)
library(forcats)
# library(rbin)

library(dplyr) # part of the tidyverse modules

G <- 6.67408E-11
M <- 5.972E24
EarthRadius <- 6.371E6 # in meters
EarthVolume <- (EarthRadius^3 * 4 * pi)/3 # in meters^3

# read the two csv files that are manually downloaded from space-track

cdm_public <- read.csv("cdm_public.07Jan21.csv")
gp_catalog <- read.csv("gp_catalog.07Jan21.csv",fill = TRUE, na.strings = "NA")
# using the MEAN_MOTION from the gp, calculate the object altitude.  In orbital 
# mechanics, mean motion is the angular speed required for a body to complete 
# one orbit, assuming constant speed in a circular orbit which completes in the 
# same time as the variable speed, elliptical orbit of the actual body.Wikipedia
gp_catalog$altitude <- (((((1/gp_catalog$MEAN_MOTION*86400)^2/((2*pi))^2)
                          *G*M)^(1/3)-EarthRadius))/1000
# only show LEO Objects in new df - gp_leo, note the weird comma needed
gp_leo <- gp_catalog[gp_catalog$altitude < 2201,]
#gp_catalog$altitude <- as.numeric(gp_catalog$altitude)
hist <- hist(gp_leo$altitude, breaks = 200)
# calculate the volumes per bin, create a new dataframe, note that 'counts' 
# need to add 1
df_volume <- data.frame(breaks = c(hist[["breaks"]]), 
                        counts = c(hist[["counts"]],1),
                        mids = c(hist[["mids"]],1), 
                        alt.density = c(hist[["density"]],1))
# now that we know the bin size, put that column into the df_leo
gp_leo$bins <- cut(gp_leo$altitude, breaks = df_volume$breaks)

# calculate the hollow sphere of each histogram bin
for(i in 2:nrow(df_volume))
{
df_volume$volume[i] <- 4.1888*((df_volume$breaks[i]^3) - df_volume$breaks[i-1]^3)
}
# Calculate and add $vol_density, object count per volume, per cubic kilometer
df_volume$vol.density <- df_volume$counts / df_volume$volume
# vary the xlim for LEO to GEO or all

ghist <- ggplot(gp_leo, aes(x=altitude)) +
      geom_histogram(binwidth = 20, color="green", aes(y=..density..)) +
      geom_line(stat = "density")
      labs(title="Histogram of Number of Satellites Per Band of Orbit",
           x = "Altitude in bins of 100 Kilometers",
           y = "Object Count") +
           xlim(100, 2200)
ghist

gvol <- ggplot(df_volume, aes(x=mids, vol.density)) +
  #geom_histogram(binwidth = 20, color="green", aes(y=..density..)) +
  #geom_line(stat = "density")
  geom_point(alpha=1/1) + 
labs(title="Object Density per Band of Orbit",
     x = "Middle of Bin in Kilometers",
     y = "Object Count / Hollow Sphere Volume") +
  xlim(100, 2200)

gvol
# merge the catalog and cdm keying on the ,
# Average CDM Probability of Collision per cdm_pubic download
# to to caculate the mean of same objects but different ids
# https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group
# form-split-apply-combine
df_cdm <- aggregate(cdm_public$PC, list(cdm_public$SAT_1_ID), mean)

df_merge <- merge(df_cdm, gp_leo, by.x = "Group.1", by.y = "NORAD_CAT_ID")
hist_merge <- hist(df_merge$altitude, breaks = 200)
# this one for the presentation
ghist <- ggplot(df_merge, aes(x=altitude)) +
  geom_histogram(binwidth = 20, color="green", aes(y=..density..)) +
  geom_line(stat = "density")
labs(title="Histogram of Probability of Collision Per Band of Orbit",
     x = "Altitude in bins of 100 Kilometers",
     y = "Object Count") +
  xlim(100, 2200)
ghist
#
merged.aor.1 <- merge(df.aor, df.country.list, by.x = "COUNTRY_CODE", by.y = "SpaDoc.Code", 
                      sort = F, all.x = T)
# reduce the number of columns by explicitly adding what columns you want
merged.aor.select.1 <- merged.aor.1[ , c("COUNTRY_CODE","English.ISO","OBJECT_ID","Alpha.3.Code","OBJECT_NAME",
                                         "INCLINATION", "PERIOD","SITE", "orbit","RA_OF_ASC_NODE")]

write.csv(merged.aor.select.1, file = 'merged.geo.select.1.csv')
#############end##########
#to do
# using factors show the density debris vs active
# in the CDM 'ID' is the 'NORAD_CAT_ID
  
gp_count <- tapply(gp_catalog$altitude, cut(gp_catalog$altitude, alt.bin), sum)
gp_count <- as.integer(gp_count)
gp_count

ggplot(gp_catalog, aes(x=altitude))+ geom_histogram(binwidth = 20) +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
  xlim(100,2200)+
  stat_bin(aes(y=..count..))

count <- length(which(gp_catalog$altitude > 500))
                  

#####################################
# create a column called 'mark' that will help sort out specific assets
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
# http://www.stltracker.com/resources/tle - explaings tle_master
# http://www.golombek.com/ - GEO look angle caculator
# http://www.satsig.net/sslist.htm - list of GEO satellites
set.seed(15)
dd<-data.frame(x=rnorm(100,5,2))
ggplot(dd, aes(x=x))+ geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5) 
