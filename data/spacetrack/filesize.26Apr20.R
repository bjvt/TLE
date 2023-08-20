# 
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle/spacetrack")
# setwd("/media/bvt/913d2c1c-f3d7-4c30-9b83-eca903c169dd/R/ebd_US_relDec-2019")
library(ggplot2)
# read in the data
size_data <- read.csv("Book1.txt")

plot_colors <- c("green", "gray","blue","red","forestgreen")

p <- ggplot(size_data, aes(x=Number)) +
              