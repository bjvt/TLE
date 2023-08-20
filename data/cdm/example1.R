# Setting the working directory
# or else comment out the setwd and set it by selecting 'session/'set working directory'
setwd("/home/bvt/Dropbox/eng_science/R/wd/tle/cdm")
# Goal is to take a column of data, bin it in assigned bins, then multiply the 
# number (or count) in the bin times the bin width.  Real data consists of Low 
# Earth Orbit satellites -> # of satellites divided by ([bin1, bin1) times 
# hollow sphere equation)
df <- data.frame(altitude = c(10, 21.2, 11.5, 32, 22, 15.7, 37, 25, 19.7))

hist1 <- hist(df$altitude, freq = TRUE, seq(10, 40, 10))

test1 <- length()

df_hist1 <- as.data.frame(hist1)

https://stackoverflow.com/questions/24359863/binning-data-in-r
https://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation
https://stackoverflow.com/questions/51688403/r-count-function
https://duckduckgo.com/?q=count+the+number+of+values+in+each+bin+of+histogram+in+r&t=newext&atb=v250-1&ia=web
https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/hist