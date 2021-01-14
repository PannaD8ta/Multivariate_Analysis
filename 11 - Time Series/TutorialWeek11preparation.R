##https://otexts.org/fpp2/graphics.html Chapter 2 Section 2.10 exercises
install.packages("fpp2")
install.packages("forecast")
##if you would like to see which datasets are available in this package
data(package="fpp2")
data(package="forecast")
library("forecast")

##loading a woolyrnq dataset
data(woolyrnq, package="forecast")
??woolyrnq
frequency(woolyrnq)
seasonplot(woolyrnq)
tsdisplay(woolyrnq)

##loading gold dataset
data(gold, package="forecast")
??gold
frequency(gold)
tsdisplay(gold)

##loading gas dataset
data(qgas, package="fpp2")
??gas
frequency(qgas)
plot(gas)
seasonplot(gas)
tsdisplay(gas)

# set your working directory and save file tute1.csv file in this folder
setwd("/Users/nictsau/Documents/Swinburne University of Technology/Bachelor of Computer Science 2017-2020/S2 - 2019/STA30005 - Multivariate Analysis/Week 11")

# read the data into R
tute1 <- read.csv("tute1.csv", header=TRUE)
mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)

#plot the data
par(mfrow = c(3, 1))
plot(mytimeseries[,1])
plot(mytimeseries[,2])
plot(mytimeseries[,3])

##load more data
retaildata <- readxl::read_excel("retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],frequency=12, start=c(1982,4))
ggseasonplot(myts)
ggsubseriesplot(myts)
gglagplot(myts)
ggAcf(myts)

###more data
#import ANNUAL data
install.packages("Quandl")
library(Quandl)
ausgdp <- ts(rev(Quandl("FRED/AUSRGDPC", type="ts")), end=2011)
plot(ausgdp, main = "Australian GDP", xlab = "Time (years)")

#Why is the time series data being plotted backwards in R?
#https://stackoverflow.com/questions/26507912/why-is-the-time-series-data-being-plotted-backwards-in-r
library(xts)
rev_xts_ausgdp = as.xts(rev(as.xts(ausgdp))) 
plot(rev_xts_ausgdp)

ausgdp_lag3 <- lag(rev_xts_ausgdp,-3)
plot(rev_xts_ausgdp, main = "Australian GDP", lwd=2, xlab = "Time (years)")
lines (ausgdp_lag3, col="blue", lwd=2)
legend(1980,37000, c("Australian GDP", "Australian GDP lagged three years"),col = c("black", "blue"), pch=20, lwd=2)

##lags of the data
install.packages("quantmod")
library(quantmod)
lag1_ausgdp <- Lag(rev_xts_ausgdp, k = 1)
lag2_ausgdp <- Lag(rev_xts_ausgdp, k = 2)
lag3_ausgdp <- Lag(rev_xts_ausgdp, k = 3)

###create differences
diff1_ausgdp <- diff(rev_xts_ausgdp, lag = 1, differences = 1)
diff2_ausgdp <- diff(rev_xts_ausgdp, lag = 1, differences = 2)
ausgdp_diffs <- cbind(lag2_ausgdp, lag1_ausgdp, diff1_ausgdp, lag2_ausgdp,lag1_ausgdp, diff2_ausgdp)
par(mfrow = c(3, 1))
plot(rev_xts_ausgdp)
plot(diff1_ausgdp)
plot(diff2_ausgdp)

###acfs and pacfs
library("forecast")
par(mfrow = c(1, 2))
Acf(rev_xts_ausgdp, lag.max = 36)
Pacf(rev_xts_ausgdp, lag.max = 36)
par(mfrow = c(1, 2))
Acf(diff1_ausgdp, lag.max = 36)
Pacf(diff1_ausgdp, lag.max = 36)

#generate white noise
wn <- rnorm(100, mean = 0, sd = 1)
par(mfrow = c(3, 1))
plot(wn, type="l", main="White Noise")
Acf(wn)
Pacf(wn)
data(arrivals, package="fpp2")
tsdisplay(arrivals)
