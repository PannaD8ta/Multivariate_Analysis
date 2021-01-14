##Task 1
##1.1generating AR processes with phi_1=0.2
library(forecast)
ar02 <- arima.sim(list(order = c(1,0,0), ar = 0.2), n = 100)
par(mfrow = c(3, 3))
plot(ar02)
Acf(ar02, main ="")
Pacf(ar02, main ="")
mtext(expression(paste("AR(1) with ", phi[1], "=0.2")), side = 3, line = -2, outer = TRUE)

##1.2generating AR processes with phi_1=0.6
library(forecast)
ar06 <- arima.sim(list(order = c(1,0,0), ar = 0.6), n = 100)

#par(mfrow = c(3, 1))
plot(ar06)
Acf(ar06, main ="")
Pacf(ar06, main ="")
mtext(expression(paste("AR(1) with ", phi[1], "=0.6")), side = 3, line = -2, outer = TRUE)

#1.3generating AR processes with phi_1=0.9
library(forecast)
ar09 <- arima.sim(list(order = c(1,0,0), ar = 0.9), n = 100)
#par(mfrow = c(3, 1))
plot(ar09)
Acf(ar09, main ="")
Pacf(ar09, main ="")
mtext(expression(paste("AR(1) with ", phi[1], "=0.9")), side = 3, line = -2, outer = TRUE)

##2.1generating AR processes with theta_1=0.2
ma02 <- arima.sim(list(order = c(0,0,1), ma = 0.2), n = 100)
par(mfrow = c(3, 3))
plot(ma02)
Acf(ma02, main ="")
Pacf(ma02, main ="")
mtext(expression(paste("MA(1) with ", theta[1], "=0.2")), side = 3, line = -2, outer = TRUE)

##2.2generating AR processes with theta_1=0.2
ma06 <- arima.sim(list(order = c(0,0,1), ma = 0.6), n = 100)
#par(mfrow = c(3, 1))
plot(ma06)
Acf(ma06, main ="")
Pacf(ma06, main ="")
mtext(expression(paste("MA(1) with ", theta[1], "=0.6")), side = 3, line = -2, outer = TRUE)

##2.3generating AR processes with theta_1=0.2
ma06n <- arima.sim(list(order = c(0,0,1), ma = -0.6), n = 100)
#par(mfrow = c(3, 1))
plot(ma06n)
Acf(ma06n, main ="")
Pacf(ma06n, main ="")
mtext(expression(paste("MA(1) with ", theta[1], "=-0.6")), side = 3, line = -2, outer = TRUE)

##Task 2
##1.1Random Walk
rw<-cumsum(rnorm(n=100, mean=0))
par(mfcol = c(2, 3))
plot(rw, type="l", xlab="Time", main ="")
Acf(rw, main ="")
mtext(paste("Random walk model"), side = 3, line = -2, outer = TRUE)

##1.2Random Walk with drift
rwd<-cumsum(rnorm(n=100, mean=0.5))
#par(mfrow = c(2, 1))
plot(rwd, type="l", xlab="Time", main ="")
Acf(rwd, main ="")
mtext(paste("Random walk with drift model"), side = 3, line = -2, outer = TRUE)

##1.3Trend stationary
ts_data <- seq(from = 1, to = 100, by = 1) + 10 + rnorm(100, sd = 2)
#par(mfrow = c(2, 1))
plot(ts_data, type="l", xlab="Time", main ="")
Acf(ts_data, main ="")
mtext(paste("Trend stationary model"), side = 3, line = -2, outer = TRUE)

##2.1First difference of random walk
rw_1d <- diff(rw)
par(mfcol = c(2, 3))
plot(rw_1d, type="l", xlab="Time", main ="")
Acf(rw_1d, main ="")
mtext(paste("First difference of random walk"), side = 3, line = -2, outer = TRUE)

##2.2First difference of random walk with drif
rwd_1d <- diff(rwd)
#par(mfrow = c(2, 1))
plot(rwd_1d, type="l", xlab="Time", main ="")
Acf(rwd_1d, main ="")
mtext(paste("First difference of random walk with drift"), side = 3, line = -2, outer = TRUE)

##2.3First difference of Trend stationary
ts_1d <- diff(ts_data)
#par(mfrow = c(2, 1))
plot(ts_1d, type="l", xlab="Time", main ="")
Acf(ts_1d, main ="")
mtext(paste("First difference of trend stationary"), side = 3, line = -2, outer = TRUE)

##Task 3
##1 adf test on levels
library(tseries)
adf.test(rw)
adf.test(rwd)
adf.test(ts_data)
##1 adf test on 1st dif
adf.test(rw_1d)
adf.test(rwd_1d)
adf.test(ts_1d)
#------------------------

##Task 4
### 1. download
library(quantmod)
getSymbols("USD/EUR",src="oanda")
plot(USDEUR, type="l", xlab="Time", main ="USD/EUR exchange rate")
Acf(USDEUR, main ="USDEUR")
### . 1st difference
USDEUR_1d <- diff(USDEUR)
Acf(USDEUR_1d, main ="USDEUR")
plot(USDEUR_1d, type="l", xlab="Time", main ="USD/EUR exchange rate")
## 3. split 
length(USDEUR)
## first 143 observations - training set
USDEUR[1]
USDEUR[143]
training_set <- USDEUR_1d["2018/2018-09-09"]
length(training_set)
end(USDEUR)
USDEUR[144]
test_set <- USDEUR_1d["2018-09-10/2018-10-14"]
length(test_set)
##4. ACF and PACF
par(mfrow = c(2, 1))
Acf(training_set, main ="")
Pacf(training_set, main ="")
mtext(paste("Change in the USD/EUR exchange rate"), side = 3, line = -2, outer = TRUE)
###5. auto.arima 
fit_USDEUR_1d <- auto.arima(USDEUR); fit_USDEUR_1d
fit_USDEUR_1d <- auto.arima(training_set); fit_USDEUR_1d
##6. fitting ARMA
library(forecast)
fit_ar1ma0 <- Arima(training_set, order=c(1,0,0)); fit_ar1ma0 ##AIC=-1305.02   AICc=-1304.85   BIC=-1296.13
fit_ar0ma1 <- Arima(training_set, order=c(0,0,1)); fit_ar0ma1 ## AIC=-1305.21!   AICc=-1305.04!   BIC=-1296.32!
fit_ar1ma1 <- Arima(training_set, order=c(1,0,1)); fit_ar1ma1##AIC=-1303.38   AICc=-1303.09   BIC=-1291.52
##7. ACFs of residuals
par(mfrow = c(3, 1))
Acf(residuals(fit_ar1ma0), main ="", lag.max = 24)
Acf(residuals(fit_ar0ma1), main ="", lag.max = 24)
Acf(residuals(fit_ar1ma1), main ="", lag.max = 24)
##8. accuracy
# Apply fitted model to test data
ar1ma0.test <- Arima(test_set,model=fit_ar1ma0);ar1ma0.test 
ar0ma1.test <- Arima(test_set,model=fit_ar0ma1);ar0ma1.test
ar1ma1.test <- Arima(test_set,model=fit_ar1ma1);ar1ma1.test 
# out-of-sample one-step forecasts.
accuracy(ar1ma0.test)
accuracy(ar0ma1.test)
accuracy(ar1ma1.test)
#------------------------

##task 5
##1. download
quantmod::getSymbols("^DJI", src = "yahoo", from = "2016-02-11", to="2017-09-11", periodicity = "daily") #"2016-09-11"
plot(DJI$DJI.Adjusted, type="l", xlab="Time", main ="Dow Jones Industrial Average index")
par(mfrow = c(2, 1))
forecast::Acf(DJI$DJI.Adjusted, main ="")
forecast::Pacf(DJI$DJI.Adjusted, main ="")
##2. return
DJI_r <- quantmod::allReturns(DJI$DJI.Adjusted)
DJI_return<- DJI_r$daily[2:nrow(DJI_r)]
plot(DJI_return, type="l", xlab="Time", main ="Returns for Dow Jones Industrial Average index")
## 3. split 
length(DJI_return)
## first 143 observations - training set
DJI_return[317]
training_set <- DJI_return["2016/2017-05-16"]
DJI_return[318]
end(DJI_return)
test_set <- DJI_return["2017-05-17/2017-09-08"]
##4. ACF and PACF
par(mfrow = c(2, 1))
forecast::Acf(training_set, main ="")
forecast::Pacf(training_set, main ="")
mtext(paste("Change in the USD/EUR exchange rate"), side = 3, line = -2, outer = TRUE)
###5. auto.arima 
fit_DJI_return <- forecast::auto.arima(training_set); fit_DJI_return
##6. fitting ARMA
library(forecast)
fit_ar1ma0 <- forecast::Arima(training_set, order=c(1,0,0)); fit_ar1ma0 ##AIC=-2322.08!   AICc=-2322!   BIC=-2310.8!
fit_ar0ma1 <- forecast::Arima(training_set, order=c(0,0,1)); fit_ar0ma1 ## AIC=-2322.04   AICc=-2321.97   BIC=-2310.77
fit_ar1ma1 <- forecast::Arima(training_set, order=c(1,0,1)); fit_ar1ma1##AIC=-2319.75   AICc=-2319.62   BIC=-2304.71
##7. ACFs of residuals
par(mfrow = c(3, 1))
forecast::Acf(residuals(fit_ar1ma0), main ="", lag.max = 24)
forecast::Acf(residuals(fit_ar0ma1), main ="", lag.max = 24)
forecast::Acf(residuals(fit_ar1ma1), main ="", lag.max = 24)
##8. accuracy
# Apply fitted model to test data
ar1ma0.test <- forecast::Arima(test_set,model=fit_ar1ma0);ar1ma0.test 
ar0ma1.test <- forecast::Arima(test_set,model=fit_ar0ma1);ar0ma1.test
ar1ma1.test <- forecast::Arima(test_set,model=fit_ar1ma1);ar1ma1.test 
# out-of-sample one-step forecasts.
forecast::accuracy(ar1ma0.test)
forecast::accuracy(ar0ma1.test)
forecast::accuracy(ar1ma1.test)
#------------------------

