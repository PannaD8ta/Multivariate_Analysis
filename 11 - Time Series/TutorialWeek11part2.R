#------------------------

##Task 1
### 1. download
library(Quandl)
alum_price <- Quandl("LME/PR_AA.1", start_date="2012-01-03", type="xts")
plot(alum_price, type="l", xlab="Time", main ="Aluminium Alloy Price, US$ per tonne")
par(mfrow = c(2, 1))
forecast::Acf(alum_price, main ="")
forecast::Pacf(alum_price, main ="")
##2. 1st dif
alprice_1d <- diff(alum_price)
plot(alprice_1d)
fit_alprice_1d <- forecast::auto.arima(alprice_1d); fit_alprice_1d
par(mfrow = c(2, 1))
forecast::Acf(alprice_1d, main ="")
forecast::Pacf(alprice_1d, main ="")
##adf test
library(tseries)
adf.test(alum_price)
adf.test(alprice_1d[-1,])
## 3. split 
## first 143 observations - training set
b <- round(0.8*length(alprice_1d))
alprice_1d[b]
training_set <- alprice_1d["2012-01-04/2017-05-31"]
hist(alprice_1d)
end(alprice_1d)
test_set <- alprice_1d["2017-06-01/2018-10-18"]
##4. ACF and PACF
par(mfrow = c(2, 1))
forecast::Acf(training_set, main ="")
forecast::Pacf(training_set, main ="")
mtext(paste("Change in Aluminum Alloy Price, US$ per tonne"), side = 3, line = -2, outer = TRUE)
###5. auto.arima 
#fit_alprice <- forecast::auto.arima(alum_price); fit_alprice
fit_alprice_1d <- forecast::auto.arima(training_set); fit_alprice_1d
##6. fitting ARMA
library(forecast)
fit_ar0ma1 <- forecast::Arima(training_set, order=c(0,0,1)); fit_ar0ma1#AIC=10390.44!   AICc=10390.46   BIC=10405.6!
str(fit_ar0ma1)
res_ar0ma1 <- as.data.frame(fit_ar0ma1$residuals)
fit_ar2ma0 <- forecast::Arima(training_set, order=c(2,0,0)); fit_ar2ma0 ##AIC=10391.18   AICc=10391.22!   BIC=10411.39!
fit_ar1ma1 <- forecast::Arima(training_set, order=c(1,0,1)); fit_ar1ma1 ## AIC=10391.91   AICc=10391.94   BIC=10412.12
fit_ar0ma2 <- forecast::Arima(training_set, order=c(0,0,2)); fit_ar0ma2##AIC=10391.81   AICc=10391.84   BIC=10412.02
##7. ACFs of residuals
par(mfrow = c(2, 2))
forecast::Acf(residuals(fit_ar0ma1), main ="", lag.max = 24)
forecast::Acf(residuals(fit_ar2ma0), main ="", lag.max = 24)
forecast::Acf(residuals(fit_ar1ma1), main ="", lag.max = 24)
forecast::Acf(residuals(fit_ar0ma2), main ="", lag.max = 24)
##8. accuracy
# Apply fitted model to test data
ar0ma1.test <- forecast::Arima(test_set,model=fit_ar0ma1);ar0ma1.test
ar2ma0.test <- forecast::Arima(test_set,model=fit_ar2ma0);ar2ma0.test
#forecast::forecast(fit_ar2ma0, h=10)
ar1ma1.test <- forecast::Arima(test_set,model=fit_ar1ma1);ar1ma1.test
ar0ma2.test <- forecast::Arima(test_set,model=fit_ar0ma2);ar0ma2.test 
#forecast_ar0ma2 <- as.data.frame(forecast::forecast(fit_ar0ma2, h=10)$mean)
# out-of-sample one-step forecasts.

forecast::accuracy(ar2ma0.test)
forecast::accuracy(ar1ma1.test)
forecast::accuracy(ar0ma2.test)
forecast::accuracy(ar0ma1.test)

#------------------------

##task 2   NIKKEI/VLTL
##1. download
st_mci <- Quandl("NASDAQOMX/OMXSMCGI.1", start_date="2008-11-17", type="xts") 
plot(st_mci, type="l", xlab="Time", main ="Stockholm Mid Cap Index")
par(mfrow = c(2, 1))
forecast::Acf(st_mci, main ="")
forecast::Pacf(st_mci, main ="")
##2. return
st_mci_r <- quantmod::allReturns(st_mci)
st_mci_return<- st_mci_r$daily[2:nrow(st_mci_r)]
plot(st_mci_return, type="l", xlab="Time", main ="Returns for Stockholm Mid Cap Index")
## 3. split 
b <- round(0.8*length(st_mci_return));b
## first 143 observations - training set
st_mci_return[b]
training_set <- st_mci_return["2008/2016-01-11"]
st_mci_return[1772]
end(st_mci_return)
test_set <- st_mci_return["2016-01-12/2017-10-10"]
##4. ACF and PACF
par(mfrow = c(2, 1))
forecast::Acf(training_set, main ="")
forecast::Pacf(training_set, main ="")
mtext(paste("Returns for Stockholm Mid Cap Indexe"), side = 3, line = -2, outer = TRUE)
###5. auto.arima 
fit_st_mci_return <- forecast::auto.arima(training_set); fit_st_mci_return
##6. fitting ARMA
library(forecast)
fit_ar1ma0 <- forecast::Arima(training_set, order=c(1,0,0)); fit_ar1ma0 ##AIC=-11120.29   AICc=-11120.27   BIC=-11103.85
fit_ar3ma1 <- forecast::Arima(training_set, order=c(3,0,1)); fit_ar3ma1 ## AIC=-11121.38!   AICc=-11121.34   BIC=-11088.51!
fit_ar3ma2 <- forecast::Arima(training_set, order=c(3,0,2)); fit_ar3ma2##AIC=-11117.34   AICc=-11117.28!   BIC=-11078.99!
fit_ar2ma3 <- forecast::Arima(training_set, order=c(2,0,3)); fit_ar2ma3##AIC=-11120.42   AICc=-11120.36   BIC=-11082.06
##7. ACFs of residuals
par(mfrow = c(4, 1))
forecast::Acf(residuals(fit_ar1ma0), main ="1,0", lag.max = 24)
forecast::Acf(residuals(fit_ar3ma1), main ="3,1", lag.max = 24)
forecast::Acf(residuals(fit_ar3ma2), main ="3,2", lag.max = 24)
forecast::Acf(residuals(fit_ar2ma3), main ="2,3", lag.max = 24)
##8. accuracy
# Apply fitted model to test data
ar3ma1.test <- forecast::Arima(test_set,model=fit_ar3ma1);ar3ma1.test 
ar3ma2.test <- forecast::Arima(test_set,model=fit_ar3ma2);ar3ma2.test
ar2ma3.test <- forecast::Arima(test_set,model=fit_ar2ma3);ar2ma3.test 
# out-of-sample one-step forecasts.
forecast::accuracy(ar3ma1.test)
forecast::accuracy(ar3ma2.test)
forecast::accuracy(ar2ma3.test)
#------------------------

