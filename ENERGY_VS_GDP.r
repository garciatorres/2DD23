#Exploratory Analysis on Brazil's energy use per capita
brazil <- group15_data_2
brazil$enerbrazil[as.Date(brazil$date,'%Y') == as.Date("2008",'%Y')] <- 1.29654123
enerbrazil <- brazil$enerbrazil
enerbrazil <- na.omit(enerbrazil)
enerbrazil.ts <- ts(enerbrazil, start=c(1971))
tsdisplay(enerbrazil.ts)

#de-trending for spectral analysis
enerbrazil.d2 <- diff(enerbrazil.ts,differences=2) 

#smoothed periodogram
enerbrazil.spec <- spectrum(enerbrazil.d2,log="no",span=5)
enerbrazil.spec

#Exploratory Analysis on Brazil's GDP per capita
gdpbrazilcomplete <- brazil$gdpbrazil
gdpbrazil <- na.omit(gdpbrazilcomplete)
gdpbrazil <- gdpbrazil[-(1:11)]
gdpbrazil.ts <- ts(gdpbrazil, start=c(1971))
tsdisplay(gdpbrazil.ts)

#de-trending for spectral analysis
gdpbrazil.d2 <- diff(gdpbrazil.ts,differences=2)

#smoothed periodogram
gdpbrazil.spec <- spectrum(gdpbrazil.d2,log="no",span=3)
gdpbrazil.spec

#An Univariate Box-Jenkins model for Brazil’s energy use per capita
enerbrazil.d1 <- diff(enerbrazil.ts,differences=1)
tsdisplay(enerbrazil.d1)

#Dicker-Fuller test for 1st differenciation
adf.test(enerbrazil.d1)

enerbrazil.d2 <- diff(enerbrazil.ts,differences=2)
tsdisplay(enerbrazil.d2)

#Dicker-Fuller test for 2nd differenciation
adf.test(enerbrazil.d2)

#Model determination according to AIC values
enerbrazil.arima.110 <- arima(enerbrazil.ts, order=c(1,1,0))

enerbrazil.arima.011 <- arima(enerbrazil.ts, order=c(0,1,1))
enerbrazil.arima.010 <- arima(enerbrazil.ts, order=c(0,1,0))
enerbrazil.arima.111 <- arima(enerbrazil.ts, order=c(1,1,1))
enerbrazil.arima.211 <- arima(enerbrazil.ts, order=c(2,1,1))
enerbrazil.arima.112 <- arima(enerbrazil.ts, order=c(1,1,2))
enerbrazil.arima.212 <- arima(enerbrazil.ts, order=c(2,1,2))
enerbrazil.arima.012 <- arima(enerbrazil.ts, order=c(0,1,2))
enerbrazil.arima.210 <- arima(enerbrazil.ts, order=c(2,1,0))
enerbrazil.arima.020 <- arima(enerbrazil.ts, order=c(0,2,0))
enerbrazil.arima.120 <- arima(enerbrazil.ts, order=c(1,2,0))
enerbrazil.arima.021 <- arima(enerbrazil.ts, order=c(0,2,1))
enerbrazil.arima.121 <- arima(enerbrazil.ts, order=c(1,2,1))

#Forecasts with ARIMA(1,1,0) and ARIMA(1,2,1)
enerbrazil.arima.110.forecast <- forecast.Arima(enerbrazil.arima.110, h=10)
enerbrazil.arima.121.forecast <- forecast.Arima(enerbrazil.arima.121, h=10)

#Validation of model: ARIMA(1,1,0)
residuals.110 <- enerbrazil.arima.110.forecast$residuals
plot.ts(residuals.110) #verify that the residuals look like white noise

#correlogram of the forecast errors
acf(residuals.110)

#test to verify the absence of auto-correlations
Box.test(residuals.110, type="Ljung-Box")

#QQ-Plot to verify normality of the residuals
qqnorm(residuals.110); qqline(residuals.110)

shapiro.test(residuals.110) #test to verify normality of the residuals

#Validation of model: ARIMA(1,2,1)
residuals.121 <- enerbrazil.arima.121.forecast$residuals 
plot.ts(residuals.121) #verify that the residuals look like white noise

acf(residuals.121) #correlogram of the forecast errors

#test to verify the absence of auto-correlations
Box.test(residuals.121, type="Ljung-Box")

#QQ-Plot to verify normality of the residuals
qqnorm(residuals.121); qqline(residuals.121)

#test to verify normality of the residuals
shapiro.test(residuals.121)

#Compare ARIMA(1,1,0) with ARIMA(1,2,1) based on the RMSE (the smaller, the better)
accuracy(enerbrazil.arima.110)

accuracy(enerbrazil.arima.121)

#Forecast with ARIMA(1,1,0)
plot.forecast(enerbrazil.arima.110.forecast)
enerbrazil.arima.110.forecast

#Forecast with ARIMA(1,2,1)
enerbrazil.arima.121.forecast
plot.forecast(enerbrazil.arima.121.forecast)

#Dynamic Regression Model: fit the regression model with AR(2) errors
enerbrazil.proxy.210 <- Arima(enerbrazil.ts, xreg=gdpbrazil.ts, order=c(2,1,0))
enerbrazil.proxy.210
tsdisplay(residuals(enerbrazil.proxy.210))
