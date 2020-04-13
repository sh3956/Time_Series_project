library(Hmisc)
library("varhandle")
library(timeSeries) 
library(lubridate)
library(zoo)
library(dynlm)
library(forecast)
library(dplyr)
library(vars)
##### 29year sp & gdp #########
#read in the csv file
gdp29 <- read.csv('gdp29y.csv', header = TRUE, sep = ",")
sp29 <- read.csv('sp29y.csv', header = TRUE, sep = ",")

#pick the every close index of SP500 
sp29_close <- sp29[c(1,5)]
colnames(sp29_close) <- c("DATE","SP500")

#data set using for the project
data <- merge(gdp29,sp29_close)
colnames(data)<- c("DATE","GDP","SP500")
data$DATE = as.Date(data$DATE,"%Y-%m-%d")

# time series data from orginal datasets
ts_GDP <- ts(data$GDP,1990,2019,frequency = 4)
ts_SP500 <- ts(data$SP500,1990,2019,frequency = 4)

#(a) Produce a time-series plot of your data including the respective ACF and PACF plots.
quartz("ts_GDP")
tsdisplay(ts_GDP)#AR(1)
quartz("ts_SP500")
tsdisplay(ts_SP500)#AR(1)
#actually both plot is strong AR……

#(b) Fit a model that includes, trend, seasonality and cyclical components. Make sure to discuss your model in detail.
#Alex: I use auto.arima here, we might need to "pretend" have tried a lot of models:)

## fit with auto.arima
fit_GDP <- auto.arima(ts_GDP)
fit_GDP#ARIMA(2,1,0)??? with drift
quartz()
plot(x = data$DATE, y = ts_GDP, xlab = "Date(in year)", ylab = "GDP(in billion dollar)", main = "GDP", type = "l")
lines(x = data$DATE, y = fit_GDP$fitted, col = "blue")

#Emma: The autoarima process is suspect. I prefer using arima(1,1,0).
# fit_SP500 <- auto.arima(ts_SP500)
# fit_SP500 #ARIMA(0,1,0) with no AR???
# quartz()
# plot(x = data$DATE, y = ts_SP500, xlab = "Date(in year)", ylab = "SP500 index(in dollar)", main = "SP500", type = "l")
# lines(x = data$DATE, y = fit_SP500$fitted, col = "blue")

#I...personally think this one makes slightly better prediction, at least intuitively from the graph
fit_SP500_1 <- Arima(ts_SP500, order = c(1, 1, 0))
quartz()
plot(data$DATE, ts_SP500, xlab = "Date(in year)", ylab = "SP500 index(in dollar)", main = "SP500", type = "l")
lines(data$DATE,fit_SP500_1$fitted, col = "red")

#(c) Plot the respective residuals vs. fitted values and discuss your observations.
#for GDP:
quartz("e_GDP")
#transform it into vector to remove the quarterly frequency will remove horrible data notation
plot(x = c(fit_GDP$fitted), y = c(fit_GDP$residuals), type = "p", pch = 20, main = "fitted value vs. residual(GDP)",xlab = "Fitted Value", ylab = "Residuals")
abline(a = 0, b = 0)

#for SP500:
quartz("e_SP500") # fit with arima(1,1,0)
plot(x = c(fit_SP500_1$fitted), y = c(fit_SP500_1$residuals), type = "p", pch = 20, main = "fitted value vs. residual(SP500)",xlab = "Fitted Value", ylab = "Residuals")
abline(a = 0, b = 0)

#(d) Plot the ACF and PACF of the respective residuals and interpret the plots.
quartz("tsdisplay for GDP residuals")
par(mfcol=c(1,2))
Acf(fit_GDP$residuals, main = "ACF for GDP residuals")
Pacf(fit_GDP$residuals, main = "PACF for GDP residuals")
tsdisplay(fit_GDP$residuals)

quartz("tsdisplay for SP500 residuals")
par(mfcol=c(1,2))
Acf(fit_SP500_1$residuals, main = "ACF for SP500 residuals")
Pacf(fit_SP500_1$residuals, main = "PACF for SP500 residuals")
tsdisplay(fit_SP500_1$residuals)

#(e) Plot the respective CUSUM and interpret the plot.
quartz("CUSUM-GDP")
plot(efp(fit_GDP$residuals ~ 1, type = "Rec-CUSUM"), main = "GDP CUSUM")

quartz("CUSUM-SP500")
plot(efp(fit_SP500_1$residuals ~ 1, type = "Rec-CUSUM"), main = "SP500 CUSUM")

#(f) Plot the respective Recursive Residuals and interpret the plot.
recursive_GDP <- recresid(fit_GDP$res ~ 1)
x_116 <- seq(1990, 2019, length = 116)
quartz("Recursive Residuals_GDP")
plot(x_116, recursive_GDP, pch = 16, xlab = "year", ylab = "Recursive Residuals", main = "Recursive Residuals(GDP)")
abline(a = 0, b = 0)

recursive_SP500 <- recresid(fit_SP500_1$res ~ 1)
quartz("Recursive Residuals_GDP")
plot(x_116, recursive_SP500, pch = 16, xlab = "year", ylab = "Recursive Residuals", main = "Recursive Residuals(SP500)")
abline(a = 0, b = 0)

#(g) For your model, discuss the associated diagnostic statistics.
summary(fit_GDP)
summary(fit_SP500_1)


#(h) Use your model to forecast 12-steps ahead. Your forecast should include the respective error bands.
quartz()
plot(forecast(fit_GDP, h = 12), main = "GDP Forecast with ARIMA(2,1,0)", xlab = "Date(in year)", ylab = "GDP(in billion dollar)")
quartz()
plot(forecast(fit_SP500_1, h = 12), main = "SP500 Forecast with ARIMA(1,1,0)", xlab = "Date(in year)", ylab = "SP500(in dollar)")#...this ARIMA(1,1,0) behaves weird

#(i) Fit an appropriate VAR model using your two variables. Make sure to show the relevant plots and discuss your results from the fit.
# z-score
GDP_z <- (data$GDP - mean(data$GDP)) / sd(data$GDP)
SP500_z <- (data$SP500 - mean(data$SP500)) / sd(data$SP500)
data_z<- data_frame(GDP_z,SP500_z)
data_z <- ts(data_z, 1990, 2019, frequency = 4) # convert to time series

VARselect(data_z) # order = 3
var_model <-VAR(data_z, p = 3)
summary(var_model)

# construct x-axis for this data set
x_ts <- seq(1990, 2019, length = length(var_model$varresult$GDP_z$fitted.values))
# plot VAR fitted values
quartz()
par(mfcol = c(2, 1))
plot(x_ts, var_model$varresult$GDP_z$fitted.values, type = 'l', lwd = 2, main = "VAR fitted of zscore(GDP)", xlab = "year", ylab = "zscore value")
lines(x_ts, data_z[4:117,1], col = "blue", lwd = 1.5)
legend("topleft",pch= c("-","-"),legend=c("Original Data", "Var Fitted"),col=c("black", "blue"),  cex=0.8)
plot(x_ts, var_model$varresult$SP500_z$fitted.values, type = 'l', lwd = 2, main = "VAR fitted of zscore(SP500)", xlab = "year", ylab = "zscore value")
lines(x_ts, data_z[4:117,2], col = "blue", lwd = 1.5)
legend("topleft",pch= c("-","-"),legend=c("Original Data", "Var Fitted"),col=c("black", "blue"),  cex=0.8)
# plot VAR fitted values(after transfering zscore to true value)
quartz()
par(mfcol = c(2, 1))
plot(x_ts, var_model$varresult$GDP_z$fitted.values * sd(ts_GDP) + mean(ts_GDP), type = 'l', lwd = 2, main = "VAR fitted of true value(GDP)", xlab = "year", ylab = "billion dollar")
lines(x_ts, data[4:117,2], col = "blue", lwd = 1.5)
legend("topleft",pch= c("-","-"),legend=c("Original Data", "Var Fitted"),col=c("black", "blue"),  cex=0.8)
plot(x_ts, var_model$varresult$SP500_z$fitted.values* sd(ts_SP500) + mean(ts_SP500), type = 'l', lwd = 2, main = "VAR fitted of true value(SP500)", xlab = "year", ylab = "dollar")
lines(x_ts, data[4:117,3], col = "blue", lwd = 1.5)
legend("topleft",pch= c("-","-"),legend=c("Original Data", "Var Fitted"),col=c("black", "blue"),  cex=0.8)

# Look at ACF and PACf
quartz()
tsdisplay(residuals(var_model)[,1], main = "GDP = GDP(t-k) + SP500(t-k)")
quartz()
tsdisplay(residuals(var_model)[,2], main ="SP500 = GDP(t-k) + SP500(t-k)")

#(j) Compute, plot, and interpret the respective impulse response functions.
irf(var_model)
quartz()
plot(irf(var_model, n.ahead = 24))#this n.ahead need to be modified then


#(k) Perform a Granger-Causality test on your variables and discuss your results from the test.
grangertest(SP500_z,GDP_z, order = 3) # not significant
grangertest(GDP_z,SP500_z, order = 3) # significant
#Granger test kind of accept that change in GDP is a cause of change in SP500

#(l) Use your VAR model to forecast 12-steps ahead. Your forecast should include the respective error bands. 
#Comment on the differences between the two forecasts (VAR vs. ARIMA).
var.predict <- predict(object = var_model, n.ahead = 24)

# construct a dataframe that contains fitted value + predicted value
var_fullgdp <- c(var_model$varresult$GDP_z$fitted.values, var.predict$fcst$GDP_z[,1])
var_fullsp500 <- c(var_model$varresult$SP500_z$fitted.values, var.predict$fcst$SP500_z[,1])
var_fullgdp <- var_fullgdp * sd(ts_GDP) + mean(ts_GDP)
var_fullsp500 <- var_fullsp500 * sd(ts_SP500) + mean(ts_SP500)
var_full <- data.frame(var_fullgdp, var_fullsp500)

# forecast zscore
quartz()
forecast(var_model) %>% 
  autoplot() + 
  xlab("year") + 
  ggtitle("z-score forecast by VAR")
#at least it behaves better in SP500

# plot var model fitted value + prediction compared to real data
xfull_ts <- seq(1990, 2025, length = length(var_fullgdp))
xfit_ts <- seq(1990, 2019, length = length(data$GDP))
xpre_ts <- seq(2019, 2025, length = 24)

quartz()
par(mfcol = c(2, 1))
plot(xfull_ts, var_fullgdp, type = 'l', lwd = 2, main = "VAR fitted of true value(GDP)", xlab = "year", ylab = "billion dollar")
lines(xfit_ts, data[1:117,2], col = "blue", lwd = 1.5)
lines(xfull_ts[115:138], var.predict$fcst$GDP_z[,2] * sd(ts_GDP) + mean(ts_GDP), col = "red", lty = 2)
lines(xfull_ts[115:138], var.predict$fcst$GDP_z[,3] * sd(ts_GDP) + mean(ts_GDP), col = "red", lty = 2)
legend("topleft",pch= c("-","-","-"),legend=c("Original Data", "Var Fitted", "Error Bound"),col=c("black", "blue", "Red"),  cex=0.8)

plot(xfull_ts, var_fullsp500, type = 'l', lwd = 2, main = "VAR fitted of true value(SP500)", xlab = "year", ylab = "dollar")
lines(xfit_ts, data[1:117,3], col = "blue", lwd = 1.5)
lines(xfull_ts[115:138], var.predict$fcst$SP500_z[,2] * sd(ts_SP500) + mean(ts_SP500), col = "red", lty = 2) 
lines(xfull_ts[115:138], var.predict$fcst$SP500_z [,3] * sd(ts_SP500) + mean(ts_SP500), col = "red", lty = 2)
legend("topleft",pch= c("-","-","-"),legend=c("Original Data", "Var Fitted", "Error Bound"),col=c("black", "blue", "Red"),  cex=0.8)

## forecast with arima
quartz()
plot(forecast(fit_GDP,h=24),shadecols="oldstyle")
quartz()
plot(forecast(fit_SP500_1,h=24),shadecols="oldstyle")

## forecast with ETS
quartz()
ts_GDP %>% forecast(h = 24) %>%
  autoplot() +
  xlab("year") +
  ylab("GDP(in billion dollar)")+
  ggtitle('Forecast GDP with ETS') +
  theme(text = element_text(family = "STHeiti"))+
  theme(plot.title = element_text(hjust = 0.5))

quartz()
ts_SP500 %>% forecast(h = 24) %>%
  autoplot() +
  xlab("year") +
  ylab("SP500 (in dollar)")+
  ggtitle('Forecast SP500 with ETS') +
  theme(text = element_text(family = "STHeiti"))+
  theme(plot.title = element_text(hjust = 0.5))




##*************************************************************
#not quite good tries:)
##test for the relationship btw log-return and growthrate(grangertest failed)

# try logreturn
#SP500_logreturn<-diff(log(data$SP500))
#GDP_logreturn<-diff(log(data$GDP))
#grangertest(SP500_logreturn, GDP_logreturn, order=3) #fail

# try growth rate
#growth_GDP<-(data$GDP - Lag(data$GDP,1))/Lag(data$GDP,1)*100
#growth_SP500<-(data$SP500 - Lag(data$SP500,1))/Lag(data$SP500,1)*100

#grangertest(growth_GDP,growth_SP500,order=1)  #fail
#grangertest(growth_SP500,growth_GDP,order=4)  #fail

# omit NAs for test correlation 
#growth_GDP_nonan<-na.omit(growth_GDP)
#growth_SP500_nonan<-na.omit(growth_SP500)
#cor(growth_GDP_nonan,growth_SP500_nonan) # 0.4372168, not very correlated


