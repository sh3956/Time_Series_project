## R SETUP
library(lubridate)
library(zoo)
library(dynlm)
library(forecast)
library(minpack.lm)
library(numbers)
#read in the data
Data = read.csv("TOTALNSA.csv", header=TRUE, sep=",")

#decode the date
Data$DATE = as.Date(Data$DATE,"%Y-%m-%d")
ts_TOTALNSA = ts(data = Data$TOTALNSA,1976,2019,freq=12)

#II.1
#(a) Show a time-series plot of your data. 
quartz()
plot(ts_TOTALNSA,main="Total Vehicle Sales time series",xlab="year",ylab="Thousands of Unit ")

#(b) Does your plot in (a) suggest that the data are covariance stationary? Explain your answer. 
#The data points are not covariance stationary because they are not sharing the same mean.

#(c) Plot and discuss the ACF and PACF of your data. 

Sales_acf<-acf(Data$TOTALNSA)
Sales_pacf<-pacf(Data$TOTALNSA)
quartz()
par(mfrow=c(2,1))
plot(Sales_acf,main="Total Vihecle Sales Time Series ACF")
plot(Sales_pacf,main="Total Vihecle Sales Time Series PACF")


#(d) Fit a linear and nonlinear (e.g., polynomial, exponential, quadratic + periodic, etc.) model to your series. 
#    In one window, show both figures of the original times series plot with the respective fit. 

#Linear(3 Lag): 
sales = ts(Data$TOTALNSA, start = 1976, frequency = 12)
m1=dynlm(sales~L(sales, 1)+L(sales, 2)+L(sales, 3))
t1<-seq(1976, 2019,length=length(m1$fit))
quartz("Linear")
plot(sales, main="Linear Model Prediction", ylab = "Total NSA(Thousands of Unit)", xlab = "Year", lwd = 1, xlim = c(1976,2019))
lines(m1$fitted.values,col = "red3", lwd = 1)
legend("topright",pch= c("-","-"),legend=c("Original Data", "Linear Model Prediction"),col=c("black", "red"),  cex=0.8)
summary(m1)

AIC(m1)
BIC(m1)

#Exponential:
t2 = seq(2917,2019,length = length(sales))
ds = data.frame(x = t, y = sales)
m2 = nlsLM(y ~ exp(a+b*t),data = ds, start = list(a = 0, b = 0))
quartz("plots exp")
plot(x = Data$DATE, y= m2$m$fitted()[1:519], type = "l", main="Exponential Model Prediction",col = "red3",ylab = "Total NSA(Thousands of Unit)", xlab = "Year", ylim = c(600,2000))
lines(x = Data$DATE, y = sales,  lwd = 1)
legend("topright",pch= c("-","-"),legend=c("Original Data", "Exponential Model Prediction"),col=c("black", "red"),  cex=0.8)

quartz("Exponential Model residuals vs. fitted values")
plot(m2)
summary(m2)

#(e) For each model, plot the respective residuals vs. fitted values and discuss your observations.

#for linear:
quartz("e_linear")
plot(x = m1$fitted.values, y = m1$residuals, type = "p", pch = 20, main = "fitted value vs. residual",xlab = "Fitted Value", ylab = "Residuals")
abline(a = 0, b = 0)

#for Exponential:
quartz("e_Exp")
plot(x = m2$m$fitted(), y = m2$m$resid(), type = "p", pch = 20, main = "fitted value vs. residual", xlab = "Fitted Value", ylab = "Residuals")
abline(a = 0, b = 0)

#There's no obvious shape and generally symmetrically distributed around 0 in both graph plotting
#Thus the variables is linearly related and generally homoscedastic.

#(f) For each model, plot a histogram of the residuals and discuss your observations. 
quartz("m1 residuals histogram")
hist(m1$residuals,main="Hisrogram for Residual of Linear Model",xlab="Number of Units(in thousands)")
quartz("m2 residuals histogram")
hist(m2$m$resid(),main="Hisrogram for Residual of Exponential Model",xlab="Number of Units(in thousands)")
#From the histogram, the residuals has a relatively good normal distribution

#(g) For each model, discuss the associated diagnostic statistics (R2 , t−distribution, F−distribution, etc.) 
summary(m1)
#Multiple R-squared:  0.5734,	Adjusted R-squared:  0.5709 which is quite acceptable
#t-distribution: all the t-value is far greater than any usually accepted confidence interval, 
#                so we can reject the null hypothesis that any coefficient is 0 
#F-statistic: 229.4 on 3 and 512 DF,  p-value: < 2.2e-16 which is quite acceptable
summary(m2)
#t-distribution: all the t-value is far greater than any usually accepted confidence interval, 
#                so we can reject the null hypothesis that any coefficient is 0 
#The other two data are not provided

#(h) Select a trend model using AIC and one using BIC (show the values obtained from each criterion). Do the selected models agree? 
AIC(m1,m2)
BIC(m1,m2)
#Because of the trend, both model have extremely large AIC and BIC, but linear model performs relatively better

#(i) Use your preferred model to forecast h-steps (at least 16) ahead. Your forecast should include the respective uncertainty prediction interval. Depending on your data, h will be in days, months, years, etc. 
quartz("m1 forecasting")
plot(forecast(m1,h = 519))

##II.2
#(a) Construct and test (by looking at the diagnostic statistics) a model with a full set of seasonal dummies.
#construct the dummy set, each column is a dummy variable

# Construct seasonal dummies
dummies = matrix(nrow = length(Data$TOTALNSA), ncol = 12)
for (i in 1:12)
{
  for(j in 1:519)
  {
    dummies[j,i] = as.integer(mod(j,12) == i-1)
  }
}
# Construct model with full seasonal dummies
m3=dynlm(sales~dummies[,1]+dummies[,2]+dummies[,3]+dummies[,4]++dummies[,5]+dummies[,6]+dummies[,7]+dummies[,8]+dummies[,9]+dummies[,10]+dummies[,11]+dummies[,12])
#Diagnostic Statistics
quartz("Model diagnostic statistic with seasonal dummies")
par(mfrow=c(2,2))
plot(m3,main="Diagnostic statistic Graph Part")
# numerical diagnostic statistic
summary(m3)
AIC(m3)

#(b) Plot the estimated seasonal factors and interpret your plot.
coef_sdummy <-m3$coefficients
# rearrange order
Dec_sdummy<-coef_sdummy[2]
coef_sdummy <- coef_sdummy[-c(2)]
coef_sdummy[12]<-0 # change na to 0
coef_sdummy <-c(coef_sdummy,Dec_sdummy)
x<-c(1:13)   # x-axis
# x-axis name
x_name <- c("Intercept","Jan.","Feb.","Mar.","Apr.","May.","Jun.","Jul.","Aug.","Sep.","Oct","Nov.","Dec.")
# plot seasonal factor
quartz("seasonal dummies plot")
plot(x,coef_sdummy,xaxt="none",type="l",main="Seasonal Factor plot",ylab="Value of dummy variables",xlab="Variable name")
axis(1, at=1:13, labels=x_name, cex.axis=0.5)

#(c)
m5=dynlm(sales~L(sales, 1)+L(sales, 2)+L(sales, 3)+dummies[,1]+dummies[,2]+dummies[,3]+dummies[,4]++dummies[,5]+dummies[,6]+dummies[,7]+dummies[,8]+dummies[,9]+dummies[,10]+dummies[,11]+dummies[,12])
summary(m5)
AIC(m5)

#have dummies[,10](insignificant) removed
m6=dynlm(sales~L(sales, 1)+L(sales, 2)+L(sales, 3)+dummies[,1]+dummies[,2]+dummies[,3]+dummies[,4]++dummies[,5]+dummies[,6]+dummies[,7]+dummies[,8]+dummies[,9]+dummies[,11]+dummies[,12])
summary(m6)
AIC(m6)

#remove insignificant factor
#Alex's note:
#I hesitate for a while if we should delete the intercept, 
#for it's a linear model. If I keep it
m7_1=dynlm(sales~L(sales, 1)+L(sales, 2)+L(sales, 3)+dummies[,1]+dummies[,2]+dummies[,3]+dummies[,4]++dummies[,5]+dummies[,6]+dummies[,7]+dummies[,8]+dummies[,9]+dummies[,11])
summary(m7_1)
AIC(m7_1)

#But if I removed the intercept the R^2 is greatly improved(.99 R^2...)
m7_2=dynlm(sales~0+L(sales, 1)+L(sales, 2)+L(sales, 3)+dummies[,1]+dummies[,2]+dummies[,3]+dummies[,4]++dummies[,5]+dummies[,6]+dummies[,7]+dummies[,8]+dummies[,9]+dummies[,11])
summary(m7_2)
AIC(m7_2)

#For the sake of .99 R^2, I choose the one without intercept as the full model
m7 = m7_2

#plot original data and predction by the full model
quartz("Linear")
plot(sales, main="Full Model Prediction", ylab = "Total NSA(Thousands of Unit)", xlab = "Year", lwd = 1, xlim = c(1976,2019))
lines(m7_2$fitted.values,col = "red3", lwd = 1)
legend("topright",pch= c("-","-"),legend=c("Original Data", "Full Model Prediction"),col=c("black", "red"),  cex=0.8)


#for residuals vs. fitted values
quartz("Residuals vs. fitted values for full model")
plot(x = m7_2$fitted.values, y = m7$residuals, type = "p", pch = 20, main = "Full model fitted value vs. residual",xlab = "Fitted Value", ylab = "Residuals")
abline(a = 0, b = 0)
#There's no obvious shape and generally symmetrically distributed around 0 in both graph plotting
#Thus the variables not linearly correlated and generally homoscedastic.

#(d) Interpret the respective summary statistics including the error metrics of your full model.
#Very Impressive R^2, t, and f
summary(m7_2)
#However we are pretty bad at this(Because we haven't discuss cycles in lectures)
AIC(m7)
BIC(m7)
#Find mse is 9794.861, behave poorly
mean((sales - m7_2$fitted.values)^2)


#(e)Use the full model to forecast h-steps (at least 16) ahead. Your forecast should include the respective prediction interval.

# In forecast function, only h=519 can be used, otherwise there would be Error in variables 
quartz()
plot(forecast(m7,h = 519))

#III
#As what I mentioned in II.2.(d), we have a horrible AIC and BIC, 
#which is not only because of we have great amount of variables(it's overfitted for sure)
#but we also have a very stochastic trend, see:
plot(stl(ts(Data$TOTALNSA,freq = 12), s.window = "periodic"))
#as what is discussed in OH, we need more techniques to dealing with the cycles pattern
