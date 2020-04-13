library(Hmisc)
library("varhandle")
library(timeSeries) 



##### 29year sp & gdp #########

gdp29 <- read.csv('gdp29y.csv')
sp29 <- read.csv('sp29y.csv')
sp29_close <- sp29[c(1,5)]
colnames(sp29_close) <- c("DATE","SP500")
data <- merge(gdp29,sp29_close)  # this is the dataset I sent first 

# time series data from orginal datasets
ts_gdp29 <- ts(data$GDPC1,1990,2019,frequency = 4)
ts_sp50029 <- ts(data$SP500,1990,2019,frequency = 4)
quartz()
tsdisplay(ts_gdp29)
quartz()
tsdisplay(ts_sp50029)

## after z-score
gdp_z <- (data$GDPC1 - mean(data$GDPC1)) / sd(data$GDPC1)
sp_z <- (data$SP500 - mean(data$SP500)) / sd(data$SP500)
data_z <- data_frame(gdp_z,sp_z)   # this is the dataset I sent secondly

VARselect(data_z) # order = 3
var <- VAR(data_z, p = 3)
  
grangertest(sp_z,gdp_z, order = 3)
grangertest(gdp_z,sp_z, order = 3)

##test for the relationship btw log-return and growthrate(grangertest failed)

# try logreturn
sp29_logreturn <- diff(log(data$SP500))
gdp29_logreturn <- diff(log(data$GDPC1))
grangertest(sp29_logreturn, gdp29_logreturn, order=3) #fail

# try growth rate
growth_gdp29 <- (data$GDPC1 - Lag(data$GDPC1,1))/Lag(data$GDPC1,1) * 100
growth_sp50029 <- (data$SP500 - Lag(data$SP500,1))/Lag(data$SP500,1) * 100

grangertest(growth_gdp29,growth_sp500,order = 1)  #fail
grangertest(growth_sp500,growth_gdp29,order = 4)  #fail

# omit NAs for correlation test
growth_gdp29nonan <- na.omit(growth_gdp29)
growth_sp50029nonan <- na.omit(growth_sp50029)
cor(growth_gdp29nonan,growth_sp50029nonan) # 0.4372168, not very correlated



#####
quartz()
tsdisplay(ts_sp50029)
auto.arima(ts_sp50029)
