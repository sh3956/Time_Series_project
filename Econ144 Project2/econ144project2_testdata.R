library(Hmisc)
library("varhandle")
library(timeSeries) # use interpNA

setwd("/Users/Renaissance/Desktop/econ144/Econ144 Project2")
vix <- read.csv('^VIX.csv')
sp <- read.csv('^GSPC.csv')

diffsp<-diff(sp$Close)
diffvix<-diff(vix$Close)

resp<-(sp$Close - Lag(sp$Close,1))/Lag(sp$Close)
revix<-(vix$Close - Lag(vix$Close,1))/Lag(vix$Close)

resp<-na.omit(resp)
revix<-na.omit(revix)

######
pep<-read.csv('PEP.csv')
coke<-read.csv('COKE.csv')

repep<-(pep$Close - Lag(pep$Close,1))/Lag(pep$Close)
recoke<-(coke$Close - Lag(coke$Close,1))/Lag(coke$Close)

grangertest(recoke,repep,order=4)

grangertest(pep$Close,coke$Close, order = 2)
######

brk<- read.csv('BRK-A.csv')
ixic<-read.csv('^IXIC.csv')

rebrk<-(brk$Close - Lag(brk$Close,1))/Lag(brk$Close,1)
reixic<-(ixic$Close - Lag(ixic$Close,1))/Lag(ixic$Close,1)

brkun<-as.numeric(unfactor(brk$Close))
ixicun<-as.numeric(unfactor(ixic$Close))

nonan_brk<-na.omit(brkun)
nonan_ixic<-na.omit(ixicun)

brkdiff<-(diff(brkun))
ixicdiff<-(diff(ixicun))

grangertest(ixicdiff,brkdiff,order = 5)


#########
sp500<-read.csv('SP500-2.csv')
gdp<-read.csv('GDPC1.csv')

merge<-merge(sp500,gdp)

merge$SP500<-as.numeric(merge$SP500)

for(i in 1:27)
  
{
if(is.na(merge$SP500[i])== 'TRUE')
{
  merge$SP500[i]=merge$SP500[i-1]
}
}

diffgdp<-diff(merge$GDPC1)
diffsp500<-diff(merge$SP500)

growth_gdp<-(merge$GDPC1 - Lag(merge$GDPC1,1))/Lag(merge$GDPC1,1)
growth_sp500<-(merge$SP500 - Lag(merge$SP500,1))/Lag(merge$SP500,1)

grangertest(growth_gdp,growth_sp500,order = 1)

## tsdisplay
sp500_int <- interpNA(merge$SP500,method="linear")
tsx<-ts(sp500_int,2009.5,2018,frequency=3)
quartz()
tsdisplay(tsx)

gdp_int <- interpNA(merge$GDPC1,method="linear")
tsy<-ts(gdp_int,2009,2018,frequency=3)
quartz()
tsdisplay(tsy)

###########
pep_c<-read.csv('PEP copy.csv')
coke_c<-read.csv('COKE copy.csv')

tspep<- interpNA(pep_c$Close)
tspep<- as.numeric(tspep)
tscoke<- interpNA(coke_c$Close)
tscoke<- as.numeric(tscoke)

repep_c<-(tspep- Lag(tspep,1))/Lag(tspep)
recoke_c<-(tscoke - Lag(tscoke,1))/Lag(tscoke)


tspep_c<-ts(tspep,2009.5,2019.5,frequency=12)
tscoke_c<-ts(tscoke,2009.5,2019.5,frequency=12)

quartz()
tsdisplay(tspep_c)
quartz()
tsdisplay(tscoke_c)


grangertest(recoke_c,repep_c,order=6) # ail

grangertest(tspep,tscoke,order = 6) #fail
 
nonan_tspep<- na.omit(tspep)
nonan_tscoke<- na.omit(tscoke)

##### 29year sp & gdp #########

gdp29<-read.csv('gdp29y.csv')
sp29<-read.csv('sp29y.csv')
sp29_close<-sp29[c(1,5)]
colnames(sp29_close)<-c("DATE","SP500")
data<-merge(gdp29,sp29_close)


## after z-score
gdp_z <- (data$GDPC1-mean(data$GDPC1))/sd(data$GDPC1)
sp_z <- (data$SP500-mean(data$SP500))/sd(data$SP500)
z_data<-data_frame(gdp_z,sp_z) 
VARselect(z_data)
grangertest(sp_z,gdp_z,order=3)
grangertest(gdp_z,sp_z,order=3)


##### var for original data
var_data<-data[c(2,3)]
VAR(var_data,p=2)

sp29_logreturn<-diff(log(data$SP500))
gdp29_logreturn<-diff(log(data$GDPC1))
grangertest(sp29_logreturn,gdp29_logreturn,order=3)

tsgdp29<-ts(data$GDPC1,1990,2019,frequency = 3)
tssp50029<-ts(data$SP500,1990,2019,frequency = 3)
tssp50029_logreturn<-ts(sp29_logreturn,1990,2019,frequency = 3)
quartz()
tsdisplay(tsgdp29)
quartz()
tsdisplay(tssp50029)
quartz()
tsdisplay(tssp50029_logreturn)


grangertest(data$GDPC1,data$SP500,order=2)
grangertest(data$SP500,data$GDPC1,order=1)

cor(data$GDPC1,data$SP500)

### growth rate
growth_gdp29<-(data$GDPC1 - Lag(data$GDPC1,1))/Lag(data$GDPC1,1)*100
growth_sp50029<-(data$SP500 - Lag(data$SP500,1))/Lag(data$SP500,1)*100

grangertest(growth_gdp29,growth_sp500,order=1)  ### no 
grangertest(growth_sp500,growth_gdp29,order=4)

growth_gdp29nonan<-na.omit(growth_gdp29)
growth_sp50029nonan<-na.omit(growth_sp50029)

cor(growth_gdp29nonan,growth_sp50029nonan)

##save
write.csv(data, file = "29yeardata_2.csv")
write.csv(z_data,file = "29yeardata_zscore.csv")

