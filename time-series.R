
#___________________________ IMPORTING DATA AND LIBRARIES __________________________________#
libraries<-c("lubridate","forecast","tseries")
sapply(libraries,require,character.only=TRUE)

train<-read.csv("F:/AnalyticsVidya/TimeSeriesAnalysis/train.csv")
test<-read.csv("F:/AnalyticsVidya/TimeSeriesAnalysis/test.csv")

#________________________________ FORMATTING DATA _____________________________________#

#Lets have a look at the train data
summary(train)
str(train)

#Coverting date time columns in to Datetime object
train$Datetime<-dmy_hm(train$Datetime)
test$Datetime<-dmy_hm(test$Datetime)
summary(train)
str(train)

dim(train)
dim(test)
#There are no missing observation in train data
#However we are going to work only with 'Count' column 

#_____________________________________ MODELLING ________________________________________#

#Plotting first 240 data or first 
temp_ts<-ts(train[10000:10167,3],frequency=24)
ts.plot(temp_ts)
#From the above plot we can easily decide that this timeseries has a 24 hour seasonal period

ts.plot(train[,3]) #graph shows that it has increasing variance.

#Taking log of the series
timeseries<-ts(train[,3],frequency=24)
log_timeseries<-log(timeseries)
ts.plot(log_timeseries)

#As it is a seasonal time series we are taking seasonal difference
diff_ts<-diff(log_train_ts,12)
ts.plot(diff_ts)

#lest look at the different metrics after taking seasonal difference
acf(diff_ts,lag.max = 100)
pacf(diff_ts,lag.max = 100)
adf.test(diff_ts,k = 12)

#Now apply auto arima to the train dataset
fit <- auto.arima(timeseries, lambda=0)
summary(fit)
Acf(residuals(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
test<-Box.test(residuals(fit), lag=240, fitdf=24, type="Ljung")

#After calculation auto arima function chooses as the best fit.
#but we know we have to take 1 seasonal difference.So->
fit <- auto.arima(timeseries, lambda=0,d=0,D=1)
summary(fit)
Acf(residuals(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
test<-Box.test(residuals(fit), lag=240, fitdf=24, type="Ljung")

#Predicting future data
timeseries1_forcast<-forecast(fit, h=5112)

#So best model predicted by arima is ARIMA(4,0,1)(2,1,0)[24]


#___________________________________ OUTPUT ______________________________________#
output<-data.frame(ID=test$ID,Count=timeseries1_forcast$mean)
write.csv(output,"F:/AnalyticsVidya/TimeSeriesAnalysis/output.csv",row.names=FALSE)

