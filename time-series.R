
libraries<-c("lubridate","xts","forecast")
sapply(libraries,require,character.only=TRUE)

######################## IMPORTING DATA #################################
train<-read.csv("C:/Users/20720/Desktop/AV-Time_Series/train.csv")
test<-read.csv("C:/Users/20720/Desktop/AV-Time_Series/test.csv")

######################### FORMATTING DATA ##############################


train$Datetime<-dmy_hm(train$Datetime)
test$Datetime<-dmy_hm(test$Datetime)
#times.init <-format(train[,2], "%Y-%m-%d %H:%M:%S",tz="UTC",usetz=FALSE)
#times.init<-as.POSIXct(times.init)
#data2 <-zoo(train[,3],times.init)
#data3 <-merge(data2, zoo(,seq(start(data2), end(data2),by="hour")))


############## ARIMA ########################
train_ts<-ts(train[,3],frequency=12)
plot(train_ts)
log_train_ts<-log(train_ts)
plot(log_train_ts)

ndiffs(log_train_ts, alpha=0.05, test=c("kpss","adf", "pp"), max.d=2)
nsdiffs(log_train_ts, m=frequency(log_train_ts), test=c("ocsb","ch"), max.D=1)

diff1<-diff(log_train_ts,12)
plot(diff1)
adf.test(diff1)

acf(diff1, lag.max=100)
pacf(diff1,lag.max=100)

fit <- Arima(log_train_ts, order=c(1,1,1))
summary(fit)
Acf(residuals(fit))
temp<-Box.test(residuals(fit), lag=24, fitdf=4, type="Ljung")

timeseries1<-auto.arima(log_train_ts,stepwise=FALSE)
timeseries1_forcast<-forecast(timeseries1, h=50)

############ Holt's Exponential smoothing #####

################## MSTS ###########################
train_ts<-msts(train[,3],seasonal.periods=c(24,168))
m_tbats = tbats(train_ts)
f_tbats = forecast(m_tbats, h=5112)
plot(f_tbats)

##################### Output ########################
output<-data.frame(ID=test$ID,Count=round(f_tbats$mean))

################# EXTRA ####################
