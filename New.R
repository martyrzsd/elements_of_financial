library(fGarch)
daily_2008 <- read.csv("./Data/SP500/08年每日.csv")
SPY <- ts(daily_2008$收盘, frequency = 251, start = c(2008, 1))
dp <- ts(diff(SPY), frequency = 251, start = c(2008, 1))
par(mfrow=c(2,1))
plot(SPY)
plot(dp)
cprice <- diff(daily_2008$收盘)


m2 <- arima(cprice, order = c(3, 0, 0), seasonal = list(order = c(2, 0, 0), period = 5), include.mean = F)
m2
#length(cprice)
acf(cprice)
pacf(cprice)


m3 <- arima(cprice, seasonal = list(order = c(2, 0, 0), period = 5), include.mean = F)
summary(m3)
#减去季节性干扰因素
adjcp <- cprice[11:3081] + 0.0817 * cprice[6:3076] - 0.0667 * cprice[1:3071]
acf(adjcp)
pacf(adjcp)


m4 <- garchFit(~arma(3, 0) + garch(1, 1), data = adjcp, trace = F, include.mean = F)
summary(m4)
plot(m4)
predict(m4,n.head=10,plot=TRUE)

m5 <- garchFit(~arma(3, 0) + garch(1, 1), data = adjcp, trace = F, include.mean = F, cond.dist = "std")
summary(m5)
plot(m5)
predict(m5)

m6 <- garchFit(~arma(1, 0) + garch(1, 1), data = adjcp, trace = F, include.mean = F, cond.dist = "sstd")
summary(m6)
plot(m6)
set.seed(2020)
predict(m6,n.head=10,plot=TRUE)





############另外的代码
monthly_2008 <- read.csv("./Data/SP500/08年每月.csv")
library(forecast)
Monthly_Price<-monthly_2008$收盘
library(tseries)
Monthly_Pricets<-ts(Monthly_Price,frequency = 12,start=c(2008,1))
fit_ets<-ets(Monthly_Pricets)
fit_ets
plot(forecast(fit_ets),xlab="Time",flty=2)
accuracy(fit_ets)



#####

forecast_Original_Arma1<-accumulate(forecast(m2,h=10)$mean     ,sum)+daily_2008$收盘[length(daily_2008$收盘)]
upper80_Original_Arma1 <-accumulate(forecast(m2,h=10)$upper[,1],sum)+daily_2008$收盘[length(daily_2008$收盘)]
upper95_Original_Arma1 <-accumulate(forecast(m2,h=10)$upper[,2],sum)+daily_2008$收盘[length(daily_2008$收盘)]
lower80_Original_Arma1 <-accumulate(forecast(m2,h=10)$lower[,1],sum)+daily_2008$收盘[length(daily_2008$收盘)]
lower95_Original_Arma1 <-accumulate(forecast(m2,h=10)$lower[,2],sum)+daily_2008$收盘[length(daily_2008$收盘)]





forecast_Original_Arma2<-accumulate(forecast(m3,h=10)$mean     ,sum)+daily_2008$收盘[length(daily_2008$收盘)]
upper80_Original_Arma2 <-accumulate(forecast(m3,h=10)$upper[,1],sum)+daily_2008$收盘[length(daily_2008$收盘)]
upper95_Original_Arma2 <-accumulate(forecast(m3,h=10)$upper[,2],sum)+daily_2008$收盘[length(daily_2008$收盘)]
lower80_Original_Arma2 <-accumulate(forecast(m3,h=10)$lower[,1],sum)+daily_2008$收盘[length(daily_2008$收盘)]
lower95_Original_Arma2 <-accumulate(forecast(m3,h=10)$lower[,2],sum)+daily_2008$收盘[length(daily_2008$收盘)]



