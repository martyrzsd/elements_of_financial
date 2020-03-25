library(quantmod)
library(plyr)
library(TTR)
library(ggplot2)
library(scales)
library(tseries)
library(tseries)
library(forecast)
#稳定性检验

#Import Data
AAPL_Daily_Data <- read.csv("./Data/AAPL.csv")


AAPL_Daily_Close <- as.vector(AAPL_Daily_Data$Adj.Close,mode = "numeric")#收盘价
AAPL_Daily_Close_diff <- diff(AAPL_Daily_Close)/AAPL_Daily_Close[-length(AAPL_Daily_Close)]
AAPL_Daily_Close_diff[AAPL_Daily_Close_diff=="NaN"] <- 0
AAPL_Daily_Close_diff[AAPL_Daily_Close_diff=="Inf"] <- 0

#createdata <- data[-1,]#我这个data不知道为啥多了一行
createdata <- cbind(AAPL_Daily_Data$Date, c(0,AAPL_Daily_Close_diff))
#createdata=createdata[nrow(createdata):1,]
plot(createdata[,1],createdata[,2])
##添加指标
lines(createdata[,1],DEMA(createdata[,2]),col="green")
lines(createdata[,1],SMA(createdata[,2]),col="red")
legend("bottomright",col=c("green","red"),legend=c("DEMA","SMA"),lty=1,pch=1)
#感觉不平稳 就差分一下 定阶

ndiffs(Daily_Price)#算出来为2
dPrice<-diff(Daily_Price,differences = 2)
plot(dPrice)
adf.test(dPrice)