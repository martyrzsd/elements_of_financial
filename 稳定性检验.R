library(quantmod)
library(plyr)
library(TTR)
library(ggplot2)
library(scales)
library(tseries)
#稳定性检验
a=data_Daily$Adj.Close#收盘价
a=diff(a)/a[-length(a)]
a[a=="NaN"]=0
a[a=="Inf"]=0
createdata<-data[-1,]#我这个data不知道为啥多了一行
createdata[,1]=data_Daily$Date
createdata[,2]=c(0,a)
createdata=createdata[nrow(createdata):1,]
plot(createdata[,1],createdata[,2])
##添加指标
lines(createdata[,1],DEMA(createdata[,2]),col="green")
lines(createdata[,1],SMA(createdata[,2]),col="red")
legend("bottomright",col=c("green","red"),legend=c("DEMA","SMA"),lty=1,pch=1)
#感觉不平稳 就差分一下 定阶
library(tseries)
library(forecast)
ndiffs(Daily_Price)#算出来为2
dPrice<-diff(Daily_Price,differences = 2)
plot(dPrice)
adf.test(dPrice)