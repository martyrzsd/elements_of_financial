library(quantmod)
library(plyr)
library(TTR)
library(ggplot2)
library(scales)
library(tseries)
library(tseries)
library(forecast)
library(RColorBrewer)
#library(svglite)
#稳定性检验

#Import Data
AAPL_Daily_Data <- read.csv("./Data/AAPL.csv")

# Wash and differenciate
AAPL_Daily_Close <- as.vector(AAPL_Daily_Data$Adj.Close,mode = "numeric")#收盘价
AAPL_Daily_Close_diff <- diff(AAPL_Daily_Close)/AAPL_Daily_Close[-length(AAPL_Daily_Close)]
AAPL_Daily_Close_diff[AAPL_Daily_Close_diff=="NaN"] <- 0
AAPL_Daily_Close_diff[AAPL_Daily_Close_diff=="Inf"] <- 0
AAPL_Daily_Close_diff[AAPL_Daily_Close_diff>60]<-0 #166为大于60的异常值

# Plot
DateStep <- "1 year" #month, weak, day. Change the scale of x
DataToPlot <- data.frame(Date=as.Date(AAPL_Daily_Data$Date), DIFF=c(0,AAPL_Daily_Close_diff) ,DEMA=DEMA(c(0,AAPL_Daily_Close_diff)),SMA=SMA(c(0,AAPL_Daily_Close_diff)))
plot1 <- ggplot(data = DataToPlot,aes(x=Date))
plot1 <- plot1+scale_x_date(date_breaks = DateStep)+theme(axis.text.x = element_text(angle = 90))
plot1 <- plot1+geom_point(aes(y=DIFF),size=0.3 )
plot1 <- plot1+geom_line(aes(y=DEMA),col="#984EA3", alpha = 0.9)# Use RColorBrewer to see color template
plot1 <- plot1+geom_line(aes(y=SMA),col="#377EB8", alpha = 0.9)
plot1
ggsave(file="Rplot1.svg", plot=plot1, width=40, height=8)

#createdata=createdata[nrow(createdata):1,]
#plot(DataToPlot[,1],DataToPlot[,2])
##添加指标
#lines(DataToPlot[,1],DEMA(DataToPlot[,2]),col="green")
#lines(DataToPlot[,1],SMA(DataToPlot[,2]),col="red")
#legend("bottomright",col=c("green","red"),legend=c("DEMA","SMA"),lty=1,pch=1)
#感觉不平稳 就差分一下 定阶

#ndiffs(Daily_Price)#算出来为2
#dPrice<-diff(Daily_Price,differences = 2)
#plot(dPrice)
#adf.test(dPrice)