# install.packages("tseries")
# install.packages("forecast")
library(tseries)
library(forecast)
library(openxlsx)
setwd("F:\\liu\\百度云同步盘\\我的文档\\2020\\试验设计与统计\\课件新")
a<-read.xlsx("ma.xlsx")
P<-ts(a$P,frequency = 4,start = c(2003,1))

plm1<-lm(P~t,data = a)
summary(plm1)
plm2<-lm(P~t+Q1+Q2+Q3,data = a)
summary(plm2)
a$model<-fitted.values(plm2)
plot(P,col="black")
par(new=TRUE)
lines(ts(a$model,frequency = 4,start = c(2003,1)),col="blue",lty=2)

new=data.frame(t=c(13:16),Q1=c(1,0,0,0),Q2=c(0,1,0,0),Q3=c(0,0,1,0))
new
lm.pred=predict(plm2,new,interval='prediction',level=0.95)
lm.pred



b<-read.xlsx("CO2MONTH.xlsx")
CO2<-ts(b$value,frequency = 12,start = c(2003,1))

plot(CO2)

#同样可以使用tsdisplay函数显示ACF和PACF图，用来确定arima函数的参数
tsdisplay(CO2)
#使用decompose()函数将其拆分为不同成分
dc<-decompose(CO2)
dc$trend

#打印出四种趋势图
plot(dc)
