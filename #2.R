## 1.

data1 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/hw2.1.csv")
colnames(data1) <- c("phi0","phi1","t","at","phi__0.8","phi_0.8","phi_1","phi_1.05")
data1 <- data1[,-(1:2)]

#phi1 = -0.8
tt1.1 <- ts(data1$phi__0.8)
plot.ts(tt1.1)

acf(data1$phi__0.8[-1],lag.max=20)
pacf(data1$phi__0.8[-1],lag.max=20)

#phi1 = 0.8
tt1.2 <- ts(data1$phi_0.8)
plot.ts(tt1.2)

acf(data1$phi_0.8[-1],lag.max=20)
pacf(data1$phi_0.8[-1],lag.max=20)

#phi1 = 1
tt1.3 <- ts(data1$phi_1)
plot.ts(tt1.3)

acf(data1$phi_1[-1],lag.max=20)
pacf(data1$phi_1[-1],lag.max=20)

#phi1 = 1.05
tt1.4 <- ts(data1$phi_1.05)
plot.ts(tt1.4)

acf(data1$phi_1.05[-1],lag.max=20)
pacf(data1$phi_1.05[-1],lag.max=20)


## 2.
data2 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/hw2.2.csv")
data2 <- data2[,-(1:2)]

# 1)
tt2.1 <- ts(data2$yt)
plot.ts(tt2.1)

# 2)
tt2.2 <- ts(data2$yt_at)
plot.ts(tt2.2)

# 3)


# 4)
acf(data2$yt_at[-2],lag.max=50)

## 3.
install.packages("forecast")
library(forecast)
data3 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/RV10.csv")
data3_an <- data3[1:1723,]
data3_real <- data3[-(1:1723),]

# 1)
arima.fit1 <- Arima(data3$RV_10[1:1723],order=c(1,0,0))

mu <- 0.0099; phi1 <- 0.7900
(phi0<-mu*(1-phi1))
ar.ols(data3_an$RV_10, order=1, demean=F, intercept=T)



# 2)
forecast1 <- c()
for(i in 1:18){
  forecast1[i] <- forecast(arima.fit1,h=1)$mean
  arima.fit1 <- Arima(data3$RV_10[1:1723+i],order=c(1,0,0))
}
forecast1

library(ggplot2)
ggplot()+ geom_line(aes(x=1:18,y=data3_real$RV_10)) + geom_line(aes(x=1:18,y=forecast1),col="red")

# 3)
et <- data3_real$RV_10 - forecast4$mean
plot.ts(et)

# 4)
mae <- mean(abs(et))
mse <- mean(et^2)

# 3-2)
arima.fit5 = Arima(data3_an$RV_10, order=c(5,0,0))
phi0_5 <- 0.0099*(1-0.4718 -0.2198-0.1193+0.0342-0.1212)
ar.ols(data3_an$RV_10, order=5, demean=F, intercept=T)


forecast5 <- c()
for(i in 1:18){
  forecast5[i] <- forecast(arima.fit5,h=1)$mean
  arima.fit5 <- Arima(data3$RV_10[1:1723+i],order=c(5,0,0))
}
forecast5
 
ggplot()+ geom_line(aes(x=1:18,y=data3_real$RV_10)) + geom_line(aes(x=1:18,y=forecast5),col="red")
et5 <- data3_real$RV_10 - forecast5
plot.ts(et5)
mae5 <- mean(abs(et5))
mse5 <- mean(et5^2)
