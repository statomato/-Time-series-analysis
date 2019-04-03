#4주차

##1
hw3.2 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/hw3.2.csv")
library(moments)

acf(hw3.2$RVt,lag.max=20)
acf(hw3.2$pt,lag.max=20)
acf(hw3.2$rt[-1],lag.max=20)

pacf(hw3.2$RVt,lag.max=20)
pacf(hw3.2$pt,lag.max=20)
pacf(hw3.2$rt[-1],lag.max=20)


library(fUnitRoots); library(forecast)

#AIC
arima.aic1 <- c()
for(i in 0:8){
  arima.aic1[i+1] <- Arima(hw3.2$RVt, order=c(i,0,0), method="ML")$aic
}
which.min(arima.aic1)
plot(arima.aic1, type="b")
axis(1,c(0:8))
abline(v=which.min(arima.aic1))

#BIC
arima.bic1 <- c()
for(i in 0:8){
  arima.bic1[i+1] <- Arima(hw3.2$RVt, order=c(i,0,0), method="ML")$bic
}
which.min(arima.bic1)
plot(arima.bic1, type="b")
axis(1,c(0:8))
abline(v=which.min(arima.bic1))


#AIC
arima.aic3 <- c()
for(i in 0:8){
  arima.aic3[i+1] <- Arima(hw3.2$rt, order=c(i,0,0), method="ML")$aic
}
which.min(arima.aic3)
plot(arima.aic3, type="b")
axis(1,c(0:8))
abline(v=which.min(arima.aic3))
adfTest(hw3.2$rt, lags = 3, type = "c") 

#BIC
arima.bic3 <- c()
for(i in 0:8){
  arima.bic3[i+1] <- Arima(hw3.2$RVt, order=c(i,0,0), method="ML")$bic
}
which.min(arima.bic3)
plot(arima.bic3, type="b")
axis(1,c(0:8))
abline(v=which.min(arima.bic3))


##2
#(1)
arima.fit1 <- Arima(hw3.2$RVt[1:1516],order=c(7,0,0))
forecast1 <- forecast(arima.fit1,h=1)$mean
forecast1
hw3.2$RVt[1517]-forecast1

#(2)
arima.fit2 <- Arima(hw3.2$RVt[1:1517],order=c(7,0,0))
forecast2 <- forecast(arima.fit2,h=1)$mean
hw3.2$RVt[1518];forecast2
hw3.2$RVt[1518]-forecast2

#(3)
arima.fit3 <- Arima(hw3.2$RVt[1:1763],order=c(7,0,0))
forecast3 <- forecast(arima.fit3,h=1)$mean
hw3.2$RVt[1764];forecast3
hw3.2$RVt[1764]-forecast3

#(4)
arima.fit <- c()
pred <- c()
e <- c()
for (i in 1518:1763){
  arima.fit <- Arima(hw3.2$RVt[1:(i-1)], order=c(7,0,0)) 
  pred[i] <- forecast(arima.fit, h=1)$mean[1]
  e[i] <- hw3.2$RVt[i]- pred[i]
}
ee <- e[1518:1763]

mean(abs(ee))
mean(abs(ee)/hw3.2$RVt[1518:1763]*100)
sqrt(mean(ee^2))

##HAR(3) model
library(xts); library(highfrequency)


##(1)
RVt_1516 <- hw3.2$RVt[1:1516]
xts_data_1516 <- xts(RVt_1516, order.by = as.Date(hw3.2$Date2[1:1516]))
har_1516 <- harModel(data=xts_data_1516 , periods = c(1,5,22), RVest = c("rCov"), 
                     type="HARRV",h=1,transform=NULL)
har_1516
y_w_1517 <- mean(RVt_1764[1512:1516])
y_d_1517 <- mean(RVt_1764[1495:1516])
y_hat1517 <- har_1516$coefficients[1]+hw3.2$RVt[1516]*har_1516$coefficients[2]+y_w_1517*har_1516$coefficients[3]+y_d_1517*har_1516$coefficients[4]
RVt_1764[1517]-y_hat1517

##(2)
RVt_1517 <- hw3.2$RVt[1:1517]
xts_data_1517 <- xts(RVt_1517, order.by = as.Date(hw3.2$Date2[1:1517]))
har_1517 <- harModel(data=xts_data_1517 , periods = c(1,5,22), RVest = c("rCov"), 
                     type="HARRV",h=1,transform=NULL)
har_1517
y_w_1518 <- mean(RVt_1764[1513:1517])
y_d_1518 <- mean(RVt_1764[1496:1517])
y_hat1518 <- har_1517$coefficients[1]+hw3.2$RVt[1517]*har_1517$coefficients[2]+y_w_1518*har_1517$coefficients[3]+y_d_1518*har_1517$coefficients[4]
RVt_1764[1518]-y_hat1518

##(3)
RVt_1764 <- hw3.2$RVt[1:1763]
xts_data_1764 <- xts(RVt_1764, order.by = as.Date(hw3.2$Date2[1:1763]))
har_1764 <- harModel(data=xts_data_1764 , periods = c(1,5,22), RVest = c("rCov"), 
                     type="HARRV",h=1,transform=NULL)
har_1764

y_w_1764 <- mean(RVt_1764[1759:1763])
y_d_1764 <- mean(RVt_1764[1742:1763])
y_hat1764 <- har_1764$coefficients[1]+hw3.2$RVt[1763]*har_1764$coefficients[2]+y_w_1764*har_1764$coefficients[3]+y_d_1764*har_1764$coefficients[4]
hw3.2$RVt[1764]-y_hat1764

##(4)
e4 <-c()
har <- c()
w4 <- c()
m4 <- c()
RVt <- hw3.2$RVt
for (i in 1518:1763){
  xts_data <- xts(RVt[1:(i-1)],order.by=as.Date(hw3.2$Date2[1:(i-1)]))
  har <- harModel(data=xts_data, periods = c(1,5,22), RVest = c("rCov"), 
                  type="HARRV",h=1,transform=NULL)$coefficients
  w4 <- mean((RVt[(i-5):(i-1)]))
  m4 <- mean((RVt[(i-22):(i-1)]))
  pred[i] <- har[1]+har[2]*RVt[i-1]+har[3]*w4+har[4]*m4
  
  e4[i] <- RVt[i]-pred[i]
  
}

ee2 <- e4[1518:1763]

mean(abs(ee2))
mean(abs(ee2)/hw3.2$RVt[1518:1763]*100)
sqrt(mean(ee2^2))


