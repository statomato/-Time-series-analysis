library(forecast)
data <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/일별 Won_Dollar_환율_2006_2012.csv")

acf(data$rt[-1],lag.max=20)
pacf(data$rt[-1],lag.max=20)

mean(data$rt[-1]); sd(data$rt[-1])

acf1 <-acf(data$rt[-1],lag.max=20)
acf1$acf
n <- length(data$rt)
se_ <- 1
for(i in 2:6){
  se_ <- se_+2*(1-(i-1)/n)*acf1$acf[i]
}
se_rbar <- sqrt(se_*var(data$rt)/n)
se_rbar

arima.bic <- c()
for(i in 0:8){
  arima.bic[i+1] <- Arima(data$rt, order=c(i,0,0), method="ML")$bic
}
which.min(arima.bic)

library(fGarch)
garchFit(~arma(0,0)+garch(1,1), data=data$rt, cond.dist = "norm", include.mean = F, include.delta = F, include.skew = F)

garchFit1 <- garchFit(~arma(0,0)+garch(1,1), data=data$rt, cond.dist = "norm", include.mean = F, include.delta = F, include.skew = F) 
pred <- c()


arima.fit <- Arima(data$rt[-1], order=c(0,0,0)) 
at <- data$rt[-1]
sigma <- garchFit1@sigma.t

library(ggplot2)
ggplot()+geom_line(aes(x=1:length(at),y=at),col="red")+geom_line(aes(x=1:length(sigma),y=sigma),col="blue")

garch_norm <- c(); pred_norm <- c()
rt <- data$rt[-1]
for(i in 1:247){
  garch_norm <- garchFit(~arma(0,0)+garch(1,1),data = rt[1:(1516+i)],cond.dist = "norm",include.mean = F,include.delta = F,include.skew = F)
  pred_norm[i] <- predict(garch_norm, n.ahead = 1, mse='cond')$standardDeviation
}
ggplot()+geom_line(aes(x=1:length(pred_norm),y=pred_norm))
pred_ntest5 <- c(); pred_ntest1 <- c()
for (i in 1:247){
  pred_ntest5[i] <- pred_norm[i]*qnorm(0.05)  
  pred_ntest1[i] <- pred_norm[i]*qnorm(0.01)   
} 
ggplot()+geom_line(aes(x=1:length(pred_ntest5),y=pred_ntest5))
ggplot()+geom_line(aes(x=1:length(pred_ntest1),y=pred_ntest1))




garchFit2 <- garchFit(~arma(0,0)+garch(1,1), data=data$rt, cond.dist = "std", shape=5,include.shape = F,include.mean = F, include.delta = F, include.skew = F) 

sigma2 <- garchFit2@sigma.t
ggplot()+geom_line(aes(x=1:length(at),y=at),col="red")+geom_line(aes(x=1:length(sigma2),y=sigma2),col="blue")


garch_t <- c(); pred_t <- c()
for(i in 1:247){
  garch_t <- garchFit(~arma(0,0)+garch(1,1),data = rt[1:(1516+i)],cond.dist = "std",shape=5,include.shape = F,include.mean = F,include.delta = F,include.skew = F)
  pred_t[i] <- predict(garch_t, n.ahead = 1, mse='cond')$standardDeviation
}
ggplot()+geom_line(aes(x=1:length(pred_t),y=pred_t))
pred_ttest5 <- c(); pred_ttest1 <- c()
for (i in 1:247){
  pred_ttest5[i] <- pred_t[i]*qt(0.05,5)*sqrt(3/5)  
  pred_ttest1[i] <- pred_t[i]*qt(0.01,5)*sqrt(3/5)
} 
ggplot()+geom_line(aes(x=1:length(pred_ttest5),y=pred_ttest5))
ggplot()+geom_line(aes(x=1:length(pred_ttest1),y=pred_ttest1))
