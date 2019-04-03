# HW 7
library(moments); library(ggplot2); library(rugarch)
data <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/KOSPIdaily(20010102-20151109) (1).csv")
colnames(data)
n <- length(data$P)
rt <- log(data$P[2:n])-log(data$P[1:n-1])

#A_1
plot.ts(data$P)
plot.ts(rt)

#A_2
mean(rt)
var(rt)
sd(rt)
summary(rt)
boxplot(rt)
skewness(rt)
kurtosis(rt)

x <- seq(-0.2,0.2,0.001)
y <- dnorm(x,mean(rt),sd(rt))
ggplot()+geom_histogram(aes(x=rt,y=..density..),bins = 80)+geom_line(aes(x=x,y=y))

#A_3
rt1 <- rt[rt>=0]
rt2 <- rt[rt<0]

length(rt1); length(rt2)
mean(rt1); mean(rt2)
var(rt1); var(rt2)

#A_4
at <- rt- mean(rt)
mean(rt)
var(rt)


##GARCH(1,1)
spec.garch = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        
                        mean.model=list(armaOrder=c(0,0), include.mean=F), distribution.model="norm")


garch.fit = ugarchfit(data = at, spec=spec.garch)

infocriteria(garch.fit)[1] *n

##EGARCH(1,1)
spec.egarch = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                         
                         mean.model=list(armaOrder=c(0,0),
                                         include.mean=F), distribution.model="norm")

egarch.fit = ugarchfit(data = at,
          spec=spec.egarch)
coef(egarch.fit)
infocriteria(egarch.fit)[1] *n

##GJR-GARCH(1,1)
spec.gjr = ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                      
                      mean.model=list(armaOrder=c(0,0),include.mean=F), distribution.model="norm")


gjrgarch.fit = ugarchfit(data = at, spec=spec.gjr)
infocriteria(gjrgarch.fit)[1] *n

##APARCH(1,1)
spec.aparch = ugarchspec(variance.model=list(model="apARCH",garchOrder=c(1,1)),
                         
                         mean.model=list(armaOrder=c(0,0),include.mean=F), distribution.model="norm")


aparch.fit = ugarchfit(data = at, spec=spec.aparch)
infocriteria(aparch.fit)[1] *n

#A_5
garch.rt = ugarchfit(data =rt, spec = spec.garch)
garch.rt@fit$sigma # volatility 값
model.fore = ugarchforecast(garch.rt, data = rt, n.ahead = 1) 
# model.fore@forecast$sigmaFor # forecast 값 

a_ <- at/garch.rt@fit$sigma
ggplot()+geom_histogram(aes(x=a_),bins = 80)

#B
temp <- c()
for(i in 1:168){
  temp[i] <- data$P[22*(i-1)+1]
}
rt_22 <- diff(log(temp))

## Empirical 분포 이용시 pt의 VaR
VaR_pt_0.1 <- 1000*(1+quantile(rt,0.001))
VaR_pt_1 <- 1000*(1+quantile(rt,0.01))
VaR_pt_5 <- 1000*(1+quantile(rt,0.05))

cbind(VaR_pt_0.1,VaR_pt_1,VaR_pt_5)

1000*(1+quantile(rt_22,0.001))
1000*(1+quantile(rt_22,0.01))
1000*(1+quantile(rt_22,0.05))

## Volatility 예측치
spec.garch <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0), include.mean=F), distribution.model="norm")
garch.fit <- ugarchfit(data = at, spec=spec.garch)
garch.fit@fit$sigma 
model.fore <- ugarchforecast(garch.fit, data = at, n.ahead = 22) 
pred_norm <- model.fore@forecast$sigmaFor     #volatility


spec.gjr = ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                      
                      mean.model=list(armaOrder=c(0,0)), distribution.model="norm")


gjrgarch.fit = ugarchfit(data = at, spec=spec.gjr)
model.fore2 <- ugarchforecast(gjrgarch.fit, data = at, n.ahead = 22) 
pred_norm2 <- model.fore2@forecast$sigmaFor     #volatility
pred_norm2

## Normal error 가정시 손실액의 VaR
#GARCH(1,1)
#1일 VaR
VaR_rt_0.1 <-mean(rt)+qnorm(0.001)*pred_norm[1]
VaR_rt_1 <- mean(rt)+qnorm(0.01)*pred_norm[1]
VaR_rt_5 <- mean(rt)+qnorm(0.05)*pred_norm[1]

1000*(1+VaR_rt_0.1)
1000*(1+VaR_rt_0.1)
1000*(1+VaR_rt_5)

#1달 VaR
VaR_rt22_0.1 <- 22*mean(rt)+qnorm(0.001)*sqrt(sum(pred_norm^2))
VaR_rt22_1 <- 22*mean(rt)+qnorm(0.01)*sqrt(sum(pred_norm^2))
VaR_rt22_5 <- 22*mean(rt)+qnorm(0.05)*sqrt(sum(pred_norm^2))

1000*(1+VaR_rt22_0.1)
1000*(1+VaR_rt22_1)
1000*(1+VaR_rt22_5)

#GJR-GARCH(1,1)
VaR_rt_0.1_2 <-mean(rt)+qnorm(0.001)*pred_norm2[1]
VaR_rt_1_2 <- mean(rt)+qnorm(0.01)*pred_norm2[1]
VaR_rt_5_2 <- mean(rt)+qnorm(0.05)*pred_norm2[1]

1000*(1+VaR_rt_0.1_2)
1000*(1+VaR_rt_0.1_2)
1000*(1+VaR_rt_5_2)

## 표준화된 t분포 error 가정시 손실액의 VaR
#GARCH(1,1)
#1일 VaR
VaR_rt_0.1_q <-mean(rt)+qt(0.001,5)*pred_norm[1]*sqrt(3/5)
VaR_rt_1_q <- mean(rt)+qt(0.01,5)*pred_norm[1]*sqrt(3/5)
VaR_rt_5_q <- mean(rt)+qt(0.05,5)*pred_norm[1]*sqrt(3/5)

1000*(1+VaR_rt_0.1_q)
1000*(1+VaR_rt_0.1_q)
1000*(1+VaR_rt_5_q)

#1달 VaR
VaR_rt22_0.1_q <- 22*mean(rt)+qt(0.001,5)*sqrt(sum(pred_norm^2))*sqrt(3/5)
VaR_rt22_1_q <- 22*mean(rt)+qt(0.01,5)*sqrt(sum(pred_norm^2))*sqrt(3/5)
VaR_rt22_5_q <- 22*mean(rt)+qt(0.05,5)*sqrt(sum(pred_norm^2))*sqrt(3/5)

1000*(1+VaR_rt22_0.1_q)
1000*(1+VaR_rt22_1_q)
1000*(1+VaR_rt22_5_q)

#GJR-GARCH(1,1)
VaR_rt_0.1_2_q <-mean(rt)+qt(0.001,5)*pred_norm2[1]*sqrt(3/5)
VaR_rt_1_2_q <- mean(rt)+qt(0.01,5)*pred_norm2[1]*sqrt(3/5)
VaR_rt_5_2_q <- mean(rt)+qt(0.05,5)*pred_norm2[1]*sqrt(3/5)

1000*(1+VaR_rt_0.1_2_q)
1000*(1+VaR_rt_0.1_2_q)
1000*(1+VaR_rt_5_2_q)

VaR_rt22_0.1_q2 <- 22*mean(rt)+qt(0.001,5)*sqrt(sum(pred_norm2^2))*sqrt(3/5)
VaR_rt22_1_q2 <- 22*mean(rt)+qt(0.01,5)*sqrt(sum(pred_norm2^2))*sqrt(3/5)
VaR_rt22_5_q2 <- 22*mean(rt)+qt(0.05,5)*sqrt(sum(pred_norm2^2))*sqrt(3/5)

1000*(1+VaR_rt22_0.1_q2)
1000*(1+VaR_rt22_1_q2)
1000*(1+VaR_rt22_5_q2)
