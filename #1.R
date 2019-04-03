## part A.

data1 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/data1.csv")
colnames(data1) <- c("day","time","p","rt","r10")

library(dplyr)


# 1)
tt <- ts(data1$rt)
plot(data1$rt)
plot.ts(tt)

# 2)
acf(data1$rt[-1],lag.max=20)
pacf(data1$rt[-1],lag.max=20)

# 3)
acf(data1$rt[-1],lag.max=3)
pacf(data1$rt[-1],lag.max=3)

# 4)
result4_acf <- acf(na.omit(data1$r10),lag.max=20)
result4_sacf <- pacf(na.omit(data1$r10),lag.max=20)

result4_acf[1]
1/sqrt(36)

## part B.
data2 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/data2.csv",header=F)
colnames(data2) <- c("RV")

# 1)
plot.ts(data2$RV)

# 2)
acf(data2$RV,lag.max=20)
pacf(data2$RV,lag.max=20)

# 6)
data3 <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/data3.csv")
acf(data3$RV_10)
pacf(data3$RV_10)
