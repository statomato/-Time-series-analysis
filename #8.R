#6.4
Pt <- 120; sigma <- 0.5; r <- 0.07

#(a)
K <- 125
h_plus <- (log(Pt/K)+(r+sigma^2/2)*0.25)/(sigma*sqrt(0.25))
h_minu <- (log(Pt/K)+(r-sigma^2/2)*0.25)/(sigma*sqrt(0.25))
ct <- Pt*pnorm(h_plus)-K*exp(-r*0.25)*pnorm(h_minu)
h_plus; h_minu
ct

#(b)
K <- 118
h_plus <- (log(Pt/K)+(r+sigma^2/2)*0.25)/(sigma*sqrt(0.25))
h_minu <- (log(Pt/K)+(r-sigma^2/2)*0.25)/(sigma*sqrt(0.25))
pt <- K*exp(-r*0.25)*pnorm(-h_minu)-Pt*pnorm(-h_plus)
h_plus; h_minu
pt

#6.6
mu <- log(60)+(0.2-0.4^2/2)*2
sigma <- sqrt(2)*0.4
mu;sigma
lower <- mu-1.96*sigma; upper <- mu+1.96*sigma
cbind(lower,upper)
cbind(exp(lower),exp(upper))

#6.7
0.2-0.4^2/2
sqrt(0.4^2/2)



#HW 8
data <- read.csv("C:/대학원/2018-2/1. 전공/시계열분석/HW8_KOSPI_data.csv")

#1
rt <- diff(data$KOSPI)
delta <- length(data$KOSPI)

s1 <- sd(rt)/delta
m1 <- mean(rt)/delta+var(rt)/delta

m1; s1

#2
m2 <-(m1-s1^2/2)/12+log(data$KOSPI[delta])
s2 <- s1^2*12

m2; s2

#3
m3 <- m1-s1^2/2
s3 <- s1^2/12

m3; s3

#4
OptionPrice_call <- function(Pt, sigma, r, K, N){
  h_plus <- (log(Pt/K)+(r+sigma^2/2)*N)/(sigma*sqrt(N))
  h_minu <- (log(Pt/K)+(r-sigma^2/2)*N)/(sigma*sqrt(N))
  ct <- Pt*pnorm(h_plus)-K*exp(-r*N)*pnorm(h_minu)
  ct
}
OptionPrice_put <- function(Pt, sigma, r, K, N){
  h_plus <- (log(Pt/K)+(r+sigma^2/2)*N)/(sigma*sqrt(N))
  h_minu <- (log(Pt/K)+(r-sigma^2/2)*N)/(sigma*sqrt(N))
  pt <- K*exp(-r*N)*pnorm(-h_minu)-Pt*pnorm(-h_plus)
  pt
}

N <- 0.25
r1 <- 1.75; r2 <- 2.75
Pt <- data$KOSPI[delta]
K1 <- 1800; K2 <-2000; K3 <-2500


rt2 <- rt[-c(1:200)]
s2 <- sd(rt2)/length(rt2)
sigma1 <- s1; sigma2 <- s2
sigma1; sigma2

OptionPrice_call(Pt, sigma1, r1, K1, N); OptionPrice_put(Pt, sigma1, r1, K1, N)
OptionPrice_call(Pt, sigma1, r2, K1, N); OptionPrice_put(Pt, sigma1, r2, K1, N)
OptionPrice_call(Pt, sigma2, r1, K1, N); OptionPrice_put(Pt, sigma2, r1, K1, N)
OptionPrice_call(Pt, sigma2, r2, K1, N); OptionPrice_put(Pt, sigma2, r2, K1, N)

OptionPrice_call(Pt, sigma1, r1, K2, N); OptionPrice_put(Pt, sigma1, r1, K2, N)
OptionPrice_call(Pt, sigma1, r2, K2, N); OptionPrice_put(Pt, sigma1, r2, K2, N)
OptionPrice_call(Pt, sigma2, r1, K2, N); OptionPrice_put(Pt, sigma2, r1, K2, N)
OptionPrice_call(Pt, sigma2, r2, K2, N); OptionPrice_put(Pt, sigma2, r2, K2, N)

OptionPrice_call(Pt, sigma1, r1, K3, N); OptionPrice_put(Pt, sigma1, r1, K3, N)
OptionPrice_call(Pt, sigma1, r2, K3, N); OptionPrice_put(Pt, sigma1, r2, K3, N)
OptionPrice_call(Pt, sigma2, r1, K3, N); OptionPrice_put(Pt, sigma2, r1, K3, N)
OptionPrice_call(Pt, sigma2, r2, K3, N); OptionPrice_put(Pt, sigma2, r2, K3, N)

