library(Quandl)
## Loading required package: xts
## Loading required package: zoo
##
## Attaching package: 'zoo'
## The following objects are masked from 'package:base': ##
## as.Date, as.Date.numeric
data0 = Quandl("BLSE/CEU9000000001", api_key="x3GfXqvJWsoBc1xZ1e2C") #reverse order of rows so dates are increasing
data0<-data0[dim(data0)[1]:1,]
head(data0)
## Date Value
## 970 1939-01-31 3958
## 969 1939-02-28 3947
## 968 1939-03-31 3971
## 967 1939-04-30 3985
## 966 1939-05-31 4032
## 965 1939-06-30 4035
tail(data0)
## Date Value
## 6 2019-05-31 22839
## 5 2019-06-30 22443
## 4 2019-07-31 21286
## 3 2019-08-31 21652
## 2 2019-09-30 22630
## 1 2019-10-31 23021
#data is from june 30 1939 to october 31 2019, we will remove the last year (2019) of data to determine our forecasting accuracy
data0 = ts(data0[,2])
data = data0[-c(1:10)]
#plot of the unemployment
plot.ts(data,xlab="Month", ylab="Unemployment", xlim=c(0,1000), main="Figure 1: Employment June 30 1939 to December 31 2018")
axis(1, seq(0,100,100))
acf(data, main="ACF", lag.max=1000)
(data, main="PACF", lag.max=100)
library(MASS)
bcTransform = boxcox(data ~ as.numeric(1:length(data)))
lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))] title("Figure 3: Log-Likelihood of Box-Cox Transformation")
bcdata = (1/lambda)*(data^lambda - 1) #Boc-cox
#label this figure 4
plot.ts(bcdata, main="Box-Cox Transformed Data", ylab=expression(Y[t]))
acf(bcdata, main="ACF of Box-Cox Transformed Data", lag.max=1000) pacf(bcdata, main="PACF of Box-Cox Transformed Data")
#label this figure 5
logdata = log(data)
acf(logdata, main="ACF of Log-Transformed Data", lag.max=1000)
pacf(logdata, main="PACF of Log-Transformed Data")
var(data)
## [1] 37300735 var(bcdata)
## [1] 31195044692 var(logdata)
## [1] 0.2496415
#label this figure 6
datax = ts(data, frequency=12)
fit = stl(datax, s.window='periodic') plot(fit)
title("Decomposition of Data")
#label this figure 7 #deseasonalize @ lag 12 d12 = diff(logdata, 12) var(d12)
## [1] 0.0009973204
plot.ts(d12, main="Differenced Data at Lag = 12",
        ylab=expression(paste(nabla^12, data)))
acf(d12, lag.max=1000, main="ACF of Differenced Data at Lag=12") pacf(d12, main="PACF of Differenced Data at Lag=12")
#label this figure 8
d1 = diff(d12, 1) var(d1)
## [1] 3.283604e-05
plot.ts(d1, main="Differenced Data at Lag = 12",
        ylab=expression(paste(nabla^12, data)))
acf(d1, lag.max=1000, main="ACF of Differenced Data at Lag=1") pacf(d1, main="PACF of Differenced Data at Lag=1") library(tseries)
## Warning: package 'tseries' was built under R version 3.5.2 adf.test(d1)
## Warning in adf.test(d1): p-value smaller than printed p-value
##
## Augmented Dickey-Fuller Test ##
## data: d1
## Dickey-Fuller = -8.6464, Lag order
## alternative hypothesis: stationary
pacf(d1, lag.max = 50)
library(astsa)
## Warning: package 'astsa' was built
= 9, p-value = 0.01
under R version 3.5.2
fit1 = arima(logdata,order = c(3,1,5), seasonal = list(order = c(0,1,1) , period = 12))
fit1
##
## Call:
##
1,
##
##
##
ma5
##
.0191
## s.e. 0.5533 0.9825 0.4774 0.5531 .0391
##
##
##
##
## 295.24
sma1 -0.3881 0.0323
arima(x = logdata, order = c(3, 1, 5), seasonal = list(order = c(0, 1), period = 12))
Coefficients:
  ar1 ar2 ar3 ma1
ma2 ma3 ma4 1.2170 -0.1232 -0.0029 -0 0.9175 0.3849 0.1026 0
s.e.
sigma^2 estimated as 2.579e-05: log likelihood = 3657.62, aic = -7
2.0989 -1.4397 0.2859 -1.9786
fit2 = arima(logdata,order = c(3,1,3), seasonal = list(order = c(0,1,1) , period = 12))
fit2
##
## Call:
##
1,
##
##
##
##
##
##
arima(x = logdata, order = c(3, 1, 3), seasonal = list(order = c(0, 1), period = 12))
Coefficients:
  ar1 ar2 ar3 ma1
1.8717 -1.0548 0.1067 -1.7511 s.e. 0.2543 0.4835 0.2533 0.2560
ma2 ma3 sma1
0.8604 0.4775
0.0235 -0.3905 0.2494 0.0317
## sigma^2 estimated as 2.58e-05: log likelihood = 3657.52, aic = -72 99.03
fit3 = arima(logdata,order = c(1,1,1), seasonal = list(order = c(0,1,1) , period = 12))
fit3
##
## Call:
## arima(x = logdata, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
##
## Coefficients:
## ar1 ma1 sma1
## 0.8824 -0.7591 -0.4135
## s.e. 0.0359 0.0479 0.0302
##
## sigma^2 estimated as 2.672e-05: log likelihood = 3641.2, aic = -72 74.41
sarima(logdata,p = 3,d = 1,q = 3, P = 0, D = 1, Q = 1, S = 12) hist(residuals(fit2),main="")
shapiro.test(residuals(fit2))
vals = sarima.for(logdata[1:(length(logdata)-12)], n.ahead = 24, p = 3, d = 1,q = 3, P = 0, D = 1, Q = 1, S = 12)
plot.ts(data, xlim = c(800,length(data)+12), ylim = c(10000,25000)) points(exp(vals$pred), col = 'RED', cex = 0.5)
forecast_error_up <- exp(vals$pred+1.96*vals$se)
forecast_error_low <- exp(vals$pred-1.96*vals$se) 
lines(forecast_error_up,lty=2,col="BLUE") 
lines(forecast_error_low,lty=2,col='BLUE')
