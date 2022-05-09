setwd("~/Documents/GWU/Forecasting/Assignment 2")

data1 <- read.table(file="stock prices.txt", header=TRUE)

ge <- ts(data=data1$ge)

ts.plot(ge, ylab="GE Closing Price", main="GE Closing Price Aug 1994-October 1995")
acf(ge, plot=TRUE, main="ACF of GE Closing Price")

# From the plot we can see that the series has an upwards trajectory 
# From the acf plot we can see that the series is decaying very slowly
# We can conclude that the series is non-stationary

diffGE <- diff(ge)

acf( diffGE, plot=TRUE, main="First Diff of GE Prices")
Box.test(diffGE, type=c("Box-Pierce"))

# From the acf plot of the first difference of GE closing prices, we can see that no acf is outside the 2 standard error bounds
# The p-value from the Box-Pierce test is 0.8618 which means we cannot reject the null hypothesis that this series is white noise
# Based on the above 2 factors we can conclude that GE closing price is a random walk

fit <- lm(diffGE~1)
summary(fit)
C = fit$coef

# The drift term is very small at 0.007860, however the p-value is 0.138 
# Due to this we cannot reject the null hypothesis that the drift term is 0

n=length(ge)
PR_GE=0
PR_GE[1]=NA
error=0
ape=0

for (t in 2:n){
  PR_GE[t]=C+ge[t-1]
  error[t-1]=ge[t]-PR_GE[t]
  ape[t-1]= abs(error[t-1])/ge[t]
}

plot.ts(ge,ylab="ge",lwd=2,main="Actual versus Predicted Values of ge")
lines(PR_GE,col="red", type="b", lwd=1)

mean(ape)

PR_GE2=0
PR_GE2[1]=NA
error2=0
ape2=0

for (t in 2:n){
  PR_GE2[t]=ge[t-1]
  error2[t-1]=ge[t]-PR_GE2[t]
  ape2[t-1]= abs(error2[t-1])/ge[t]
}

plot.ts(ge,ylab="ge",lwd=2,main="Actual versus Predicted Values of ge")
lines(PR_GE2,col="blue", type="b", lwd=1)

mean(ape2)

# The mean absolute percentile error is very similar if drift term is considered or not
# MAPE with drift term: 0.009224 (0.922%)
# MAPE without drift term: 0.009105 (0.911%)
# As the MAPE without the drift is just slightly better and we are cannot reject the null hypothesis that the drift is 0 
# then the model without the drift term is justified 

data2 <- read.table(file="Price of Gas.txt", header=TRUE)

gas <- ts(data=data2, start=c(1974,1))

ts.plot(gas, ylab="Gas Price", main="Gas Price from Jan 1974-December 1982")
acf(gas, plot=TRUE, main="ACF of Gas Prices")

# From the plot we can see that the series has an upwards trajectory 
# From the acf plot we can see that the series is decaying very slowly
# We can conclude that the series is non-stationary

diffGas <- diff(gas)

acf( diffGas, plot=TRUE, main="First Diff of Gas Prices")

# From the first difference plot we can see that there is siginificant ACF at lag 1
# This implies that this is not white noise
# We can further confirm using the Box-Pierce Test

Box.test(diffGas, type=c("Box-Pierce"))

# The p-value us 2.105e-07 hence we can reject the null hypothesis that all autocorrelations are 0
# This leads to the conclusion that the first difference is not white noise
# As the first difference is not white noise we cannot say that the monthly price of gas is a random walk
