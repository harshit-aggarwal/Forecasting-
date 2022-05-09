setwd("~/Documents/GWU/Forecasting")

# Question 1

data <- read.table("US_Electricity_Usage.txt", header=TRUE)
attach(data)
head(data)

Electricity.dat <- ts(data=USAGE, start=c(1985,1), frequency=12)

detach(data)

ts.plot(Electricity.dat,ylab="US Electricity Usage",main="US Electricity Usage Jan 1985-Aug 2002")

boxplot(Electricity.dat/1~data$MONTH,xlab="Month",ylab="US Electricity Usage",col="blue")

# From the time series plot we can see that there is overall upwards trend to the Electricity Usage
# We can also see the electricity usage peaking during the middle of the year followed by a sharp decline
# From the monthly boxplot we can see that July/August have the highest electricity usage this could be due to A/C in the summer
# The next 2 closest months are December, January which could be due to heating being used in winter

acf(Electricity.dat,plot=TRUE,lag=48, main= "ACF of Electricity Usage")
# This is a non-stationary time series, with seasonality 
# We can see that month 0,6 and month 12,18 are highly correlated
# The seasonality can be seen as with month 1 and 2 having high usage and then declining until month 6,7,8 and the declining until month 1 of the next year

# Bartlett test
2*(1/sqrt(nrow(data)))
# If the autocorrelation at lag k is greater than 0.1373 we can reject the null hypothesis that it is 0
# The 2 standard error bounds are 0.1373

acf(Electricity.dat, lag=1, plot=FALSE)
# Autocorrelation at lag 1 is 0.695 hence we can conclude it is something other than 0
# The test statistic Z
sqrt(nrow(data))*0.695
# 10.119 > 2 (95% upper and lower tail value)

acf(Electricity.dat, lag=12, plot=FALSE)
# Autocorrelation at lag 12 is 0.840 hence we can reject the null hypothesis that it is 0
sqrt(nrow(data))*0.840
# 12.231 > 2 (95% upper and lower tail value)

Box.test(Electricity.dat, type=c("Ljung-Box"), lag=12)
# The null hypothesis is that all auto correlations up to lag 12 are 0.
# As the p-value is 2.2e-16, we can reject the null hypothesis and conclude the series is not white noise

# Question 2

data2 <- read.table("COCA COLA DIVIDENDS.txt", header=TRUE)
attach(data2)
head(data2)

coke <- ts(data=DIV, start=c(1984,1), frequency=12)

detach(data2)

acf(coke, plot=TRUE, lag=24, main="ACF plot of Coca Cola Dividends")

# A non-stationary series with an overall downwards trends
# Based on the acf plot we can conclude the series is not white noise as there is a downwards trend and 
# acf values are not 0, and are outside the 2 standard error bounds.

acf(diff(coke), plot=TRUE, lag=24, main="ACF of First Difference of Coca Cola Dividends")
# From the plot we can see that ACF at lag = 12 is outside the 2 standard error bounds hence we can say that this is not a white noise series

Box.test(diff(coke), type=c("Box-Pierce"), lag=24)
# The null hypothesis is that all autocorrelations of the first difference of the dividends of coke up to lag 24 are 0
# The p-value is 0.2155, we cannot reject the null hypothesis and cannot conclude that the series is not white noise

# As the first difference of coca-cola dividends is white noise based on the Box-Pierce test,
# we can conclude that cola-cola dividends is random walk

