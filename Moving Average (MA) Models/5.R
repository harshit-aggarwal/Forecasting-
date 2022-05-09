setwd("~/Documents/GWU/Forecasting/Assignment 5")
library(greybox)
library(forecast)

data <- read.table(file="SALES.txt", header=TRUE)

sales <- ts(data$SALES, start=c(1,1980))

acf(sales,plot=TRUE, main="ACF of Sales")

# As we can see the ACF is decaying very slowly with values at many lags above the 2 standard error line hence we can conclude that this series is non-stationary

D_sales <- diff(data$SALES)

acf(D_sales, plot=TRUE, main="ACF of first difference of Sales")

# We can see that the ACF at lag 1 is outside the 2 standard error bounds however the series chops off to 0 right after hence we can say it is stationary

pacf(D_sales, main="Partial ACF of first difference of Sales")

# As the Partial ACF is decaying slowly we will use a Moving Average model for this series
# From the ACF plot we can see the values cuts off to 0 after lag 1, hence we will use moving average series of order 1

fit=Arima(sales, order=c(0,1,1))
fit

# M(t) = M(t-1) - 0.5998*ε(t-1)

epsilon = fit$residuals

acf(epsilon, plot=TRUE, main="ACF of residuals of MA(1) model")
Box.test(epsilon)


# As we can see no residuals except at lag 0 are outside the 2 standard error bounds
# The p-value value from the box test is 0.7127 so we cannot reject that this series is white noise
# Based on the above two factors we can conclude that the residuals are white noise

# One step ahead forecast

# M(t+1) = 4976 - 0.5998*-31.6270266 = 4994.969

# Two step ahead forecast

# M(t+2) = 4994.969 - 0.5998*ε(t+1) = 4994.969
# As we don't have the error from one step in the future the forecast will be the previous step value 




