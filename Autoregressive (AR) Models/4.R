setwd("~/Documents/GWU/Forecasting/Assignment 4")
library(forecast)

data <- read.table(file="GNP_Quarterly.txt", header=TRUE)

attach(data)

growth <- ts(data=GROWTH, start=c(2, 1947))

detach(data)

ts.plot(growth)
acf(growth, plot=TRUE, main="ACF of growth data")

# From the time series plot we can see that the mean is constant in the series with no trend being observed
# From the ACF plot we can see a quick decay in values with only lag 1 and lag 2 being significant

pacf(growth, plot=TRUE, main="Partial ACF of growth data")

# From the Partial ACF plot we can see that only values at lag 1 are outside the 2 standard error bounds
# Hence we will use a autoregressive function of order 1

fit <- Arima(growth, order=c(1,0,0))
fit

phi=fit$coef
mu = phi[2]
C = mu*(1-phi[1])
# Estimated Intercept:
C

# Estimated Model:
# S(t) = 0.004776108 + 0.3786541*S(t-1)

se_phi = rep(0,2)

se_phi[1] = (fit$var.coef[1,1])^0.5
se_phi[2] = (fit$var.coef[2,2])^0.5

t_stat = phi/se_phi
t_stat

# As we can see the T-stat for both AR1 coefficient and the intercept is over 1.96 so we can reject the null hypothesis that they are 0.

acf(fit$resid, main="ACF of the residuals of the AR(1) model")
Box.test(fit$resid, lag=20)

# As we can see from the residuals plot none of the ACF values are significant except at lag 0
# We can see the p-value of the Box-Pierce test at 0.7639, having such a high values means we cannot reject the that all acf's are 0
# Based on the above factors we can conclude that the residuals are white noise

forecast(fit)
autoplot(forecast(fit,h=50))

# As we can see from the forecast plot as k gets large the k-step ahead forecast will approach its mean.
# This is due to the fact that lags are not correlated and as k gets large we will use the mean as the prediction