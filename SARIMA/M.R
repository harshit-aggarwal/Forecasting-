library(greybox)
library(forecast)

setwd("~/Documents/GWU/Forecasting/Final")

data <- read.table(file="DC COVID CASES.txt", header=TRUE)
attach(data)
cases <- ts(data$Cases, start=c(1,2021), frequency = 7)
par(mfrow=c(1,2))
ts.plot(cases, main="Covid Cases in DC")
acf(cases, main="ACF of Covid Cases")

dcases <- diff(cases)
par(mfrow=c(1,2))
acf(dcases)
pacf(dcases)

sarima <- Arima(cases, order= c(0,1,1), seasonal=list(order=c(1,0,0), period=7), lambda=0)
sarima

par(mfrow=c(1,1))
acf(sarima$residuals, main="Residuals of SARIMA model")
Box.test(sarima$residuals)

preds=fitted(sarima)
plot.ts(cases,col="blue",type="b",lwd=1, main="Comparison of SARIMA and Actual")
lines(preds,col="red",lwd=2)

forecast(sarima, h=1)

# MAPE from model 1
mean(abs((Cases-preds)/Cases)) * 100

time <- seq(1, length(Date))
fit2 <- lm(Cases~as.factor(Day)+time)
summary(fit2)

pred2 = predict(fit2)
par(mfrow=c(1,2))
ts.plot(cases,col="blue",type="b",lwd=1)
ts.plot(pred2, ylab="Predicted values", main="Graph of model with trend and dummy variables")

# MAPE from model 2
mean(abs((Cases-pred2)/Cases)) * 100
