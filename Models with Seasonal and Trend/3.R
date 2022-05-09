setwd("~/Documents/GWU/Forecasting/Assignment 3")
library(greybox)
library(forecast)

data <- read.table(file="US_Electricity_Usage.txt", header=TRUE)

attach(data)

ts.plot(USAGE,ylab="US Electricity Usage",main="US Electricity Usage Jan 1985-Aug 2002")

boxplot(USAGE/1~MONTH,xlab="Month",ylab="US Electricity Usage",col="blue")

n_USAGE=USAGE[1:188]
n_MONTH=MONTH[1:188]

fit<-lm(n_USAGE~as.factor(n_MONTH))
summary(fit)

# The intercept reflect the usage in the month og January which is used as a reference month
# Each coefficient associated with the months is the expected change on usage during that month on average
# The coefficient of July is 8.574 with a p-value of 0.49968, due to a high p-value we cannot say that the average usage in July different than the average usage during January
# The coefficient of September is -33.975 with a p-value of 0.00912, as it is below 0.05 we can say that usage in September is 33.975 units below usage in January on average

plot.ts(n_USAGE, main="Actual versus Predicted Usage",ylab="Usage", col="blue")
lines(predict(fit),col="red")

pred=predict(fit, data.frame(n_MONTH=MONTH[189:212]), interval="prediction")

mape=mean(abs(USAGE[189:212]-pred[,1])/USAGE[189:212])
mape

time<-seq(1, length(n_USAGE))
fit2<-lm(n_USAGE~time+as.factor(n_MONTH))
summary(fit2)

# The coefficient of July is 5.0724 with a p-value of 0.331, due to a high p-value we cannot say that the average usage in July different than the usage during January on average beyond trend (controlling for trend)
# The coefficient of September is -35.1418 with a p-value of 3.78e-10, as it is below 0.05 we can say that usage in September is 35.1418 units below usage in January on average beyond trend (controlling for trend)

plot.ts(n_USAGE, main="Actual versus Predicted Usage",ylab="Usage", col="blue")
lines(predict(fit2),col="red")

pred=predict(fit2, data.frame(n_MONTH=MONTH[189:212], time=c(189:212)), interval="prediction")

mape=mean(abs(USAGE[189:212]-pred[,1])/USAGE[189:212])
mape

# MAPE with only seasonal dummy variables : 19.805%
# MAPE with seasonal dummy variables and trend : 4.72%
# If we see the plots the predicted usage of the model with dummy seasonal variables and trend variable is a much better fit
# This can be seen from the MAPE also.

# Yes there is on the analysis there is both trend and seasonality in the series
# As we can see from fit2 the trend term (time) is has a p-value < 2e-16, this means we can reject the null hypothesis that there is no trend in the series
# We can see from the p-value of dummy seasonal variables that out of 11, 9 of them have p-values lower than 0.05, so we can reject the null hypothesis that the usage is the same throughout the year beyond trend

# If we use December as reference month, the coefficient for January will be
# (273.2567 - 0)  - (273.2567- 39.7391) = 39.7391

# The coefficient for September will be:
# (273.2567-35.1418) - (273.2567-39.7391) = 4.5973

