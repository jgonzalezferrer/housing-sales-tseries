library(forecast)
library(astsa)
library(tseries)
library(car)

# User-defined function to obtain the best ARIMA model.
source("getBestArima.R")

# Loading the data
visados.df <- read.csv("../data/data_g2.csv", header=T, sep=",", stringsAsFactors = FALSE)
# Create a Time Series object, starting from 1997 and seasonal data.
visados.ts <- ts(visados.df$Visados, start=c(1997,1), end=c(2013, 8), frequency = 12)

# Plot original data.
plot(visados.ts)

## Decomposition methods

# Multiplicative decomposition
visados.stl <- stl(log(visados.ts), s.window="periodic")
plot(visados.stl)

# Extracting and ploting the remainder part.
visados.stl.remainder <- visados.stl$time.series[,c("remainder")]
tsdisplay(visados.stl.remainder)

# Box-Pierce test for independence of a time series.
Box.test(visados.stl.remainder, lag=12) 

# Forecasting new values.
plot(forecast(visados.stl, method="naive"))

## ARIMA model

# Data transformation

par(mfrow=c(2,1))
plot(visados.ts)
plot(log(visados.ts))
par(mfrow=c(1,1))

```{r boxcox, include=FALSE}
# Which lamba to use to transform the variable.
BoxCox.lambda(visados.ts, lower=0, upper=2)

# Seasonality

# Periodogram of the time series.
tsdisplay(log(visados.ts), plot.type = "spectrum")

# Season and month plots.
par(mfrow=c(1,2))
seasonplot(log(visados.ts))
monthplot(log(visados.ts))
par(mfrow=c(1,1))

# Differencing

# Initial standard deviation
sd(log(visados.ts))

# One order of differencing.
sd(diff(log(visados.ts)))
# Augmented Dickey–Fuller Test for stationarity.
adf.test(diff(log(visados.ts)))

# ACF and PACF of the differentiated time series.
tsdisplay(diff(log(visados.ts)))

# Two orders of differencing.
sd(diff(diff(log(visados.ts))))

# One order of seasonal differencing.
sd(diff(log(visados.ts), 12))
# Checking stationarity
adf.test(diff(log(visados.ts), 12))
# ACF and PACF of the seasonal differentiated time series.
tsdisplay(diff(log(visados.ts), 12))

# One order of non seasonal differencing and one order of seasonal differencing.
sd(diff(diff(log(visados.ts), 12)))
adf.test(diff(diff(log(visados.ts), 12)))
tsdisplay(diff(diff(log(visados.ts), 12)))

# Standard deviation.
sd(diff(diff(diff(log(visados.ts), 12))))

# ndiffs and nsdiffs
# Built-in functions to calculate the number of differences required for a stationary series.
# KPSS test.
ndiffs(log(visados.ts), test="kpss")
# ADF test.
ndiffs(log(visados.ts), test="adf")
# Seasional differences.
nsdiffs(log(visados.ts), m=12)

# AR and MA terms
# ARIMA(0,1,1)(0,1,0)
model_ma = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,1,0), period=12))
tsdisplay(model_ma$residuals)

# ARIMA(0,1,1)(0,1,1)
model_sma = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
tsdisplay(model_sma$residuals)

# Statistical assumptions
# ARIMA(0,1,1)(0,1,1)
model0 = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
# One-sample t-test
t.test(model0$residuals)

# Box-Pierce test
Box.test(model0$residuals, lag=24, fitdf=2)

# Jarque bera test for normality
jarque.bera.test(model0$residuals)

# Checking normality without greatest outlier.
which.max(model0$residuals)
jarque.bera.test(model0$residuals[-117])

res = acf2(model0$residuals)

## Alternative 1: Improve RMSE
# "Best RMSE model" maintaining some fixed parameters.
# ARIMA(2,1,1)(1,1,1)
model1 = Arima(log(visados.ts), order=c(2,1,1), seasonal=list(order=c(1,1,1), period=12))
summary(model1)

t.test(model1$residuals)
Box.test(model1$residuals, lag=24, fitdf=5)

# Find the outlier and remove it
jarque.bera.test(model1$residuals[-which.max(model1$residuals)])

## Alternative 2: Improving based on hypothesis-meeting

# "Best RMSE model" in terms of hypothesis.
# ARIMA(1,2,2)(1,0,2)
model2 = Arima(log(visados.ts), order=c(1,2,2), seasonal=list(order=c(1,0,2), period=12))
summary(model2)

t.test(model2$residuals)

Box.test(model2$residuals, lag=24, fitdf=6, type="L")
# Remove the outlier and test again
jarque.bera.test(model2$residuals[-which.max(model2$residuals)])
res = acf2(model2$residuals)

## Alternative 3: auto.arima()
# Auto Arima model.
model3 = auto.arima(log(visados.ts))
summary(model3)

t.test(model3$residuals)
Box.test(model3$residuals, lag=24, fitdf=3)
# Remove the outlier and test again
jarque.bera.test(model3$residuals[-which.max(model3$residuals)])

# Forecasting
# Forecasting new values based on the ARIMA best model.
plot(forecast(model2))