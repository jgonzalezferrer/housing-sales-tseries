setwd("Universidad/Data Science/Intelligent Data Analysis/Homework set 3/")

# Load packages
library(forecast)
library(astsa)
library(tseries)
library(car)

# Read the data
visados.df <- read.csv("data/data_g2.csv", header=T, sep=",", stringsAsFactors = FALSE)

# Dates
LC_TIME <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") # Hay que cambiar esto para que entienda meses en ingles
visados.df$Fecha <- as.Date(x=paste("1-",visados.df$Fecha, sep=""), format="%d-%b-%Y")
Sys.setlocale("LC_TIME", LC_TIME) # Lo devolvemos a como estuviera

# Create TS object, starting from 1997
visados.ts <- ts(visados.df$Visados, start=1997, frequency = 12)

# 1- PLOT AND COMMENT
plot(visados.ts)

# Parece haber seasonality al principio, cuando ademas hay trend ascendente.
# Luego se va a la puta

# (Parcial) Autocorrelation function
par(mfrow=c(2,1))
acf(visados.ts)
pacf(visados.ts)
par(mfrow=c(1,1))

vdf <- read.csv("data/data_g2.csv", header=T, sep=",", stringsAsFactors = FALSE)
dates <- as.yearmon(vdf$Fecha, "%b-%y")
ser <- zoo(visados.df$Visados, visados.df$Fecha)   # s is the series, time is the index vector
str(ser)   # ser is of class zoo
plot(ser)
