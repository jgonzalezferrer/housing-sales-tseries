setwd("Universidad/Data Science/Intelligent Data Analysis/Homework set 3/")
# JAVIER
 setwd("D:/Google Drive/EIT/Intelligent Data Analysis/homeworks/homework_set3")

# Load packages
library(forecast)
library(astsa)
library(tseries)
library(car)
library(astsa)

# PARTE 1
# Read the data
visados.df <- read.csv("data/data_g2.csv", header=T, sep=",", stringsAsFactors = FALSE)

# Create TS object, starting from 1997
visados.ts <- ts(visados.df$Visados, start=c(1997,1), end=c(2013, 8), frequency = 12)

# 1- PLOT AND COMMENT
plot(visados.ts)

# COMMENT: Parece haber seasonality al principio, cuando ademas hay trend ascendente.
# Luego se va a la puta

# Otro tipo de plot con mas opciones
tsdisplay(visados.ts, plot.type="scatter")

#PARTE 2
par(mfrow=c(2,1))
plot(visados.ts)
plot(log(visados.ts))
par(mfrow=c(1,1))
## DECOMPOSITION
# reference: https://anomaly.io/seasonal-trend-decomposition-in-r/
#When the time series increase the seasonal variation increase more and more. We should use the multiplicative model.

#Multiplicative composition = additive composition + log(data)
stl.visados=stl(log(visados.ts), s.window="periodic", robust=TRUE)
plot(stl.visados)

# Real vs. fitted
# RECALCAR QUE ES EL REAL SIN EL LOG PORQUE TODAVIA NO HEMOS HECHO LA TRANSFORMACION.
# para ello hay que ajustar el stl puesto que es multiplicative (log => exp: operacion inversa)
par(mfrow=c(2,1))
plot(visados.ts)
plot(exp(seasadj(stl.visados))) 

#forecasting with stl()
# mirar en el script donde este esto y ponerl oparecido
fcst=forecast(stl.visados, method="naive")
plot(fcst)
# COMMENT: does not look very fancy...

#Does
#the remainder look like a white noise to you? White noise is just a group of independent,
#identically distributed variables, with zero mean and constant variance. Answer to this
#point just visually or plot the ACF and PACF of the remainder part
tsdisplay(stl.visados$time.series[,c("remainder")])
#The correlogram of a stationary time series DOES NOT go to zero quickly.

# EHCARLE OTRO OJO A ESTO DE ARRIBA.

#PARTE 3

# Vamos a ver si aplicando alguna transformacion esto mejora
BoxCox.lambda(visados.ts, lower=0, upper=2)
# Very near to 0, let us consider de log transformation.
tsdisplay(log(visados.ts), plot.type="scatter")
plot(log(visados.ts))

# (Parcial) Autocorrelation function
tsdisplay(visados.ts)
tsdisplay(log(visados.ts))

#b
tsdisplay(log(visados.ts), plot.type = "spectrum")
acf2(log(visados.ts))
# PACf: no se si tiene que ver, pero lag 12 y 24 estan correlacionados.
# en acf todos estan correlacionados
# veamos algunos plots antes de empezar a diferenciar
seasonplot(log(visados.ts))
# si no estuvieran correlacionadas serian todas las lineas rectas, pero en Agosto se ve una diferencia.
monthplot(log(visados.ts))


#ACF does not drop quickly to values inside or almost inside the critical values. 
#Moreover the value r1r1 is large and positive (almost 1 in this case). 
#All this are signs of a non-stationary time series. PACF value r1r1 is almost 1. 
#This is also a sign of non-stationary process. 
#Therefore the data should be differenced


# COMMENT: Si vamos estrictamente como dice la teoría, como tenemos seasionality
#hay que aplicar primero season, y dps diferenciar.
visados.ts.diff1_12 = diff(log(visados.ts), lag = 12)
plot(visados.ts.diff1_12)
acf2(visados.ts.diff1_12)
sd(visados.ts.diff1_12)
adf.test(visados.ts.diff1_12) # p-value, no stationary

# COMMENT: No es suficiente, asi que diferenciamos otra vez
visados.ts.diff2_12 = diff(diff(log(visados.ts), lag = 12))
plot(visados.ts.diff2_12)
acf2(visados.ts.diff2_12)
sd(visados.ts.diff2_12)
adf.test(visados.ts.diff2_12) # p-value pequeño, si stationary

# Seguimos bajando?
sd(diff(visados.ts.diff2_12))
# Respuesta: no porque la sd vuelve a subir.

# SIN EMBARGO!! CON SOLO UNA DIFF LOGRAMOS STATIONARY SIN USAR SEASONALITY
visados.ts.diff1 = diff(log(visados.ts))
plot(visados.ts.diff1)
acf2(visados.ts.diff1)
sd(visados.ts.diff1)
adf.test(visados.ts.diff1) #si stationary

sd(diff(visados.ts.diff1))

# CON 1: la sd es un poco mayor.
# CON 2: los picos del grafico son mas grandes -> implica seasonality
# CON 3: no usamos seasionality ?¿¿?(esto es malo)?¿?¿
# PRO 1: es mas simple porque solo un diff ?¿?¿(esto es cierto)?¿?¿

# PARTE 3.
# MA O AM.
acf2(diff(visados.ts))
acf2(diff(diff(visados.ts,12)))

# Parece que se necesita un MA para los dos, porque tiene un pico negativo en ACF y varios picos negativos en PACF 
# y sinusoidal pattern

model1 = Arima(log(visados.ts), order=c(0,1,0), seasonal=list(order=c(0,1,0), period=12))
model1$aic
model1 = Arima(log(visados.ts), order=c(1,1,0), seasonal=list(order=c(0,1,0), period=12)) #better
model1$aic
model1 = Arima(log(visados.ts), order=c(2,1,0), seasonal=list(order=c(0,1,0), period=12)) #best
model1$aic
model1 = Arima(log(visados.ts), order=c(3,1,0), seasonal=list(order=c(0,1,0), period=12)) #NO
model1 = Arima(log(visados.ts), order=c(2,1,1), seasonal=list(order=c(0,1,0), period=12)) #NO, worst
model1$aic


model1 = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,1,0), period=12)) #gana este es igual a 2,1,0 pero tiene
# menos parametros

#CHAMPION
model1 = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,1,2), period=12))
model1$aic


# HIPOTESIS DE NO TENER SEASON COMPONENT... MAL
model1 = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,0,0)))
model1$aic


#residuals
model1 = Arima(log(visados.ts), order=c(0,1,1), seasonal=list(order=c(0,1,2), period=12))

plot(model1$residuals)
t.test(model1$residuals) 
Box.test(model1$residuals, lag=12, fitdf=3, type="L") #independence ok
jarque.bera.test(model1$residuals) #mal
which.max(model1$residuals)
jarque.bera.test(model1$residuals[-117]) #si le quito el outlier se hace normal
t.test(model1$residuals[-117]) 



model1 = Arima(log(visados.ts), order=c(1,1,2), seasonal=list(order=c(1,0,2), period=12))
model1$aic
acf2(model1$residuals)

auto.arima(log(visados.ts))
model1= auto.arima(log(visados.ts))
cov2cor(model1$var.coef)
acf2(model1$residuals)
plot(model1$residuals)
t.test(model1$residuals) 

 #independence ok
jarque.bera.test(model1$residuals) #mal

# more models

model1 = Arima(log(visados.ts), order=c(0,1,0), seasonal=list(order=c(0,0,0)))
model1$aic
