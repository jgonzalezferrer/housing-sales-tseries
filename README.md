# housing-sales-tseries: Modeling the new housing approvals in Spain from 1997 to 2013 as a Time Series in R.


The following is an analysis of time series data reflecting the number of new housing approvals per month over a timespan ranging from January 1997 to August 2013, as measured by the Spanish institution Banco de España (www.bde.es).

Installation
----------- 
Dependencies:
````
install.packages("forecast")
install.packages("car")
install.packages("tseries")
install.packages("astsa")

````

Time Series models
----------- 
We have applied different versions of statistical learning models, checking the LINE assumptions for each of them:

* [Decomposition methods](https://www.otexts.org/fpp/6/5): These methods describe the trend and seasonal factors in a time series. The function <i>stl()</i> decomposes the time series based on LOESS, a smoothing procedure that is based on local, weighted robust regression. 

* [ARIMA models](http://people.duke.edu/~rnau/arimrule.htm): The function <i>arima</i> will help us to identify and derive ARIMA models for modeling and forecasting time series. The ACF and PACF plots of the residuals will advice us how to distinguish the seasonality, degree of differencing, autoregressive models and moving average models.
