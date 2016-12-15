getBestArima <- function(p, d, q, P, D, Q, s=0, hypothesis=FALSE){
  min <- Inf
  arima_model <- ''
  
  # Rob Hyndman and George Athanasopoulos recommendation
  lag = 2*s
  if(s==0){lag=10}
  
  for(pi in p:2){
    for(di in d:2){
      for(qi in q:2){
        for(Pi in P:2){
          for(Di in D:2){
            for(Qi in Q:2){
              
              tryCatch({
                if(di+Di <= 2){
                  arima <- Arima(log(visados.ts), order=c(pi,di,qi), seasonal=list(order=c(Pi,Di,Qi), period=s))
                  rmse = accuracy(arima)[2]
                  if((t.test(arima$residuals)$p.value >= 0.05 
                     && Box.test(arima$residuals, lag=lag, fitdf=pi+qi+Pi+Qi)$p.value >= 0.05
                     # We assume there exists one outlier.
                     && jarque.bera.test(arima$residuals[-which.max(arima$residuals)])$p.value >= 0.05) || !hypothesis) {
                    if(rmse < min){
                      min <- rmse
                      arima_model <- arima
                    }
                  }
                }
                
              }, 
              # Sometimes the ARIMA model cannot be calculated with some fixed parameters.
              error=function(cond){})
            }
          }
        }
      }
    }
  }
  return(list("min"=min, "arima"=arima_model))
}