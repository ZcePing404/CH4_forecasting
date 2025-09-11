ARIMA_method <- function(ts_train, ts_test) {
  
  ndiffs(ts_train)
  nsdiffs(ts_train)
  
  # -------------------------------
  # Manual ARIMA
  # -------------------------------
  fit <- Arima(ts_train, order=c(2,1,0), seasonal=list(order =c(2,1,0), period=12))
  checkresiduals(fit)
  summary(fit)
  
  # Forecast for the manual fit arima
  fr <- forecast(fit, h = length(test))
  layout(matrix(c(1,1)))
  plot(fit)
  layout(matrix(c(1,1)))
  plot(fr)
  lines(ts_test, col="turquoise2")
  accuracy(fr, ts_test)
  
  # -------------------------------
  # Auto ARIMA
  # -------------------------------
  fit2 <- auto.arima(ts_train, seasonal = T, trace = T, approximation = F, ic="aic")            
  checkresiduals(fit2)
  summary(fit2)
  
  # Forecast for the auto fit arima
  fr <- forecast(fit2, h = length(test))
  layout(matrix(c(1,1)))
  plot(fit2)
  layout(matrix(c(1,1)))
  plot(fr)
  lines(ts_test, col="turquoise2")
  accuracy(fr, ts_test)
}