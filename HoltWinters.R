HoltWinters_method <-function(ts_train,ts_test){
  es1 <- HoltWinters(ts_train)
  es1
  
  es1
  # Check randomness of the residuals
  checkresiduals(es1)
  
  # Forecasting
  f1 <- forecast(es1, h=length(test))
  
  # Compare performance
  accuracy(f1, ts_test)
  
  # Plot
  forecast <- predict(es1, n.ahead=length(test),prediction.interval=T,level=.95)
  plot(es1,forecast,ylim=c(380,430))
  lines(test, col="turquoise2")
}