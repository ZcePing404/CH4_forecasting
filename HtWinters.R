HoltWinters_method <-function(ts_train, ts_test){
  es1 <- HoltWinters(ts_train,alpha = 0.368793, beta = 0.03007761, gamma = 0.5768063)
  es1
  
  checkresiduals(es1)
  
  fc <- forecast(es1, h= length(test))
  accuracy(fc, test)
  
  forecast <- predict(es1, n.ahead=length(test), prediction.interval=T, level=.95)
  plot(es1,forecast,ylim =c(360, 450))
  lines(test, col = "turquoise2")
}