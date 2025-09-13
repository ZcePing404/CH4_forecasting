ARIMA_method <- function() {

  
  # -------------------------------
  # ARIMA 1
  # -------------------------------
  fit <- arima(ts_train, order=c(0,1,0), seasonal=list(order =c(2,1,0), period=12))
  checkresiduals(fit)
  summary(fit)

  # Forecast for the manual fit arima
  fr <- forecast(fit, h = length(test))
  layout(matrix(c(1,1)))
  plot(fit)
  layout(matrix(c(1,1)))
  plot(fr)
  lines(ts_test, col="turquoise2")
  forecast::accuracy(fr, ts_test)
  
  plot(ts_data, 
       main = "ARIMA Model Fit and Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  # Correctly access train using the fitted() function
  lines(fitted(fit), col = "red")
  lines(fr$mean, col="blue")
  abline(v = start(ts_test), lty = 2, col = "grey")
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast"), 
         col = c("black", "red", "blue"),
         lty = 1,
         cex = 0.8)
  
  # -------------------------------
  # ARIMA 2
  # -------------------------------
  fit2 <- arima(ts_train, order=c(1,1,0), seasonal=list(order =c(2,1,0), period=12))
  checkresiduals(fit2)
  summary(fit2)
  
  # Forecast for the manual fit arima
  fr <- forecast(fit2, h = length(test))
  layout(matrix(c(1,1)))
  plot(fit2)
  layout(matrix(c(1,1)))
  plot(fr)
  lines(ts_test, col="turquoise2")
  forecast::accuracy(fr, ts_test)
  
  plot(ts_data, 
       main = "ARIMA Model Fit and Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  # Correctly access train using the fitted() function
  lines(fitted(fit2), col = "red")
  lines(fr$mean, col="blue")
  abline(v = start(ts_test), lty = 2, col = "grey")
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast"), 
         col = c("black", "red", "blue"),
         lty = 1,
         cex = 0.8)
}