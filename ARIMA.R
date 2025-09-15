ARIMA_method <- function() {

  # -------------------------------
  # ARIMA 1
  # -------------------------------
  fit <- arima(ts_train, order=c(0,1,0), seasonal=list(order =c(2,1,1), period=12))
  checkresiduals(fit)
  summary(fit)

  # Forecast for the manual fit arima
  fr <- forecast(fit, h = length(test))
  forecast::accuracy(fr, ts_test)
  
  layout(matrix(c(1,1)))
  plot(ts_data, 
       main = "ARIMA Model Fit and Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  lines(fitted(fit), col = "red")
  lines(fr$mean, col = "blue")
  polygon(c(time(fr$mean), rev(time(fr$mean))),
          c(fr$lower[,2], rev(fr$upper[,2])),   # 95% CI (2nd column)
          col = rgb(0, 0, 1, 0.2), border = NA)
  abline(v = start(ts_test), lty = 2, col = "grey")
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast", "95% CI"), 
         col = c("black", "red", "blue", rgb(0,0,1,0.2)), 
         lty = c(1,1,1, NA), 
         pch = c(NA, NA, NA, 15),
         pt.cex = 2,
         cex = 0.8)

  
  
  
  
  
  # -------------------------------
  # ARIMA 2
  # -------------------------------
  fit2 <- auto.arima(ts_train, seasonal = T, 
                     stepwise = T, 
                     trace=T, approximation = F, 
                     max.p = 2, 
                     max.q = 2, 
                     max.P = 2, 
                     max.Q = 2, 
                     ic = "aic")
  checkresiduals(fit2)
  summary(fit2)
  
  # Forecast for the manual fit arima
  fr <- forecast(fit2, h = length(test))
  forecast::accuracy(fr, ts_test)
  
  layout(matrix(c(1,1)))
  plot(ts_data, 
       main = "ARIMA Model Fit and Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  lines(fitted(fit), col = "red")
  lines(fr$mean, col = "blue")
  polygon(c(time(fr$mean), rev(time(fr$mean))),
          c(fr$lower[,2], rev(fr$upper[,2])),   # 95% CI (2nd column)
          col = rgb(0, 0, 1, 0.2), border = NA)
  abline(v = start(ts_test), lty = 2, col = "grey")
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast", "95% CI"), 
         col = c("black", "red", "blue", rgb(0,0,1,0.2)), 
         lty = c(1,1,1, NA), 
         pch = c(NA, NA, NA, 15),
         pt.cex = 2,
         cex = 0.8)
}