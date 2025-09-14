HoltWinters_method <-function(){

  es1 <- HoltWinters(ts_train,alpha = 0.9, beta = 0.02, gamma = 1)
  es1
  
  checkresiduals(es1)
  
  fc <- forecast(es1, h=length(test))
  forecast::accuracy(fc, ts_test)
  
  forecast <- predict(es1, n.ahead=length(test), prediction.interval=T, level=.95)
  
  layout(matrix(c(1,1)))
  plot(ts_data, 
       main = "HltWinters Model Fit and Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  lines(es1$fitted[, "xhat"], col = "red")
  lines(fc$mean, col = "blue")
  polygon(c(time(fc$mean), rev(time(fc$mean))),
          c(fc$lower[,2], rev(fc$upper[,2])),   # 95% CI (2nd column)
          col = rgb(0, 0, 1, 0.2), border = NA)
  abline(v = start(ts_test), lty = 2, col = "grey")
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast", "95% CI"), 
         col = c("black", "red", "blue", rgb(0,0,1,0.2)), 
         lty = c(1,1,1, NA), 
         pch = c(NA, NA, NA, 15),
         pt.cex = 2,
         cex = 0.8)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Set the range of alpha values to test
  scales_to_test <- seq(from = 0.1, to = 1, by = 0.05)
  
  # Store results
  results <- data.frame(
    alpha = numeric(),
    train_rmse = numeric(),
    test_rmse = numeric()
  )
  
  # Loop through each alpha value
  for (s in scales_to_test) {
    m <- HoltWinters(ts_train, alpha = s, beta = 0.02, gamma = 1)
    
    train_rmse <- sqrt(mean((ts_train - m$fitted[, "xhat"])^2, na.rm = TRUE))
    fr <- forecast(m, h = length(ts_test))
    test_rmse <- sqrt(mean((ts_test - fr$mean)^2, na.rm = TRUE))
    results <- rbind(results, data.frame(
      alpha = s,
      train_rmse = train_rmse,
      test_rmse = test_rmse
    ))
  }
  
  plot(results$alpha, results$test_rmse,
       type = "b",
       xlab = "Alpha (Smoothing Parameter)",
       ylab = "RMSE",
       main = "Holt-Winters Model Validation Curve",
       ylim = range(c(results$train_rmse, results$test_rmse)),
       col = "red",
       pch = 16,
       cex = 1.5,
       lwd = 2)
  
  # Add the training set RMSE to the plot
  lines(results$alpha, results$train_rmse,
        type = "b",
        col = "blue",
        pch = 16,
        cex = 1.5,
        lwd = 2)
  
  # Add a legend
  legend("topright",
         legend = c("Test RMSE", "Training RMSE"),
         col = c("red", "blue"),
         pch = 16,
         lwd = 2,
         cex = 0.8)
}