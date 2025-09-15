tbats_method <- function(ts_train, ts_test) {
  TBATSfit <- tbats(ts_train)
  forecast_results <- forecast(TBATSfit, h = length(ts_test), biasadj = TRUE)
  fitted_values <- fitted(TBATSfit)
  print(summary(TBATSfit))
  forecast::accuracy(forecast_results, ts_test)
  checkresiduals(TBATSfit)
  
  fc_mean <- forecast_results$mean
  fc_lwr <- forecast_results$lower[,2]
  fc_upr <- forecast_results$upper[,2]
  
  plot(ts_data, 
       main = "TBATS Forecast vs Actual Data",
       ylab = "Concentration (ppm)",
       xlab = "Year",
       col = "black")
  lines(fitted_values, col = "red")
  lines(fc_mean, col = "blue")
  polygon(c(time(fc_mean), rev(time(fc_mean))),
          c(fc_lwr, rev(fc_upr)),   # 95% CI (2nd column)
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