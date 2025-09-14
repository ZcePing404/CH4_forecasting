Prophet_method <- function(){
  train_df <- data.frame(
    ds = df_clean$date[1:length(train)],  # training dates
    y  = train                            # training values
  )
  test_df <- data.frame(
    ds = df_clean$date[(length(train) + 1):nrow(df_clean)],  # test dates
    y  = test                                                # test values
  )
  
  m <- prophet(train_df,
               yearly.seasonality = TRUE,   # monthly data usually has yearly cycle
               weekly.seasonality = FALSE,  # no need for weekly seasonality in monthly data
               daily.seasonality = FALSE,
               changepoint.range = 0.85,
               interval.width = 0.95
  )
  
  
  future <- make_future_dataframe(m,
                                  periods = length(test),
                                  freq = "month")
  
  forecast <- predict(m, future)
  forecast_test <- tail(forecast, nrow(test_df))
  
  residuals_test <- test_df$y - forecast_test$yhat
  # Ljung–Box test on prediction residuals
  Box.test(residuals_test, lag = 20, type = "Ljung-Box")
  
  p <- plot(m, forecast) +
    labs(
      title = "Prophet Forecast",
      x = "Date",
      y = "Average"
    )
  print(p)
  
  prophet_plot_components(m, forecast)
  
  # Make sure all date columns are Date (not POSIXct)
  train_dates    <- as.Date(train_df$ds)
  test_dates     <- as.Date(df_clean$date[(length(train) + 1):length(Data)])
  forecast_dates <- as.Date(forecast$ds)
  
  # Plot training data
  plot(train_dates, train_df$y, type = "l", col = "blue", lwd = 2,
       xlab = "Date", ylab = "Value",
       main = "Prophet Forecast vs Train & Test",
       xlim = range(c(train_dates, test_dates, forecast_dates)),
       ylim = range(c(train_df$y, test, forecast$yhat)))
  
  # Add test (actual values)
  lines(test_dates, test, col = "black", lwd = 2)
  
  # Add forecast (Prophet predictions)
  lines(forecast_dates, forecast$yhat, col = "red", lwd = 2)
  
  # Legend
  legend("topleft",
         legend = c("Train", "Test (Actual)", "Forecast"),
         col = c("blue", "black", "red"),
         lty = 1, lwd = 2)
  
  forecast_test <- forecast %>%
    filter(as.Date(ds) >= as.Date(first_test_date)) %>%
    slice(1:length(test))
  
  comparison <- data.frame(
    ds = df_clean$date[(length(train)+1):length(Data)],
    actual = as.numeric(ts_test),
    predicted = forecast_test$yhat
  )
  
  # ----------------------------
  # 5. Accuracy metrics
  # ----------------------------
  rmse_val <- rmse(comparison$actual, comparison$predicted)
  mae_val  <- mae(comparison$actual, comparison$predicted)
  mape_val <- mape(comparison$actual, comparison$predicted)
  r2_val   <- 1 - sum((comparison$actual - comparison$predicted)^2) /
    sum((comparison$actual - mean(comparison$actual))^2)
  
  cat("RMSE:", rmse_val, "\n")
  cat("MAE :", mae_val, "\n")
  cat("MAPE:", mape_val, "\n")
  cat("R²  :", r2_val, "\n")

  # 6. Plot Actual vs Forecast
  plot(comparison$ds, comparison$actual, type = "l", col = "black", lwd = 2,
       ylab = "Value", xlab = "Date", main = "Prophet Forecast vs Actual")
  lines(comparison$ds, comparison$predicted, col = "red", lwd = 2)
  legend("topleft", legend = c("Actual", "Forecast"),
         col = c("black", "red"), lty = 1, lwd = 2)
  
  # 7. Residual analysis
  comparison$residuals <- comparison$actual - comparison$predicted
  
  # Residual ACF
  acf(comparison$residuals, main = "ACF of Prophet Residuals")
}