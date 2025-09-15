Prophet_method <- function(){
  prophet_df <- data.frame(ds = df_clean$date, y = df_clean$average)
  train_df <- data.frame(
    ds = df_clean$date[1:length(train)],  # training dates
    y  = train                            # training values
  )
  test_df <- data.frame(
    ds = df_clean$date[(length(train) + 1):nrow(df_clean)],  # test dates
    y  = test                                                # test values
  )
  
  df_train <- train_df %>%
    arrange(ds) %>%
    mutate(lag1 = lag(y, 1)) %>%
    na.omit()
  
  df_test <- test_df %>%
    arrange(ds) %>%
    mutate(lag1 = lag(y, 1)) %>%
    na.omit()
  
  m <- prophet(
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    changepoint.range = 0.7,
    interval.width = 0.95
  )
  
  # Add seasonalities
  m <- add_seasonality(m, name = 'yearly', period = 365.25, fourier.order = 15)
  m <- add_seasonality(m, name = 'quarterly', period = 91.25, fourier.order = 12)
  
  # Add regressor
  m <- add_regressor(m, 'lag1')
  
  # Fit model on training data
  m <- fit.prophet(m, df_train)

  future <- make_future_dataframe(m, periods = 24, freq = "month")
  
  future <- future %>%
    left_join(df_clean %>% mutate(lag1 = dplyr::lag(average, 1)) %>%
                select(date, lag1),
              by = c("ds" = "date"))

  forecast <- predict(m, future)

  df_pred <- forecast[, c("ds", "yhat", "yhat_lower", "yhat_upper")]
  df_pred$ds <- as.Date(df_pred$ds)   # ensure Date format
  
  train_df <- merge(df_train, df_pred, by = "ds", all.x = TRUE)
  test_df  <- merge(df_test,  df_pred, by = "ds", all.x = TRUE)
  
  # --- Build plot ---
  plot(prophet_df$ds, prophet_df$y, type = "n", 
       xaxt = "n",  # suppress default x-axis
       xlab = "Year", ylab = "Concentration (ppm)", 
       main = "Prophet Forecast with Train/Test Split")
  
  # 95% CI (blue shaded ribbon for forecast)
  polygon(c(test_df$ds, rev(test_df$ds)),
          c(test_df$yhat_upper, rev(test_df$yhat_lower)),
          col = rgb(0,0,1,0.2), border = NA)
  
  lines(prophet_df$ds, prophet_df$y, col = "black", lwd = 1.5)
  lines(train_df$ds, train_df$yhat, col = "red", lwd = 1.5)
  lines(test_df$ds, test_df$yhat, col = "blue", lwd = 1.5)
  abline(v = max(train_df$ds), col = "gray50", lty = 2)

  years <- seq(from = as.numeric(format(min(prophet_df$ds), "%Y")),
               to   = 2024,
               by   = 2)
  axis(1, 
       at = as.Date(paste0(years, "-01-01")), 
       labels = years)
  
  legend("topleft", 
         legend = c("Actual Data", "Train", "Forecast", "95% CI"), 
         col    = c("black", "red", "blue", rgb(0,0,1,0.2)), 
         lty    = c(1, 1, 1, NA), 
         lwd    = c(1.5, 1.5, 1.5, NA),
         pch    = c(NA, NA, NA, 15),
         pt.cex = 2,
         bty    = "o")

  # ----------------
  # Training Forecast
  # ----------------
  future_train <- df_train %>% select(ds, lag1)
  forecast_train <- predict(m, future_train)
  
  train_pred <- forecast_train$yhat
  train_actual <- df_train$y
  
  # ---------------
  # Test Forecast
  # ---------------
  future_test <- df_test %>% select(ds, lag1)
  forecast_test <- predict(m, future_test)
  
  test_pred <- forecast_test$yhat
  test_actual <- df_test$y
  
  # --------------------
  # Train Residual Check
  # --------------------
  train_resid <- train_actual - train_pred  
  # ACF + Ljung-Box test
  checkresiduals(train_resid) 

  # Show results
  cat("\nTrain Accuracy:\n")
  forecast::accuracy(train_pred, train_actual)
  
  cat("\nTest Accuracy:\n")
  forecast::accuracy(test_pred, test_actual)
}

