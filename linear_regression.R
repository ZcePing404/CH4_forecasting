linear_regression_method <- function(df_clean, ts_train, ts_test) {
  
  min_date <- min(df_clean$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  
  ts_df <- ts(data=df_clean, start=c(min_year, min_month), frequency=12); ts_df
  df_clean$trend <- seq_along(df_clean$date)
  df_clean$month <- factor(cycle(ts_df))
  df_clean <- df_clean %>% mutate(lag_average = lag(average,1))
  df_clean <- na.omit(df_clean)

  train_df <- head(df_clean, length(ts_train)-1); 
  test_df <- tail(df_clean, nrow(df_clean)-nrow(train_df)); 
  test_df$average <- NULL; 
  
  linear_reg_model <- lm(average ~ trend + month + lag_average, data=train_df)
  min_date <- min(train_df$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  fitted_values <- ts(data=fitted(linear_reg_model), start=c(min_year, min_month), frequency=12); fitted_values
  
  
  summary(linear_reg_model)
  checkresiduals(linear_reg_model)
  
  
  forecast_result <- forecast(linear_reg_model, newdata = test_df)
  
  fc_mean <- test_df
  fc_mean$average <- forecast_result$mean
  min_date <- min(fc_mean$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  fc_mean <- ts(data=fc_mean$average, start=c(min_year, min_month), frequency=12)
  
  fc_lwr <- forecast_result$lower[, 2]
  fc_upr <- forecast_result$upper[, 2]
  
  
  plot(ts_data, 
       main = "Linear regression Forecast vs Actual Data",
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
  
  train_accuracy <- forecast::accuracy(linear_reg_model)
  test_accuracy <- forecast::accuracy(fc_mean, ts_test)
  
  cat(train_accuracy)
  cat(test_accuracy)
}