prophet_df <- data.frame(ds = df_clean$date, y = df_clean$average)

train_size <- 120
df_train <- prophet_df[1:train_size, ]
df_test <- prophet_df[(train_size + 1):nrow(prophet_df), ]

df_train <- df_train %>%
  arrange(ds) %>%
  mutate(lag1 = lag(y, 1)) %>%
  na.omit()

m <- prophet(
          yearly.seasonality = FALSE,
          weekly.seasonality = FALSE,
          daily.seasonality = FALSE,
          changepoint.range = 0.7,
          # interval.width = 0.95,
          # changepoint.prior.scale = 15,
          # seasonality.prior.scale = 15
        )

m <- add_seasonality(m, name = 'yearly', period = 365.25, fourier.order = 15)
m <- add_seasonality(m, name = 'quarterly', period = 91.25, fourier.order = 12)
m <- add_regressor(m, 'lag1')

fit <- fit.prophet(m, df_train)

forecasted_train <- predict(fit, df_train)
residuals <- df_train$y - forecasted_train$yhat
  
ts_residuals <- ts(residuals)
checkresiduals(ts_residuals)


future <- df_test[, "ds", drop = FALSE]


fr <- predict(fit, future)

prophet_plot_components(fit, fr)

predicted_values <- fr$yhat


print("--- Training Set Accuracy ---")
forecast::accuracy(as.numeric(forecasted_train$yhat), df_train$y)
print("--- Test Set Accuracy ---")
forecast::accuracy(as.numeric(predicted_values), df_test$y)



layout(matrix(c(1,1)))
plot(prophet_df$ds, prophet_df$y,
     main = "Prophet Model Fit and Forecast vs Actual Data",
     ylab = "Concentration (ppm)",
     xlab = "Date",
     type = "l",
     col = "black")

# Add the fitted values from the training set
lines(df_train$ds, forecasted_train$yhat, col = "red")

# Add the forecasted mean values for the test set
lines(df_test$ds, fr$yhat, col = "blue")

# Add the 95% confidence interval for the forecast
polygon(c(df_test$ds, rev(df_test$ds)),
        c(fr$yhat_lower, rev(fr$yhat_upper)),
        col = rgb(0, 0, 1, 0.2), border = NA)

# Add a vertical line to separate the training and test sets
abline(v = min(df_test$ds), lty = 2, col = "grey")

# Add a legend
legend("topleft",
       legend = c("Actual Data", "Fitted", "Forecast", "95% CI"),
       col = c("black", "red", "blue", rgb(0,0,1,0.2)),
       lty = c(1, 1, 1, NA),
       pch = c(NA, NA, NA, 15),
       pt.cex = 2,
       cex = 0.8)





























prophet_df <- data.frame(ds = df_clean$date, y = df_clean$average)

train_size <- 120
prophet_df_train <- prophet_df[1:train_size, ]
prophet_df_test <- prophet_df[(train_size + 1):nrow(prophet_df), ]

# scales_to_test <- c(0.5, 1, 1.5, 5, 10, 50, 100)
scales_to_test <- c(0.5, 0.6, 0.7, 0.8, 0.9)

results <- data.frame(
  scale = numeric(),
  train_rmse = numeric(),
  test_rmse = numeric()
)

for (s in scales_to_test) {
  # Fit the Prophet model with the current scale value
  m <- prophet(df_train, yearly.seasonality = TRUE,
                 weekly.seasonality = FALSE,
                 daily.seasonality = FALSE,
                 changepoint.range = s,
                 interval.width = 0.85,
                 changepoint.prior.scale = 1,
                 seasonality.prior.scale = 15
  )
  
  forecasted_train <- predict(m, prophet_df_train)
  train_rmse <- sqrt(mean((prophet_df_train$y - forecasted_train$yhat)^2))
  
  future <- prophet_df_test[, "ds", drop = FALSE]
  fr <- predict(m, future)
  test_rmse <- sqrt(mean((prophet_df_test$y - fr$yhat)^2))
  
  results <- rbind(results, data.frame(
    scale = s,
    train_rmse = train_rmse,
    test_rmse = test_rmse
  ))
}

# Create an empty plot with appropriate axes
plot(results$scale, results$test_rmse,
     type = "b",
     log = "x",
     xlab = "Changepoint Range",
     ylab = "RMSE",
     main = "Prophet Model Validation Curve",
     ylim = range(c(results$train_rmse, results$test_rmse)),
     col = "red",
     pch = 16,
     cex = 1.5,
     lwd = 2)

# Add the training set RMSE to the plot
lines(results$scale, results$train_rmse,
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
