differencing_method <- function() {
  # Determine number of differences
  num_seasonal_diff <- nsdiffs(ts_data)
  num_nonseasonal_diff <- ndiffs(ts_data)
  cat("Non-seasonal differences needed:", num_nonseasonal_diff, "\n")
  cat("Seasonal differences needed:", num_seasonal_diff, "\n")
  
  # --------------------------------------------------------------
  # First Differencing (seasonal)
  # --------------------------------------------------------------
  cat("\n--- First Differencing (seasonal) ---\n")
  diff_ts_data <- ts(diff(ts_data, lag=12))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  ts.plot(diff_ts_data,gpars=list(main= "First Differences (seasonal)", xlab="Month",
                                  ylab="Avg Concentration", lty=1))
  
  # Autocorrelation Analysis
  acf(diff_ts_data, main="ACF of first differencing", lag.max=40)
  pacf(diff_ts_data, main="PACF of first differencing", lag.max=40)
  
  # ADF Test
  adf <- adf.test(diff_ts_data)
  print(adf)
  
  # KPSS Test
  kpss <- kpss.test(diff_ts_data)
  print(kpss)
  
  # --------------------------------------------------------------
  # Second Differencing (non-seasonal)
  # --------------------------------------------------------------
  cat("\n--- Second Differencing (non-seasonal) ---\n")
  diff_ts_data2 <- ts(diff(diff_ts_data, lag = 1))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  ts.plot(diff_ts_data2,gpars=list(main= "Second Differences (non-seasonal)", xlab="Month",
                                  ylab="Avg Concentration", lty=1))
  
  # Autocorrelation Analysis
  acf(diff_ts_data2, main="ACF of Second differencing", lag.max=40)
  pacf(diff_ts_data2, main="PACF of Second differencing", lag.max=40)

  
  # ADF Test
  adf <- adf.test(diff_ts_data2)
  print(adf)
  
  # KPSS Test
  kpss <- kpss.test(diff_ts_data2)
  print(kpss)

}
