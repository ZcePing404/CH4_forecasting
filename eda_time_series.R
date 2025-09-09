eda_time_series <- function(df) {
  
  # 1. Convert to ts object
  ts_data <- ts(df$average, start = c(2002, 5), frequency = 12)
  
  # -------------------------------
  # Decomposition (trend + seasonality + remainder)
  # -------------------------------
  cat("\n--- STL Decomposition ---\n")
  ts_decomp <- stl(ts_data, s.window = "periodic")
  plot(ts_decomp, main = "Decomposition of Monthly Concentration")
  
  
  # -------------------------------
  # Seasonality
  # -------------------------------
  cat("\n--- Seasonality ---\n")
  print(ggseasonplot(ts_data, year.labels = TRUE, col = rainbow(10),
                     main = "Seasonal Plot of Monthly Concentration"))
  
  
  # -------------------------------
  # Stationarity Tests
  # -------------------------------
  cat("\n--- Original Data Analysis ---\n")
  p1 <- ggplot(df, aes(x = date, y = average)) +
    geom_line(color = "darkgreen") +
    labs(title = "Trend in Monthly Concentration", x = "date", y = "Avg Concentration") +
    theme_minimal()
  
  # Autocorrelation Analysis
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  plot(ts_data, ylab="Concentration", main="Monthly nitrous oxide concentration")
  acf(ts_data, main="ACF of original data")
  pacf(ts_data, main="PACF of original data")
  
  cat("\n--- Stationarity Tests ---\n")
  
  # ADF Test
  adf <- adf.test(ts_data)
  print(adf)
  
  # KPSS Test
  kpss <- kpss.test(ts_data)
  print(kpss)
  

  
  # --------------------------------------------------------------
  # First Differencing
  # --------------------------------------------------------------
  cat("\n--- First Differencing ---\n")
  diff_ts_data <- ts(diff(ts_data, lag=12))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  ts.plot(diff_ts_data,gpars=list(main= "First Differences", xlab="Month",
                                  ylab="Concentration", lty=1))
  
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
  # Second Differencing
  # --------------------------------------------------------------
  cat("\n--- Second Differencing ---\n")
  diff_ts_data2 <- ts(diff(diff_ts_data, lag = 1))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  ts.plot(diff_ts_data2,gpars=list(main= "Second Differences", xlab="Month",
                                  ylab="Concentration", lty=1))
  
  # Autocorrelation Analysis
  acf(diff_ts_data2, main="ACF of Second differencing", lag.max=40)
  pacf(diff_ts_data2, main="PACF of Second differencing", lag.max=40)
  
  # ADF Test
  adf <- adf.test(diff_ts_data2)
  print(adf)
  
  # KPSS Test
  kpss <- kpss.test(diff_ts_data2)
  print(kpss)
  
  
  # Return decomposition + tests
  return(list(df = diff_ts_data, decomposition = ts_decomp, adf = adf, kpss = kpss))
}