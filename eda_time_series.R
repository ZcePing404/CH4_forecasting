eda_time_series <- function(df) {
  
  # 1. Convert to ts object (weekly frequency)
  ts_data <- ts(df$TotalRevenue, frequency = 52,
                start = c(year(min(df$Date)), as.numeric(format(min(df$Date), "%U"))))
  
  # Decomposition (trend + seasonality + remainder)
  cat("\n--- STL Decomposition ---\n")
  ts_decomp <- stl(ts_data, s.window = "periodic")
  plot(ts_decomp, main = "Decomposition of Revenue")
  
  # -------------------------------
  # Trend Analysis
  # -------------------------------
  cat("\n--- Trend Analysis ---\n")
  p1 <- ggplot(df, aes(x = Date, y = TotalRevenue)) +
    geom_line(color = "steelblue") +
    labs(title = "Trend in Total Revenue", x = "Date", y = "Revenue") +
    theme_minimal()
  print(p1)
  
  # -------------------------------
  # Seasonality
  # -------------------------------
  cat("\n--- Seasonality ---\n")
  print(ggseasonplot(ts_data, year.labels = TRUE, col = rainbow(10), main = "Seasonal Plot"))
  
  # -------------------------------
  # Stationarity Tests
  # -------------------------------
  cat("\n--- Stationarity Tests ---\n")
  
  # ADF Test
  adf <- adf.test(ts_data)
  print(adf)
  
  # KPSS Test
  kpss <- kpss.test(ts_data)
  print(kpss)
  
  # -------------------------------
  # Autocorrelation
  # -------------------------------
  cat("\n--- Autocorrelation Analysis ---\n")
  
  Acf(ts_data, main = "ACF of Revenue")
  Pacf(ts_data, main = "PACF of Revenue")
  
  # Return decomposition object in case you want to use it later
  return(list(decomposition = ts_decomp, adf = adf, kpss = kpss))
}