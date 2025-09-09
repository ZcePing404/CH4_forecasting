eda_time_series <- function(df) {
  
  # 1. Convert to ts object (daily frequency)
  ts_data <- ts(df$DailyProduction, frequency = 365,
                start = c(year(min(df$Date)), yday(min(df$Date))))
  
  # -------------------------------
  # Decomposition (trend + seasonality + remainder)
  # -------------------------------
  cat("\n--- STL Decomposition ---\n")
  ts_decomp <- stl(ts_data, s.window = "periodic")
  plot(ts_decomp, main = "Decomposition of Daily Production")
  
  # -------------------------------
  # Trend Analysis
  # -------------------------------
  cat("\n--- Trend Analysis ---\n")
  p1 <- ggplot(df, aes(x = Date, y = DailyProduction)) +
    geom_line(color = "darkgreen") +
    labs(title = "Trend in Daily Production", x = "Date", y = "Production (mean)") +
    theme_minimal()
  print(p1)
  
  # -------------------------------
  # Seasonality
  # -------------------------------
  cat("\n--- Seasonality ---\n")
  print(ggseasonplot(ts_data, year.labels = TRUE, col = rainbow(10), 
                     main = "Seasonal Plot of Daily Production"))
  
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
  
  acf(ts_data, main = "ACF of Daily Production")
  pacf(ts_data, main = "PACF of Daily Production")
  
  # Return decomposition + tests
  return(list(decomposition = ts_decomp, adf = adf, kpss = kpss))
}
