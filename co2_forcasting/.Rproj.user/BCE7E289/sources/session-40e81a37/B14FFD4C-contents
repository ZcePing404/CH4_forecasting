stationary_test <- function(df) {
  
  # -------------------------------
  # -------------------------------
  # Decomposition (trend + seasonality + remainder)
  # -------------------------------
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

}
