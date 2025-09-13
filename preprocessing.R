preprocess_data <- function() {
  cat("\nBefore preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  # Remove missing values
  missing_count <- sum(is.na(df$average))
  if (missing_count > 0) {
    cat("Missing values detected in dataset:", missing_count, "→ removing rows.\n")
    df <- df %>% filter(!is.na(average))
  } else {
    cat("No missing values detected in dataset\n")
  }
  
  df <- df %>%
    mutate(
      date_char = as.character(date),
      previous_date = lag(date_char),
      month_num = ifelse(
        grepl("\\.1$", date_char) & grepl("\\.9$", previous_date),
        10,
        as.numeric(sub(".*\\.", "", date_char))
      ),
      date = ymd(paste(floor(as.numeric(date)), month_num, "01", sep = "-"))
    ) %>%
    select(date,average)
  
  # Remove duplicate rows
  dup_count <- sum(duplicated(df))
  if (dup_count > 0) {
    cat("Duplicate rows detected:", dup_count, "→ removing duplicates.\n")
    df <- df[!duplicated(df), ]
  } else {
    cat("No duplicate rows detected.\n")
  }
  
  # Select the recent 14 years
  df <- df %>%
    filter(date >= as.Date("2010-01-01") & date <= as.Date("2023-12-01"))
  
  min_date <- min(df$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  ts_data <- ts(df$average, start=c(min_year, min_month), frequency=12)
  
  
  # -------------------------------
  # Decomposition
  # -------------------------------
  cat("\n--- STL Decomposition ---\n")
  ts_decomp <- stl(ts_data, s.window = "periodic", robust = T)
  plot(ts_decomp, main = "Decomposition of Monthly N2O Concentration  from Jan 2010 to Dec 2023")
  
  remainder_data <- ts_decomp$time.series[, "remainder"]
  plot(remainder_data,
       xlab = "Month",
       ylab = "remainder",
       main = "Remainder of the Monthly N2O Concentration  from Jan 2010 to Dec 2023")
  
  # Box plot
  boxplot(remainder_data,
          ylab = "Remainder",
          main = "Boxplot of Remainder of Monthly N2O Concentration from Jan 2010 to Dec 2023")
  
  
  # Calculate the IQR, Q1, and Q3
  Q1 <- quantile(remainder_data, 0.25, na.rm = TRUE)
  Q3 <- quantile(remainder_data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Calculate the outlier thresholds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outlier_idx <- which(remainder_data < (lower_bound) | 
                         remainder_data > (upper_bound))
  
  ts_data_corrected <- ts_data
  
  if (length(outlier_idx) > 0) {
    cat("Outliers detected in dataset:", length(outlier_idx), "→ handling outliers.\n")
    ts_ideal_data <- ts_decomp$time.series[, "trend"] + ts_decomp$time.series[, "seasonal"]
    # Use a loop to replace each outlier with the corrected value from the clean series
    for (i in outlier_idx) {
      ts_data_corrected[i] <- ts_ideal_data[i]
    }
  } else {
    cat("No outliers detected in dataset\n")
  }
  
  cat("\nAfter preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  ts_decomp <- stl(ts_data_corrected, s.window = "periodic")
  plot(ts_decomp, main = "Decomposition of Monthly CO2 Concentration from Jan 2010 to Dec 2023")
  
  # --- Convert the corrected ts object back to a data frame ---
  df_corrected <- as.data.frame(ts_data_corrected)
  df_corrected$average <- df_corrected$x
  df_corrected$date <- as.Date(as.yearmon(time(ts_data_corrected)))
  df_corrected <- df_corrected %>%
    select(date, average)
  
  return(list(df=df_corrected, ts_data=ts_data_corrected))
}