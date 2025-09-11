preprocess_data <- function(df) {
  cat("\nBefore preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  # Remove missing values
  missing_count <- sum(is.na(df$average))
  if (missing_count > 0) {
    cat("Missing values detected in dataset:", missing_count, "→ removing rows.\n")
    df <- df %>% filter(!is.na(average))
  } else {
    cat("No missing values detected in dataset\n")
  }
  
  # Remove duplicate rows
  dup_count <- sum(duplicated(df))
  if (dup_count > 0) {
    cat("Duplicate rows detected:", dup_count, "→ removing duplicates.\n")
    df <- df[!duplicated(df), ]
  } else {
    cat("No duplicate rows detected.\n")
  }
  
  
  # Create a date column for the monthly data
  df$date <- as.Date(paste(df$year, df$month, "01", sep = "-"))
  
  df <- df %>%
    select(date, average)
  
  # Select the recent 14 years
  df <- df %>%
    filter(date >= as.Date("2010-01-01"))
  
  min_date <- min(df$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  ts_data <- ts(df$average, start=c(min_year, min_month), frequency=12)
  
  # Box plot
  boxplot(ts_data ~ cycle(ts_data),
          xlab = "Month",
          ylab = "Avg Concentration",
          main = "Boxplot of Monthly CO2 Concentration from Jan 2010 to Dec 2023")

  
  # Calculate the IQR, Q1, and Q3
  Q1 <- quantile(df$average, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$average, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Calculate the outlier thresholds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outlier_idx <- which(df$average < (lower_bound) | 
                         df$average > (upper_bound))
  
  if (length(outlier_idx) > 0) {
    cat("Outliers detected in dataset:", length(outlier_idx), "→ removing outliers.\n")
    df <- df[-outlier_idx, ]
  } else {
    cat("No outliers detected in dataset\n")
  }

  
  cat("\nAfter preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  return(list(df=df, ts_data=ts_data))
}