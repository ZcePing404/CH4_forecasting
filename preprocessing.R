preprocess_data <- function(df) {
  cat("\nBefore preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  # Remove missing values (for Production)
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
  
  # Formating the date  
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
    select(date, average)
  
  df <- df %>%
    filter(year(date) >= 2010)

  min_date <- min(df$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  ts_data <- ts(df$average, start=c(min_year, min_month), frequency=12)
  
  # Now you can use cycle()
  boxplot(ts_data ~ cycle(ts_data),
          xlab = "Month",
          ylab = "Avg Concentration",
          main = "Boxplot of Monthly Nitrous Oxide Concentration")
  
  
  # Detect and remove outliers (IQR method for Production)
  Q1 <- quantile(df$average, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$average, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outlier_idx <- which(df$average < (Q1 - 1.5 * IQR) | 
                         df$average > (Q3 + 1.5 * IQR))
  
  if (length(outlier_idx) > 0) {
    cat("Outliers detected in dataset:", length(outlier_idx), "→ removing outliers.\n")
    df <- df[-outlier_idx, ]
  } else {
    cat("No outliers detected in dataset\n")
  }
  
  cat("\nAfter preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  return(list(df=df, ts_data=ts_data))
}