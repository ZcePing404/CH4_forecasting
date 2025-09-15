preprocess_data <- function() {
  
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
  
  # Select the recent 14 years
  df <- df %>%
    filter(date >= as.Date("2012-01-01") & date <= as.Date("2023-12-01"))
  
  min_date <- min(df$date)  
  min_year <- as.numeric(format(min_date, "%Y"))
  min_month <- as.numeric(format(min_date, "%m"))
  ts_data <- ts(df$average, start=c(min_year, min_month), frequency=12)
  
  
  # -------------------------------
  # Decomposition
  # -------------------------------
  cat("\n--- STL Decomposition ---\n")
  ts_decomp <- stl(ts_data, s.window = "periodic", robust = T)
  plot(ts_decomp, main = "Decomposition of Monthly CH4 Concentration  from Jan 2010 to Dec 2023")
  
  remainder_data <- ts_decomp$time.series[, "remainder"]
  plot(remainder_data,
       xlab = "Year",
       ylab = "remainder",
       main = "Remainder of the Monthly CH4 Concentration  from Jan 2010 to Dec 2023")
  
  # Box plot
  bp <- boxplot(remainder_data,
          ylab = "Remainder",
          main = "Boxplot of Remainder of Monthly CH4 Concentration from Jan 2010 to Dec 2023")
  outlier_idx <- which(remainder_data %in% bp$out)
  
  df_corrected <- df
  ts_data_corrected <- ts_data
  
  if (length(outlier_idx) > 0) {
    cat("Outliers detected in dataset:", length(outlier_idx), "→ handling outliers.\n")
    ts_ideal_data <- ts_decomp$time.series[, "trend"] + ts_decomp$time.series[, "seasonal"]
    # Use a loop to replace each outlier with the corrected value from the clean series
    for (i in outlier_idx) {
      ts_data_corrected[i] <- ts_ideal_data[i]
    }
    # --- Convert the corrected ts object back to a data frame ---
    average <- as.numeric(ts_data_corrected)
    df_corrected <- as.data.frame(average)
    df_corrected$date <- as.Date(as.yearmon(time(ts_data_corrected)))
  } else {
    cat("No outliers detected in dataset\n")
  }
  
  plot(ts_data, 
       xlab = "Year",
       ylab = "Avg Concentration",
       main = "Original Data with Corrected Outliers")
  lines(ts_data_corrected, col = "red")
  # Add a legend to the plot
  legend("topleft", 
         legend = c("Original Data", "Corrected Data"), 
         col = c("black", "red"), 
         lty = 1,
         bty = "n")
  
  return(list(df=df_corrected, ts_data=ts_data_corrected))
}
