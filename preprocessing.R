preprocess_data <- function(df) {
  cat("\nBefore preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  # 1. Remove missing values (for Production)
  missing_count <- sum(is.na(df$DailyProduction))
  if (missing_count > 0) {
    cat("Missing values detected in Production:", missing_count, "→ removing rows.\n")
    df <- df %>% filter(!is.na(DailyProduction))
  } else {
    cat("No missing values detected in Production.\n")
  }
  
  # 2. Remove duplicate rows
  dup_count <- sum(duplicated(df))
  if (dup_count > 0) {
    cat("Duplicate rows detected:", dup_count, "→ removing duplicates.\n")
    df <- df[!duplicated(df), ]
  } else {
    cat("No duplicate rows detected.\n")
  }
  
  # 3. Detect and remove outliers (IQR method for Production)
  Q1 <- quantile(df$DailyProduction, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$DailyProduction, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outlier_idx <- which(df$DailyProduction < (Q1 - 1.5 * IQR) | 
                         df$DailyProduction > (Q3 + 1.5 * IQR))
  
  if (length(outlier_idx) > 0) {
    cat("Outliers detected in Production:", length(outlier_idx), "→ removing outliers.\n")
    df <- df[-outlier_idx, ]
  } else {
    cat("No outliers detected in Production.\n")
  }
  
  cat("\nAfter preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  return(df)
}
