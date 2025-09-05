preprocess_data <- function(df) {
  # Print the first few rows of the input
  cat("\nBefore preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  # 1. Handle missing values
  missing_count <- sum(is.na(df$TotalRevenue))
  if (missing_count > 0) {
    cat("\nMissing values detected:", missing_count, "→ applying linear interpolation.\n")
    df$TotalRevenue <- na.approx(df$TotalRevenue, na.rm = FALSE)
  } else {
    cat("\nNo missing values detected.\n")
  }
  
  # 2. Remove duplicates
  dup_count <- sum(duplicated(df))
  if (dup_count > 0) {
    cat("\nDuplicate rows detected:", dup_count, "→ removing duplicates.\n")
    df <- df[!duplicated(df), ]
  } else {
    cat("\nNo duplicate rows detected.\n")
  }
  
  # 3. Detect and remove outliers (IQR method)
  Q1 <- quantile(df$TotalRevenue, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$TotalRevenue, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  outlier_idx <- which(df$TotalRevenue < (Q1 - 1.5*IQR) | 
                         df$TotalRevenue > (Q3 + 1.5*IQR))
  if (length(outlier_idx) > 0) {
    cat("\nOutliers detected:", length(outlier_idx), "→ removing outliers.\n")
    df <- df[-outlier_idx, ]
  } else {
    cat("\nNo outliers detected.\n")
  }
  
  cat("\nAfter preprocessing:", nrow(df), "rows ×", ncol(df), "columns\n")
  
  return(df)
}
