setwd("C:/Users/enton/Project/R/supplement_sales_forecasting")

# Load packages
#install.packages("readr")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("forecast")
#install.packages("ggplot2")
library(readr)     # to read csv
library(dplyr)     # for data manipulation
library(lubridate) # to handle dates
library(forecast)  # ARIMA forecasting
library(ggplot2)   # plotting
library(tseries)
library(urca)
library(zoo)
source("preprocessing.R")
source("eda_time_series.R")

# Read dataset
df <- read_csv("Supplement_Sales_Weekly_Expanded.csv")
df_weekly <- df %>%
  group_by(Date) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop")
df_clean <- preprocess_data(df_weekly)

# Plot time series
ggplot(df_clean, aes(x = Date, y = TotalRevenue)) +
  geom_line(color = "steelblue") +
  labs(title = "Total Revenue Over Time",
       x = "Date", y = "Revenue")

ggplot(df_clean, aes(x = TotalRevenue)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "black") +
  labs(title = "Distribution of Revenue",
       x = "Revenue", y = "Count")



Data <- df_weekly$TotalRevenue

train_size <- floor(0.80 * length(Data))
train <- head(Data, train_size)
train
test  <- tail(Data, length(Data) - train_size)
ts_train <- ts(train, frequency = 52, 
               start = c(year(min(df_weekly$Date)), week(min(df_weekly$Date))))
ts_train
ts_test <- test
ts_test

eda_results <- eda_time_series(df_clean)


