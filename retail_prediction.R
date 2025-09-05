setwd("C:/Users/enton/Project/R/Retail_Prediction")

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

# Read dataset
df <- read_csv("Supplement_Sales_Weekly_Expanded.csv")
df_weekly <- df %>%
  mutate(Week = floor_date(Date, unit = "week")) %>%
  group_by(Week) %>%
  summarise(TotalRevenue = sum(Revenue), .groups = "drop")
# Plot time series
#ggplot(df_weekly, aes(x = Week, y = TotalRevenue)) +
#  geom_line(color = "steelblue", size = 1) +
#  geom_point(color = "red", size = 2) +
#  labs(title = "Weekly Total Revenue Over Time",
#       x = "Week",
#       y = "Revenue") +
#  theme_minimal()

Data <- df_weekly$TotalRevenue
Data
adf.test(Data)
train_size <- floor(0.80 * length(Data))
train <- head(Data, train_size)
kpss.test(train)
test  <- tail(Data, length(Data) - train_size)

ts_train <- ts(train, frequency = 52, 
               start = c(year(min(df_weekly$Week)), week(min(df_weekly$Week))))
ts_train


ts_test <- test
ts_test


# Fit ARIMA automatically
fit <- auto.arima(ts_train)

# Model summary
summary(fit)
checkresiduals(fit)

# Forecast next 30 days
forecast_data <- forecast(fit, h = 30)

# Plot forecast
autoplot(forecast_data)
