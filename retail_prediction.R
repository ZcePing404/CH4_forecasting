setwd("C:/Users/enton/Project/R/supplement_sales_forecasting")

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
df <- read_csv("intermittent-renewables-production-france.csv")
df
str(df)

# Aggregate to weekly level
df_daily <- df %>%
  group_by(Date) %>%
  summarise(DailyProduction = mean(Production, na.rm = TRUE))

df_clean <- preprocess_data(df_daily)

# Plot time series
ggplot(df_clean, aes(x = Date, y = DailyProduction)) +
  geom_line(color = "steelblue") +
  labs(title = "Total Production Over Time",
       x = "Date", y = "Production")

ggplot(df_clean, aes(x = DailyProduction)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "black") +
  labs(title = "Distribution of Production",
       x = "Production", y = "Count")


eda_results <- eda_time_series(df_clean)

Data <- df_clean$DailyProduction

train_size <- floor(0.80 * length(Data))
train <- head(Data, train_size)
train
test  <- tail(Data, length(Data) - train_size)
ts_train <- ts(train,
               frequency = 365,
               start = c(year(min(df_clean$Date)), yday(min(df_clean$Date))))
ts_train
ts_test <- ts(test,
              start = time(ts_train)[length(ts_train)] + 1/365,
              frequency = 365)
checkresiduals(ts_train)
Box.test(ts_train, lag = 10, type = "Ljung-Box") # fail to reject, our dataset is white noise

fit_sarima <- auto.arima(ts_train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Summary of the SARIMA model
summary(fit_sarima)

# Forecast for the length of the test set
fcast_sarima <- forecast(fit_sarima, h = length(test))

# Plot forecast vs actual
autoplot(fcast_sarima) +
  labs(title = "SARIMA Forecast vs Actual",
       x = "Time", y = "Daily Production") +
  theme_minimal()

# Accuracy metrics
accuracy(fcast_sarima, ts_test)
