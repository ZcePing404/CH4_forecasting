setwd("E:/Statistic/supplement_sales_forecasting") #Lim Wei He working directory
setwd("C:/Users/isaac/OneDrive/Documents/GitHub/supplement_sales_forecasting") #Isaac working directory
setwd("C:/Users/enton/Project/R/forecasting_nitron_concentration") # Zce Ping working directory

library(readr)     
library(dplyr)     # for data manipulation
library(lubridate) # to handle dates
library(forecast)  # ARIMA forecasting
library(ggplot2)   # plotting
library(tseries)
library(urca)
library(zoo)
source("preprocessing.R")
source("eda_time_series.R")
source("lstm.R")

# Read dataset
df <- read_csv("nitrous_oxide_concentration.csv")
print(df)
str(df)

df_clean <- preprocess_data(df)

# show the resulting monthly data frame to check the output
print(df_clean)

# Plot time series
ggplot(df_clean, aes(x = date, y = average)) +
   geom_line(color = "steelblue") +
   labs(title = "Total Production Over Time",
       x = "date", y = "Monthly Concentration")

# ggplot(df_clean, aes(x = average)) +
#   geom_histogram(fill = "skyblue", bins = 30, color = "black") +
#   labs(title = "Distribution of Production",
#        x = "Avg. Concentration", y = "Count")

eda_results <- eda_time_series(df_clean)
Data <- df_clean$average
train_size <- floor(0.80 * length(Data))
train <- head(Data, train_size)
train

test  <- tail(Data, length(Data) - train_size)
ts_train <- ts(train,
               frequency = 12,
               start = c(2002, 5))
ts_train
# Get the last date of the training set
last_train_date <- max(df_clean$date[1:length(train)])
first_test_date <- last_train_date %m+% months(1) # Find the date of the first observation in the test set
# Use the year and month of the first test date to set the start of the ts object
ts_test <- ts(test,
              start = c(year(first_test_date), month(first_test_date)),
              frequency = 12)

checkresiduals(ts_train)
Box.test(ts_train, lag = 12, type = "Ljung-Box") # fail to reject, our dataset is white noise

# -------------------------------
# Manual ARIMA
# -------------------------------
fit <- arima(ts_train, order=c(0,1,3), seasonal=list(order =c(2,1,0), period=12))
checkresiduals(fit)
summary(fit)

# Forecast for the manual fit arima
fr <- forecast(fit, h = length(test))
layout(matrix(c(1,1)))
plot(fr)
lines(ts_test, col="turquoise2")
accuracy(fr, ts_test)

# -------------------------------
# Auto ARIMA
# -------------------------------
fit2 <- auto.arima(ts_train, ic="aic", trace=TRUE)
checkresiduals(fit2)
summary(fit2)

# Forecast for the auto fit arima
fr <- forecast(fit2, h = length(test))
layout(matrix(c(1,1)))
plot(fr)
lines(ts_test, col="turquoise2")
accuracy(fr, ts_test)

