install.packages("KFAS")
library(readr)     
library(dplyr)     
library(lubridate) 
library(forecast)  
library(ggplot2)   
library(tseries)
library(urca)
library(zoo)
library(prophet)
library(Metrics)
library(KFAS)
source("preprocessing.R")
source("stationary_test.R")
source("differencing_method.R")
source("ARIMA.R")
source("HtWinters.R")
source("Prophet.R")
source("kalman_filter.R")

# Read dataset
df <- read_csv("nitrous_oxide_concentration.csv")
print(df)
str(df)

processed_data <- preprocess_data()
df_clean = processed_data$df
ts_data = processed_data$ts_data

# show the resulting monthly data frame to check the output
print(df_clean)

# Plot time series
ggplot(df_clean, aes(x = date, y = average)) +
   geom_line(color = "steelblue") +
   labs(title = "Monthly Avg N2O Concentration from Jan 2010 to Dec 2023",
       x = "date", y = "Avg Concentration")

# ggplot(df_clean, aes(x = average)) +
#   geom_histogram(fill = "skyblue", bins = 30, color = "black") +
#   labs(title = "Distribution of CO2 concentration",
#        x = "Avg Concentration", y = "Count")

stationary_test()
differencing_method()
Data <- df_clean$average

train_size <- 120
train <- head(Data, train_size)

min_date <- min(df_clean$date)
min_year <- as.numeric(format(min_date, "%Y"))
min_month <- as.numeric(format(min_date, "%m"))

test  <- tail(Data, length(Data) - train_size)
ts_train <- ts(train,
               frequency = 12,
               start = c(min_year, min_month))


# Get the last date of the training set
last_train_date <- max(df_clean$date[1:length(train)])
first_test_date <- last_train_date %m+% months(1) # Find the date of the first observation in the test set
# Use the year and month of the first test date to set the start of the ts object
ts_test <- ts(test,
              start = c(year(first_test_date), month(first_test_date)),
              frequency = 12)

checkresiduals(ts_train)

ARIMA_method()

HoltWinters_method(ts_train, ts_test)

Prophet_method()

kalman_filter_method(ts_train, ts_test)
