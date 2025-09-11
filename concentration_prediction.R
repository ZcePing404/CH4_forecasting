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
source("stationary_test.R")
source("differencing_method.R")
source("ARIMA.R")

# Read dataset
df <- read_csv("nitrous_oxide_concentration.csv")
print(df)
str(df)

processed_data <- preprocess_data(df)
df_clean = processed_data$df
ts_data = processed_data$ts_data

# show the resulting monthly data frame to check the output
print(df_clean)

# Plot time series
ggplot(df_clean, aes(x = date, y = average)) +
   geom_line(color = "steelblue") +
   labs(title = "Monthly Nitrous Oxide Concentration",
       x = "date", y = "Avg Concentration")

# ggplot(df_clean, aes(x = average)) +
#   geom_histogram(fill = "skyblue", bins = 30, color = "black") +
#   labs(title = "Distribution of nitrous oxide concentration",
#        x = "Avg. Concentration", y = "Count")

stationary_test(df_clean)
differencing_method(df_clean)
Data <- df_clean$average
train_size <- floor(0.80 * length(Data))
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

ARIMA(ts_train, ts_test)