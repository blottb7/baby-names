library(tidyverse)
library(gganimate)
library(scales)
library(forecast)
library(xts)

# Data Comes from https://www.ssa.gov/oact/babynames/names.zip

##Read In Data Files
all_data <- map_dfr(dir('./data'), function(x){
    read_csv(paste0('./data/', x), 
             col_names = c('name', 'gender', 'count'),
             col_types = list('c', 'c', 'i')) %>%
        mutate(year = x %>% str_extract('\\d+') %>% as.integer)
})

##### Forecast one name

# get one name
df_D <- all_data %>%
    filter(name == "Delilah")

# get summary statistics
summary(df_D)
summary(df_D$count)

##### convert to time_series
df_D_ts <- xts(df_D, order.by = as.Date(as.yearmon(df_D$year)))

# Set up training and test sets
# df_D_ts[,"count"]
# Baseline model
y <- df_D_ts["2022-01-01","count"]
X <- window(df_D_ts,
            end = "2021-01-01")
X <- X[,"count"]

# predict
naive(X, h = 12)

# 10 year model

# create a df with all years
# get the name and bind with year
# check for missing values
# impute missing values



# get just year and count vars
df_D_ts <- df_D %>%
    select(year, count)

# data visual
plot(df_D_ts)
# plot trend line
abline(reg = lm(df_D_ts$count ~ df_D_ts$year))

# convert to time series
df_D_ts <- ts(df_D_ts)

# ts_D_ts <- ts(df_D_ts$count)

# decompose data
# ddf <- decompose(df_D_ts, "multiplicative")  # there is no seasonality, so this doesn't apply here

# create arima model
mymodel <- auto.arima(df_D_ts[,1])

# plot the residuals
plot.ts(mymodel$residuals)

# forecast for the next ten years
myforecast <- forecast(mymodel, h=10)
plot(myforecast)

##### Forecast a few names

##### Forecast all names