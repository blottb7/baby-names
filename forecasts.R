### Libraries
library(tidyverse)
library(gganimate)
library(scales)
library(forecast)
# library(xts)

### Data
# Data Comes from https://www.ssa.gov/oact/babynames/names.zip

# Read In Data Files
all_data <- map_dfr(dir('./data'), function(x){
    read_csv(paste0('./data/', x), 
             col_names = c('name', 'gender', 'count'),
             col_types = list('c', 'c', 'i')) %>%
        mutate(year = x %>% str_extract('\\d+') %>% as.integer)
})

##### Forecast

### ...a single name:

# get one name
df_D <- all_data %>%
    filter(name == "Delilah")

# select just the count column
df_D <- df_D |>
    select(count)

# get summary statistics
summary(df_D)
summary(df_D$count)

# convert to time_series
ts(df_D, start=c(1880))
# ts(df_D['count'],start=c(1880))
df_D_ts <- ts(df_D,start=c(1880))

# Check the plot
plot.ts(ts(df_D,start=c(1880)))
plot.ts(df_D_ts)
# just count var
# plot.ts(ts(df_D['count'],start=c(1880)))

# df_D_ts <- xts(df_D, order.by = as.Date(as.yearmon(df_D$year)), start=c(1880))
# df_D_ts <- xts(df_D, order.by = as.Date(as.yearmon(df_D$year)))

### Set up training and test sets
# Baseline model

HoltWinters(df_D_ts, beta = FALSE, gamma = FALSE)
D_HW_forecasts <- HoltWinters(df_D_ts, beta = FALSE, gamma = FALSE)
D_HW_forecasts$fitted

# plot ts vs forecasts
plot(D_HW_forecasts)

# select count column and last year for y-variable
y <- df_D_ts["2022-01-01","count"]

# select all but last year for X-set
X <- window(df_D_ts,
            end = "2021-01-01")
# select only count column
X <- X[,"count"]

### predict

# visualize
plot.ts(X)

# naive(X, h = 12)

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