library(tidyverse)
library(gganimate)
library(scales)
library(forecast)

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

# convert year to variable type year
# as.Date(ISOdate(df_D$year, month = 1, day = 1))
# as.Date(paste(df_D$year, 1, 1, sep = "-"))
# 
# library(zoo)
# as.Date(as.yearmon(df_D$year))

df_D$year <- year(as.Date(as.character(df_D$year), format = "%Y"))


# get summary
summary(df_D$count)

##### convert to time_series
# select only necessary variables
df_D_ts_vars <- df_D %>%
    select(count, year)



# xts(df_D_ts_vars, order.by = year(df_D_ts_vars$year))

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