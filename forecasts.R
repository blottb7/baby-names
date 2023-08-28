### Libraries
library(tidyverse)
library(gganimate)
library(scales)
library(forecast)

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

# get one name: "Delilah"
df_D <- all_data %>%
    filter(name == "Delilah")

# select just the count column
df_D <- df_D |>
    select(count)

# get summary statistics
summary(df_D)
# summary(df_D$count)

# convert to time_series
df_D_ts <- ts(df_D,start=c(1880))

# Check the plot
plot.ts(df_D_ts)

# Forecast HoltWinter model (no seasonal, but a trend)
HoltWinters(df_D_ts, beta = TRUE, gamma = FALSE)
D_HW_forecasts <- HoltWinters(df_D_ts, beta = TRUE, gamma = FALSE)

# check values
D_HW_forecasts$fitted

# plot ts vs forecasts
plot(D_HW_forecasts)

# Run Holt Winters for years beyond data set
# 1
forecast:::forecast.HoltWinters(D_HW_forecasts, h=1)

# 10
forecast:::forecast.HoltWinters(D_HW_forecasts, h=10)

# 100
forecast:::forecast.HoltWinters(D_HW_forecasts, h=100)

# Append n years of forecasts for the new name onto the main df
future_forecasts <- forecast:::forecast.HoltWinters(D_HW_forecasts, h=10)
round(future_forecasts[[4]],0)


##### Forecast a few names

# create a df with all years
ts(0, start = 1880, end = 2022, frequency = 1)

# get the name and bind with year
# check for missing values
# impute missing values

##### Forecast all names