# This file forecasts any name in the data

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

# Instantiate an empty dataframe
# create a df with all years
df_year <- as.data.frame(matrix(seq(1880, 2022, 1)))

# name the column
names(df_year) <- "year"

##### Forecast #####
# get one name
df_D <- all_data %>%
    filter(name == "Delilah")

# reserve that data
df_D_copy <- df_D

# combine df's to get all years
df_D <- df_year %>%
    left_join(df_D)

# select just the count column
df_D <- df_D |>
    select(count)

# convert to time_series
df_D_ts <- ts(df_D,start=c(1880))

# Forecast HoltWinter model (no seasonal, but a trend)
D_HW_forecasts <- HoltWinters(df_D_ts, beta = TRUE, gamma = FALSE)

# check values
D_HW_forecasts$fitted

# plot ts vs forecasts
plot(D_HW_forecasts)

# Append n years of forecasts for the new name onto the main df
# Save forecasts in an object
future_forecasts <- forecast:::forecast.HoltWinters(D_HW_forecasts, h=10)

# save mean forecasts in an object
future_forecasts_mean <- round(future_forecasts$mean, 0)

# convert from ts to dataframe with row names as years
df_fore <- as.data.frame(matrix(future_forecasts_mean, dimnames = list(seq(2023, 2032, 1), "count")))

# convert row names to year column
df_fore <- rownames_to_column(df_fore, "year")

# convert year from character to integer
df_fore$year <- as.integer(df_fore$year)

### Join forecasts with original data
# Create a list of two dataframes
df_list <- list(df_D_copy, df_fore)

# Join the dataframes into the original
df_D_copy <- df_list %>%
    reduce(full_join, by = c("year", "count"))

# Fill in missing values in joined data frame
df_D_copy$name[is.na(df_D_copy$name)] <- df_D_copy$name[1]
df_D_copy$gender[is.na(df_D_copy$gender)] <- df_D_copy$gender[1]
df_D_copy$count[is.na(df_D_copy$count)] <- 0

