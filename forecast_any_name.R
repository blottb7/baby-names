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
df_name <- all_data %>%
    filter(name == "Susan") %>%
    filter(gender == "F")

# reserve that data
df_name_copy <- df_name

# combine df's to get all years
df_name <- df_year %>%
    left_join(df_name)

# select just the count column
df_name <- df_name |>
    select(count)

# convert to time_series
df_name_ts <- ts(df_name,start=c(1880))

# Forecast HoltWinter model (no seasonal, but a trend)
D_HW_forecasts <- HoltWinters(df_name_ts, beta = TRUE, gamma = FALSE)

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
df_list <- list(df_name_copy, df_fore)

# Join the dataframes into the original
df_name_copy <- df_list %>%
    reduce(full_join, by = c("year", "count"))

# Fill in missing values in joined data frame
df_name_copy$name[is.na(df_name_copy$name)] <- df_name_copy$name[1]
df_name_copy$gender[is.na(df_name_copy$gender)] <- df_name_copy$gender[1]
df_name_copy$count[is.na(df_name_copy$count)] <- 0

# Join forecasted data with all data
all_data <- all_data %>%
         full_join(df_name_copy)
