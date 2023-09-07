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
baby_names <- c("Albert", "Delilah", "Samantha")
baby_genders <- c("M", "F")

# function for every baby name and gender
forecast_baby_names <- function(df) {
    for (n in baby_names) {
        for (g in baby_genders) {
            
            
            df_name <- df %>%
                filter(g == gender) %>%
                filter(n == name)
            
            ### CLEAN
            
            # continue if name, gender combo exists in database
            if (nrow(df_name) > 0) {
                
                # combine df's to get all years
                df_name <- df_year %>%
                    left_join(df_name)
                
                # Fill in missing values in joined data frame
                df_name$name[is.na(df_name$name)] <- n
                df_name$gender[is.na(df_name$gender)] <- g
                df_name$count[is.na(df_name$count)] <- 0
                
                # print head or tail of df
                # print(tail(df_name))
                
                # print name and gender for testing
                # print(c(n, g))
                
                # select just the count column and convert to time series
                df_name_ts <- df_name |>
                    select(count) |>
                    ts(start=c(1880))
                
                # print time series
                # print(tail(df_name_ts))
                
                # Forecast HoltWinter model (no seasonal, but a trend)
                D_HW_forecasts <- HoltWinters(df_name_ts, beta = TRUE, gamma = FALSE)
                
                # check values
                D_HW_forecasts$fitted
                
                # plot ts vs forecasts
                # plot(D_HW_forecasts)
                
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
                
                # print df_fore
                # print(df_fore)
                
                ### Join forecasts with original, cleaned data
                # Create a list of two dataframes
                df_list <- list(df_name, df_fore)
                
                # Join the dataframes into the original
                df_name <- df_list %>%
                    reduce(full_join, by = c("year", "count"))
                
                # Fill in missing values in joined data frame
                df_name$name[is.na(df_name$name)] <- n
                df_name$gender[is.na(df_name$gender)] <- g
                df_name$count[is.na(df_name$count)] <- 0
                
                # Join forecasted data with all data
                df <- df %>%
                    full_join(df_name)
                
                # print tail all_data
                print(tail(df, 10))
            }
            
        }
    }
    return(df)
}

forecast_baby_names(all_data)
