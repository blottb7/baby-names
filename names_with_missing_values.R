# create a df with all years
ts(0, start = 1880, end = 2022, frequency = 1)
count_matrix <- matrix(0, nrow = length(unique(all_data$year)))
year_matrix <- matrix(seq(1880, 2022, 1))

# join the two matrices
df_empty <- as.data.frame(count_matrix %>%
    bind_cols(year_matrix))
names(df_empty) <- c("count", "year")

df_year <- as.data.frame(year_matrix)
names(df_year) <- "year"

df_year %>%
    left_join(df_D)

# get a name with missing counts
df_D <- all_data %>%
    filter(name == "Zylon")

# fill in values where they exist
for(year in df_empty$year) {
    
    # if(year %in% df_D$year) {
    #     print(year)
    # }
    
    ifelse(year %in% df_D_copy, print(df_D_copy$count), 0)
    
}


# reserve that data
df_D_copy <- df_D

# select just the count column
df_D <- df_D |>
    select(count)

# get the name and bind with year

# check for missing values
# impute missing values