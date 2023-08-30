# create a df with all years
df_year <- as.data.frame(matrix(seq(1880, 2022, 1)))
names(df_year) <- "year"

# get a name with missing counts
df_D <- all_data %>%
    filter(name == "Zylon")

# reserve that data
df_D_copy <- df_D

# combine df's to get all years
df_D <- df_year %>%
    left_join(df_D)

# Fill in missing values in joined data frame
df_D$name[is.na(df_D$name)] <- df_D_copy$name[1]
df_D$gender[is.na(df_D$gender)] <- df_D_copy$gender[1]
df_D$count[is.na(df_D$count)] <- 0

# reserve that data
df_D_copy <- df_D

# select just the count column
df_D <- df_D |>
    select(count)
