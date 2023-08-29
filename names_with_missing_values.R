

# create a df with all years
ts(0, start = 1880, end = 2022, frequency = 1)
# as.data.frame(matrix(0, nrow = length(unique(all_data$year))))
count_matrix <- matrix(0, nrow = length(unique(all_data$year)))
# seq(1880, 2022, 1)
year_matrix <- matrix(seq(1880, 2022, 1))
matrix(c(0, seq(2023, 2032, 1)), nrow = length(unique(all_data$year)), ncol = 2)

# get a name with missing counts
df_D <- all_data %>%
    filter(name == "Zylon")

# reserve that data
df_D_copy <- df_D

# select just the count column
df_D <- df_D |>
    select(count)

# get the name and bind with year

# check for missing values
# impute missing values