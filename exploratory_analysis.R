f_2018 <- all_data %>%
    filter(year == '2018',
           gender == 'F')

which(f_2018$name == "Delilah")

f_year <- all_data %>%
    filter(year == '2022',
           gender == 'F')
which(f_year$name == "Dolphin")

# Dolphin and other unusual names
all_data %>%
    filter(name == "Peyton") %>%
    arrange(desc(count))
