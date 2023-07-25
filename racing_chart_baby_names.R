library(tidyverse)
library(gganimate)
library(scales)

# Data Comes from https://www.ssa.gov/oact/babynames/names.zip

##Read In Data Files
all_data <- map_dfr(dir('.data'), function(x){
  read_csv(paste0('.data/', x), 
           col_names = c('name', 'gender', 'count'),
           col_types = list('c', 'c', 'i')) %>%
    mutate(year = x %>% str_extract('\\d+') %>% as.integer)
})

#Get Top 10 Per Year
top_10_names <- all_data %>% 
  group_by(gender, year) %>%
  top_n(10, wt = count) %>% 
  mutate(rank = rank(count))


gen_graph <- function(cond, output_file){
  
  if(cond == "F"){
    lbl = "Girl"
    col = "#FFC0CB"
  }else{
    lbl = "Boy"
    col = "#89cff0"
  }
  
  animated <- top_10_names %>% 
    filter(gender == cond) %>%
    ggplot(aes(x = rank, y = count/2, group = name)) + 
    geom_col(fill = col) + 
    geom_text(aes(label = count %>% comma(accuracy = 1)), hjust = 0, size = 10) + 
    geom_text(aes(label = name), y = 0, vjust = .2, hjust = 1, size = 10) +
    labs(x = paste0(lbl,"'s Name"), y = "# of Babies",
         title = paste0("Top 10 ", lbl, "'s Baby Names (1880-2018)"),
         subtitle = '{round(frame_time,0)}',
         caption = 'Source: Social Security Administration') + 
    coord_flip(clip = 'off') + 
    theme_minimal() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line(size=.4, 
                                            color="grey" ),
          panel.grid.minor.x = element_line(size=.1, 
                                            color="grey" ),
          plot.title.position = "plot",
          plot.title=element_text(size=20, 
                                  face="bold", 
                                  colour="#313632"),
          plot.subtitle=element_text(size=50, 
                                     color="#a3a5a8"),
          plot.caption =element_text(size=15, 
                                     color="#313632"),
          plot.background=element_blank(),
          plot.margin = margin(1, 9, 1, 9, "cm")) + 
    transition_time(year) + 
    ease_aes('cubic-in-out') +
    view_follow(fixed_x = T)

  animate(animated, fps = 10, duration = 50, width = 1000, height = 600, 
          end_pause = 50, start_pause = 50,
          renderer=gifski_renderer(output_file))
    
}

gen_graph("F", "baby_girls.gif")
gen_graph("M", "baby_boys.gif")