library(lubridate)

redd_data <- 
  read_rds("data/chinook/aerial-survey-observations_no_error_codes.rds") %>% 
  filter(race %in% c("Winter", "Fall")) 

r <- redd_17 %>% 
  filter(race == "Fall") %>% 
  group_by(date) %>% 
  summarise(total = sum(counts, na.rm = TRUE))

rd %>% 
  filter(year(date) == 2017) %>% 
  plot_ly() %>% 
  add_bars(x=~date, y=~counts, color=~location) %>% 
  add_trace(data=r, x=~date, y=~total, mode='markers')
