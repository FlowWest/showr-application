### NO TEMPERATURE DATA BEFORE 2010!!!!
rd <- redd_data %>%
  filter(counts > 0, year(date) >= 2010) %>% 
  mutate(redd_id = row_number(date)) %>% 
  rowwise() %>% 
  do(
    tibble(
      date = seq(.$date, estimate_emergence(.$date, .$location) -1, by="day"), # this give a sequence from the seed day to the estiamted emergence value
      seed_day = .$date,
      location = .$location, 
      counts = .$counts, # how many redds will exist in the water for this time
      redd_id = .$redd_id
    )
  ) %>% ungroup() %>% 
  mutate(cdec_gage = redd_cdec_lookup[location], 
         location = factor(location, levels = unique(redd_data$location))) %>%
  left_join(daily_temps)


# the redd hatching distribution 
chinook_hatch <- function(temp) {
  log(0.08646) + (1.23473 * log(t + 2.26721)) 
}

redd_hatching <- function(temp) {
  log(0.08646) + (1.23473 * log(temp + 2.26721)) 
}


rd %>% 
  filter(year(date) == 2018) %>% 
  group_by(redd_id) %>% 
  mutate(hatching_devel = redd_hatching(daily_mean), 
         hatching_accum = cumsum(hatching_devel)) %>%
  transmute(
    date,
    seed_day, 
    location, 
    counts, 
    exp(hatching_devel),
    hatching_accum) %>% ungroup() %>%  
  plot_ly(x=~date, y=~counts, type='bar', color=~location) %>% 
  layout(barmode='stack') 

rd_with_hatching <-
  rd %>% 
  filter(year(date) == 2016) %>% 
  group_by(redd_id) %>% 
  mutate(hatching_devel = redd_hatching(daily_mean), 
         hatching_accum = cumsum(hatching_devel)) %>%
  transmute(
    date,
    seed_day, 
    location, 
    counts, 
    hatching_accum, 
    hatching_day = hatching_accum > 100) %>% 
  ungroup() %>% 
  group_by(redd_id) %>% 
  mutate(
    flag = cumsum(hatching_day), 
    hatching_day = date[which(flag == 1)],
    hatching_day_bool = date == hatching_day,
    has_hatched = date >= hatching_day
    ) %>% ungroup()



rd_with_hatching %>% 
  plot_ly(x=~date, y=~counts, color=~has_hatched, type='bar', 
          opacity=~has_hatched) %>% 
  layout(barmode="stack")




