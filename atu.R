# functions for automating the calculations of atu's and redd emergence 
get_gage_location <- function(redd_location) {
  as.character(redd_cdec_lookup[redd_location])
}

# given a date of observation and the location of it, we can estiamte emergence 
# using Accumulated Temperature Units
# the logic is simple, use the date of spawn as the start of accumulation
# use the the reach of spawn to select appropriate cdec location.
# summarize cdec temp data to daily mean, and accumulate until the value of 1800 is
# obtained.
estimate_emergence <- function(spawn_date, redd_location) {
  # get the appropriate cdec/cvtemp location 
  gage <- get_gage_location(redd_location)
  spawn_date <- as_date(spawn_date)
  
  # summarize hourly data to a daily mean
  atu <- 
    temp_data %>%
    filter(location_id == gage) %>% 
    group_by(location_id, date = as_date(datetime)) %>% 
    summarise(daily_mean = mean(parameter_value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(date) %>% 
    filter(date >= spawn_date) %>% 
    mutate(daily_mean_C = measurements::conv_unit(daily_mean, "F", "C"), 
           atu = cumsum(daily_mean_C))
  
  
  emergence <- atu %>% filter(atu >= 958)
  
  # if the 'emergence' dataframe is empty then we have not reached
  # the emergence value, and must proceed using modeled temp data.
  if (nrow(emergence) == 0) {
    current_atu <- atu %>% tail(1) %>% pull(atu) # store latest accumulated value
    current_date <- atu %>% tail(1) %>% pull(date)
    
    if (length(current_atu) == 0) current_atu <- 0 # cases where no atu 
    
    thresh <- 958 - current_atu # new thresh will be old thresh minus current atu
    
    model_estimated_emergence <- 
      model_temps %>%
      arrange(datetime) %>% 
      filter(location_id == gage, datetime > current_date) %>% 
      mutate(daily_mean_C = measurements::conv_unit(temp_50, "F", "C"), 
             atu = cumsum(daily_mean_C)) %>%
      filter(atu >= thresh) %>%
      head(1)
    
    # na indicates the model is unable to project that far in the future
    # this usually means that the redd was observed and its emergence is
    # predicted to extend passed november
    if (nrow(model_estimated_emergence) == 0) {
      return(NA)
    } else{ 
      return(model_estimated_emergence %>% pull(datetime))  
    } 
    
  } else {
    return(emergence %>% head(1) %>% pull(date))
  } 
}