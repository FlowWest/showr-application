# functions for automating the calculations of atu's and redd emergence 

get_gage_location <- function(redd_location) {
  as.character(redd_cdec_lookup[redd_location])
}


# given a date of observation and the location of it, we can estiamte emeergence
estimate_emergence <- function(.date, redd_location) {
  # get the appropriate cdec/cvtemp location 
  gage <- get_gage_location(redd_location)
  
  daily_temps <- 
    temp_data %>% 
    filter(location_id == gage) %>% 
    group_by(location_id, date = as_date(datetime)) %>% 
    summarise(daily_mean = mean(parameter_value, na.rm = TRUE))
  
  atu_df <- daily_temps %>% 
    filter(location_id == gage, date >= .date) %>% 
    mutate(atu = cumsum(daily_mean)) 
  
  emergence <- atu_df %>% filter(atu >= 1800)
  
  if (nrow(emergence) == 0) {
    current_atu <- atu_df %>% tail(1) %>% pull(atu)
    
    if (length(current_atu) == 0) current_atu <- 0
    
    thresh <- 1800 - current_atu
    # to debug
    #cat("Model needs to get to: ",thresh, "\n\n")
    
    # NOTE: the added >=.date here is for projecting of yet unseen redds
    # to maybe provide hypothetical redd mapping
    model_estimated_emergence <- 
      model_temps %>% filter(location_id == gage, datetime >= .date) %>% 
      mutate(atu = cumsum(temp_50)) %>%
      filter(atu >= thresh) %>% 
      arrange(datetime) %>% 
      head(1)
    
    # na indicates the model is unable to project that far in the future
    # this usually means that the redd was observed TODAY
    if (nrow(model_estimated_emergence) == 0) {
      return(NA)
    } else{ 
      return(model_estimated_emergence %>% pull(datetime))  
    } 
    
  } else {
    return(emergence %>% head(1) %>% pull(date))
  } 
}



