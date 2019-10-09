library(zoo)
library(CDECRetrieve)
library(tidyverse)


# SHOWR HOURLY TEMPS -----------------------------------------------------------

hourly_temp_locations <- c("kwk", "ccr", "jlf", "bnd", "bsf")
showr_hourly_temps <- hourly_temp_locations %>% 
  map_df(function(location) {
    cdec_query(station = location, sensor_num = "25", dur_code = "h", 
               start_date = "2010-01-01")
}) %>% 
  transmute(datetime, 
            location_id = tolower(location_id), 
            parameter_id = parameter_cd, 
            parameter_value = ifelse(parameter_value < 40, as.numeric(NA), parameter_value)) %>% 
  filter(datetime <= now()) 

# fix missing values; just do linear interpolation
showr_hourly_temps_fixed <- showr_hourly_temps %>% 
  arrange(location_id, datetime) %>% 
  fill(parameter_value) %>% 
  mutate(parameter_value = round(parameter_value, 1))
  
write_csv(showr_hourly_temps_fixed, "data/temperatures/showr-hourly-temps-2019-10-09.csv")

# SHOWR DAILY MEAN TEMPS -------------------------------------------------------

showr_hourly_temps <- read_csv("data/temperatures/showr-hourly-temps-2019-10-09.csv")

showr_daily_mean_temps <- showr_hourly_temps %>% 
  group_by(location_id, datetime = as_date(datetime)) %>% 
  summarise(
    parameter_id = first(parameter_id), 
    parameter_value = mean(parameter_value, na.rm = TRUE)
  ) %>% ungroup() %>% 
  mutate(parameter_value = round(parameter_value, 1))

write_csv(showr_daily_mean_temps, "data/temperatures/showr-daily-mean-temps-2019-10-09.csv")

# SHOWR HOURLY FLOWS -----------------------------------------------------------

flow_locations <- c("sha", "kwk", "bnd", "wlk")

showr_hourly_flows <- flow_locations %>% 
  map_df(function(location) {
    if (location == "sha")
      cdec_query(station = location, sensor = "8", dur_code = "d", start_date = "2010-01-01")
    else 
      cdec_query(station = location, sensor = "20", dur_code = "h", start_date = "2010-01-01")
  })  %>% 
  transmute(datetime, location_id = tolower(location_id), 
            parameter_id = parameter_cd, parameter_value) %>% 
  filter(datetime <= now())

showr_hourly_flows_fixed <- showr_hourly_flows %>% 
  arrange(location_id, datetime) %>% 
  fill(parameter_value) %>% 
  mutate(parameter_value = round(parameter_value, 1))

write_csv(showr_hourly_flows_fixed, "data/flows/showr-hourly-flow-2019-10-09.csv")

# SHOWR DAILY FLOWS -------------------------------------------------------------

showr_hourly_flows <- read_csv("data/flows/showr-hourly-flow-2019-10-09.csv")

showr_daily_flows <- showr_hourly_flows %>% 
  group_by(location_id, datetime = as_date(datetime)) %>% 
  summarise(
    parameter_id = first(parameter_id), 
    parameter_value = mean(parameter_value, na.rm = TRUE)
  ) %>% ungroup() %>% 
  mutate(parameter_value = round(parameter_value, 1))


write_csv(showr_daily_flows, "data/flows/showr-daily-flow-2019-10-09.csv")


