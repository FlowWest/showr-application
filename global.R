library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(leaflet)
library(rgdal)
library(zoo)
library(stringr)
library(readr)
library(tidyr)
library(forcats)
library(readr)
library(sparkline)
library(shinyjs)
library(measurements)
library(purrr)
library(shinytoastr)
library(shinyWidgets)
library(shinycssloaders)
library(sf)

enableBookmarking(store = "url")

# atu
source("atu.R")

# modules -----------------------------------------------------------------------
source("modules/dashboard-page.R")
source("modules/temperature-page.R")
source("modules/winter-run-chinook-page.R")
source("modules/flow-page.R")
source("modules/about-page.R")
source("modules/welcome.R")
source("modules/shallow-redds-page.R")

# load general objects, documented in "data/make-general-objects.R"
load("data/general-objects.RData")


sac_river_miles <- 
  st_read("data/sac-river-river-mile-markers/river_mile_markers_sacriver_2012.shp") %>% 
  st_cast("POINT") %>% 
  as_tibble() %>% 
  transmute(
    river_mile = as.character(MARKER),
    lat, lon
  )

shallow_redds_danger <- read_csv("data/chinook/shallow-redds-temp.csv")

# diversion_data <- read_csv("data/flows/srsc_diversion_data.csv") %>% 
#   mutate(draft_date = mdy(draft_date))
diversion_data <- read_rds("data/flows/total-diversions-2019-07-23.rds")

upstream_diversions <- read_rds("data/flows/upstream-diversions-2019-07-23.rds")
downstream_diversions <- read_rds("data/flows/downstream-diversions-2019-07-23.rds")

# These data are all on a public S3 bucket 
temp_data <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data-site/showr_hourly_temps.csv", 
           col_types = cols(
             datetime = col_datetime(format = ""),
             location_id = col_character(),
             parameter_id = col_integer(),
             parameter_value = col_double()
           ))

temp_compliance_points_daily_mean <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data-site/showr_tempatures.csv", 
           col_types = cols(
             datetime = col_date(format = ""),
             location_id = col_character(),
             parameter_id = col_integer(),
             parameter_value = col_double()
           ))

flow_data <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data-site/showr_hourly_flows.csv",
           col_types = cols(
             location_id = col_character(),
             parameter_id = col_integer(),
             datetime = col_datetime(format = ""),
             parameter_value = col_integer()
           ))

flow_data_daily_mean <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data-site/showr_flow.csv", 
           col_types = cols(
             datetime = col_date(format = ""),
             location_id = col_character(),
             parameter_id = col_integer(),
             parameter_value = col_double()
           ))

shasta_storage_data <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data-site/showr_ops.csv", 
           col_types = cols(
             datetime = col_date(format = ""),
             location_id = col_character(),
             parameter_id = col_integer(),
             parameter_value = col_double()
           ))

redd_air_temp <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data-site/showr_daily_max_air_temp.csv", 
           col_types = cols(
             location_id = col_character(),
             parameter_id = col_integer(),
             datetime = col_datetime(format = ""),
             parameter_value = col_integer()
           )) 

redd_data <- 
  read_rds("data/chinook/2019-07-26-redd-counts.rds") %>% 
  filter(race == "Winter") 
# %>% 
#   mutate(location = factor(location, levels = redd_locations))

carcass_data <- read_rds("data/chinook/carcass_static_data.rds")

# shape files for redd map 
redd_reach <- readOGR("data/redd_reaches/redd_reach.shp", stringsAsFactors = FALSE)
redd_reach <- spTransform(redd_reach, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# TCD Configurations
tcd_configs_data <- read_rds("data/tcd_configurations/tcd_configs_through_2017-08-24.rds")

# temp locations metadata 
cdec_temperature_locations <- read_rds("data/temperatures/cdec_temperature_locations.rds")

model_temps <- read_csv("data/temperatures/cvtemp/sim_run_2019-05-24.csv") 
# %>% 
#   filter(model_type == "usbr_no_w2", 
#          scenario_name == "may_23_2018_input_90_output_90_50l3mto")
# model_temps$datetime <- as_date(model_temps$datetime)

# winter run presence 
wr_presence_data <- read_csv("data/chinook/wr_chinook_presence.csv")

# water year index classifications

historic_water_year_types <- 
  readr::read_rds("data/operations/current_water_year_classifications.rds") %>% 
  add_row("water_year" = 2018, "sac_valley_class"="BN", "san_joaquin_class"=NA)

# current_water_year_types <- 
#   readr::read_rds("data/operations/2018-04-17-water-year-index.rds")


pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}

isothermal_data <- read_rds("data/operations/shasta-temp-profile-june-2019.rds")

get_year_classification <- function(y) {
  if (y != year(today())) {
    historic_water_year_types %>% 
      filter(water_year == y) %>% 
      mutate(classification = wy_class_lookups[sac_valley_class]) %>% 
      pull(classification) %>% 
      as.character()
  } else {
    wy_class_lookups["AN"]
  }
}

# a naive similar year picker just looks at the water year index
get_similar_year <- function(yr) {
  
  as.numeric(c(
    "BN" = 2016,
    "W" = 2017,
    "D" = 2014, 
    "C" = 2013
  )[class])
}

# for winter run emergence
daily_temps <- temp_data %>% 
  group_by(cdec_gage = location_id, date = as_date(datetime)) %>% 
  summarise(
    daily_mean = mean(parameter_value, na.rm = TRUE)
  ) %>% ungroup() %>% 
  mutate(temp_type="actual")

max_daily_temp <- max(daily_temps$date, na.rm = TRUE)

model_temp_to_append <- model_temps %>% filter(datetime > max_daily_temp) %>% 
  transmute(
    date = datetime,
    cdec_gage = location_id,
    daily_mean = temp_50, 
    temp_type="model"
  )

daily_temps <- bind_rows(
  daily_temps,
  model_temp_to_append
)

historical_daily_min_max_temps <- read_rds("data/temperatures/historical-daily-min-max.rds")
historical_daily_min_max_flows <- read_rds("data/flows/flow_historical_daily_min_max.rds")

### NO TEMPERATURE DATA BEFORE 2010!!!!
rd <- redd_data %>%
  filter(counts > 0, year(date) >= 2010) %>% 
  mutate(redd_id = row_number(date)) %>% 
  rowwise() %>% 
  do(
    tibble(
      date = seq(.$date, as_date(estimate_emergence(.$date, .$location)) -1, by="day"), # this give a sequence from the seed day to the estiamted emergence value
      seed_day = .$date,
      location = .$location, 
      counts = as.integer(.$counts), # how many redds will exist in the water for this time
      redd_id = .$redd_id
    )
  ) %>% ungroup() %>% 
  mutate(cdec_gage = redd_cdec_lookup[location], 
         location = factor(location, levels = unique(redd_data$location))) %>%
  left_join(daily_temps)


make_estimated_diversion <- function(year) {
  diversion_data %>% 
    group_by(month = month(draft_date), 
             day = day(draft_date)) %>% 
    summarise(
      min = min(actual_upstream, na.rm = TRUE),
      max = max(actual_upstream, na.rm = TRUE),
      mean = mean(actual_upstream, na.rm = TRUE)
    ) %>% 
    mutate(
      fake_date = ymd(paste0(year, "/", month, "/", day))
    ) }


compact <- function(x) {
  empty <- vapply(x, is_empty, logical(1))
  x[!empty]
}

is_empty <- function(x) length(x) == 0

redd_hatching <- function(temp) {
  exp(log(0.08646) + (1.23473 * log(temp + 2.26721))) 
}
