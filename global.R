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

# atu
source("atu.R")

# modules -----------------------------------------------------------------------
source("modules/home-page.R")
source("modules/temperature-page.R")
source("modules/chinook-module.R")
source("modules/flow-page.R")
source("modules/about-page.R")
source("modules/welcome.R")

# load general objects, documented in "data/make-general-objects.R"
load("data/general-objects.RData")


diversion_data <- read_csv("data/flows/srsc_diversion_data.csv") %>% 
  mutate(draft_date = mdy(draft_date))

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

redd_data <- 
  read_csv("https://s3-us-west-2.amazonaws.com/showr-data/cdfw/redds/aerial-survey-observations_no_error_codes.csv", 
                  col_types = cols(
                    date = col_date(format = ""),
                    location = col_character(),
                    race = col_character(),
                    counts = col_double()
                  )) %>% filter(race == "Winter")

carcass_data <- read_rds("data/chinook/carcass_static_data.rds")

# shape files for redd map 
redd_reach <- readOGR("data/redd_reaches/redd_reach.shp", stringsAsFactors = FALSE)
redd_reach <- spTransform(redd_reach, CRS("+proj=longlat +datum=WGS84 +no_defs"))
carcass_reach <- readOGR("data/carcass_reaches/carcass_reach_line.shp")
carcass_reach <- spTransform(carcass_reach, CRS("+proj=longlat +datum=WGS84 +no_defs"))
carcass_location <- carcass_reach$Reach

# TODO: this is ok for now but needs to be modified, by either having the data
# come into the shiny app with the sections names already in the dataset
carcass_section_to_reach_name <- data.frame(
  river_section = as.character(1:4), 
  section_name = carcass_reach$Reach, 
  stringsAsFactors = FALSE
)

carcass_data <- 
  left_join(carcass_data, carcass_section_to_reach_name, c("river_section" = "river_section"))

# TCD Configurations
tcd_configs_data <- read_rds("data/tcd_configurations/tcd_configs_through_2017-08-24.rds")

# temp locations metadata 
cdec_temperature_locations <- read_rds("data/temperatures/cdec_temperature_locations.rds")

model_temps <- read_csv("https://s3-us-west-2.amazonaws.com/svproducers-data/cvtemp_data/2017-08-25_10%3A00%3A15_model_output.csv")
model_temps$datetime <- as_date(model_temps$datetime)

# winter run presence 
wr_presence_data <- read_csv("data/chinook/wr_chinook_presence.csv")

# water year index classifications
historic_water_year_types <- 
  readr::read_rds("data/operations/historical-water-year-index.rds") %>% 
  tibble::add_row(year =2017, yr_type="W") 
current_water_year_types <- 
  readr::read_rds("data/operations/2018-04-17-water-year-index.rds")


pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}

isothermal_data <- read_rds("data/operations/shasta_storage_temperature.rds")

get_year_classification <- function(y) {
  if (y != year(today())) {
    historic_water_year_types %>% 
      filter(year == y) %>% 
      mutate(classification = wy_class_lookups[yr_type]) %>% 
      pull(classification) %>% 
      as.character()
  } else {
    current_water_year_types %>% 
      filter(probability == 99) %>% 
      arrange(desc(date)) %>% 
      head(1) %>% 
      pull(classification) %>% 
      as.character()
  }
}