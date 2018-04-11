# library imports -----------------------------------------------------------------
library(shiny)
#library(DBI)
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

# atu
source("atu.R")

# modules -----------------------------------------------------------------------
source("modules/home-page.R")
source("modules/temperature-page.R")
source("modules/chinook-module.R")
source("modules/flow-page.R")
source("modules/about-page.R")

# load general object, documented in "data/make-general-objects.R"
load("data/general-objects.RData")


diversion_data <- read_csv("data/srsc_diversion_data.csv") %>% 
  mutate(draft_date = mdy(draft_date))


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


redd_locations <- distinct(redd_data, location) %>% pull(location)

carcass_data <- read_rds("data/carcass_static_data.rds")

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
cdec_temperature_locations <- read_rds("data/cdec_temperature_locations.rds")

# redd location to cdec location lookup table 
redd_to_cdec_location <- read_rds("data/redd_location_to_cdec_location.rds")

model_temps <- read_csv("https://s3-us-west-2.amazonaws.com/svproducers-data/cvtemp_data/2017-08-25_10%3A00%3A15_model_output.csv")
model_temps$datetime <- as_date(model_temps$datetime)

# winter run presence 
wr_presence_data <- read_csv("data/wr_chinook_presence.csv")

pretty_num <- function(num, places = 2) {
  format(round(num, places), big.mark = ',', drop = FALSE)
}

#TODO(emanuel) this is a hack at the moment, need to fix the source soon! -ergz
# redd to cdec
redd_to_cdec_location$location <- redd_to_cdec_location$location %>% 
  str_replace_all("Br ", "Bridge ") %>%
  str_replace_all("Br$", "Bridge") %>% 
  str_replace_all("Rd", "Road")



# textInputRow<-function (inputId, label, value = "") 
# {
#   div(style="display:inline-block",
#       tags$label(label, `for` = inputId), 
#       tags$input(id = inputId, type = "text", value = value,class="input-small"))
# }
# 
# cold_water_pool_2018 <- 
#   gs_url("https://docs.google.com/spreadsheets/d/1fkl4RTjOADGNjgUD56HLKMdC0xd1olaw91PFbKiNY3s/edit?usp=sharing") %>% 
#   gs_read(ws = "2017")


