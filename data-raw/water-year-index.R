url <- "http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST"

library(rvest)
library(stringr)
library(tidyverse)

current_water_year_classifications <- url %>% 
  read_html() %>% 
  html_nodes("pre") %>% 
  html_text() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  tail(-20) %>% 
  head(112) %>% 
  map_df(function(x) {
    split_row <- flatten_chr(str_split(x, " "))
    split_row <- split_row[str_detect(split_row, "")]
    tibble(
      water_year = split_row[1],
      sac_valley_class = split_row[6], 
      san_joaquin_class = split_row[11]
    )
  })
  
write_rds(current_water_year_classifications, "data/operations/current_water_year_classifications.rds")
