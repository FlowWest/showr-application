library(profvis)

# how much faster is it to load an object as opposed to create the object 
# on the apprun call
# here i look at three different types of objects 

# a vector with names 
profvis({
redd_cdec_lookup <- c(
  "Keswick to ACID Dam" = "kwk", 
  "ACID Dam to Highway 44 Bridge" = "kwk",
  "Highway 44 Bridge to Airport Road Bridge" = "ccr",
  "Airport Road Bridge to Balls Ferry Bridge" = "and",
  "Balls Ferry Bridge to Battle Creek" = "bsf",
  "Battle Creek to Jellys Ferry Bridge" = "bsf",
  "Jellys Ferry Bridge to Bend Bridge" = "jlf",
  "Bend Bridge to Red Bluff Diversion Dam" = "bnd",
  "Red Bluff Diversion Dam to Tehama Bridge " = "bnd",
  "Tehama Bridge To Woodson Bridge" = "bnd",
  "Woodson Bridge to Hamilton City Bridge" = "bnd",
  "Hamilton City Bridge to Ord Ferry Bridge" = "bnd",
  "Ord Ferry Bridge To Princeton Ferry" ="bnd"
)})


save(redd_cdec_lookup, file = "data/testing_data_object")
load("data/testing_data_object")

station_code_to_name_temps <- c(
  "kwk" = "Keswick",
  "ccr" = "Clear Creek",
  "bsf" = "Balls Ferry", 
  "jlf" = "Jellys Ferry",
  "bnd" = "Bend Bridge", 
  "sha" = "Shasta"
)

station_code_to_name_flows <- c(
  "kwk" = "Keswick Outflow",
  "ccr" = "Clear Creek",
  "bsf" = "Balls Ferry", 
  "jlf" = "Jellys Ferry",
  "bnd" = "Bend Bridge", 
  "sha" = "Shasta Inflow", 
  "wlk" = "Wilkins Slough"
)
