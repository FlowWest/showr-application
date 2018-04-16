# This script populates the data folder with general R object.
# namely just vectors that would otherwise be created and used from the 
# global.R file. The purpose is not so much for speed (but it does help a little bit), 
# but to keep the global.R file clean, and be able to document each of these
# objects clearly in this file.

# This vector is used to lookup the correct cdec station based on the 
# redd reach
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
)

# This vector is used to lookup the full name of a station for the temp plots.
# used for plotting in order to show full names in the legend and in the hovers
station_code_to_name_temps <- c(
  "kwk" = "Keswick",
  "ccr" = "Clear Creek",
  "bsf" = "Balls Ferry", 
  "jlf" = "Jellys Ferry",
  "bnd" = "Bend Bridge", 
  "sha" = "Shasta"
)

# This vector is used to lookup the full name of a station for the flow plots.
# used for plotting in order to show full names in the legend and in the hovers
station_code_to_name_flows <- c(
  "kwk" = "Keswick Outflow",
  "ccr" = "Clear Creek",
  "bsf" = "Balls Ferry", 
  "jlf" = "Jellys Ferry",
  "bnd" = "Bend Bridge", 
  "sha" = "Shasta Inflow", 
  "wlk" = "Wilkins Slough"
)

# named vector used to zoom in based on the redd selection
# for use in the leaflet map within the chinook page.
redd_reach_center_coords <- list(
  "Keswick to ACID Dam" = c(40.597215, -122.439403), 
  "ACID Dam to Highway 44 Bridge" = c(40.592037, -122.373396), 
  "Highway 44 Bridge to Airport Road Bridge" = c(40.515936, -122.357771), 
  "Airport Road Bridge to Balls Ferry Bridge" = c(40.462697, -122.251489),
  "Balls Ferry Bridge to Battle Creek" = c(40.380764, -122.199044),
  "Battle Creek to Jellys Ferry Bridge" = c(40.331602, -122.210969),
  "Jellys Ferry Bridge to Bend Bridge" = c(40.317591, -122.173661),
  "Bend Bridge to Red Bluff Diversion Dam" = c(40.203255, -122.218306),
  "Red Bluff Diversion Dam to Tehama Bridge" = c(),
  "Tehama Bridge to Woodson Bridge" = c(),
  "Woodson Bridge to Hamilton City Bridge" = c(),
  "Hamilton City Bridge to Old Ferry Bridge" = c()
)

# redd locations, this is used for the selectInput
redd_locations <- names(redd_cdec_lookup)


# run this to make the .Rdata object file
save(
  redd_cdec_lookup, 
  station_code_to_name_flows, 
  station_code_to_name_temps, 
  redd_reach_center_coords,
  redd_locations,
  file = "data/general-objects.RData"
)






