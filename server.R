
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  g_date <- callModule(global_date, 'app')
  
  callModule(home_server, 'app', g_date)
  callModule(temp_page_server, 'app', g_date)
  callModule(winter_run_server, 'app', g_date)
  callModule(flow_server, 'app', g_date)
  callModule(about_server, 'app')
})
