shinyServer(function(input, output, session) {
  g_date <- callModule(global_date, 'app')
  
  callModule(welcome_server, 'app')
  callModule(dashboard_server, 'app', g_date, x=session)
  callModule(temp_page_server, 'app', g_date)
  callModule(winter_run_server, 'app', g_date)
  callModule(flow_server, 'app', g_date)
  callModule(about_server, 'app')
  
  shinytoastr::toastr_info(
    title = "2018 Aerial Redd Survey is here!",
    message = "Go to the Dashboard or Winter Run page to view the latest aerial redds observations", 
    position = "top-right",
    timeOut = 3000, 
    closeButton = TRUE
  )
})
