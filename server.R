shinyServer(function(input, output, session) {
  g_date <- callModule(global_date, 'app')
  
  callModule(welcome_server, 'app')
  callModule(dashboard_server, 'app', g_date, x=session)
  callModule(temp_page_server, 'app', g_date)
  callModule(winter_run_server, 'app', g_date)
  callModule(flow_server, 'app', g_date)
  callModule(about_server, 'app')
  callModule(shallow_redds_server, 'app')
})
