function(request) {
  tagList(
    useShinyjs(),
    useToastr(),
    tags$style(type = "text/css", "#app-shallow_redds_map {height: calc(100vh - 175px) !important;}"),
    navbarPage(title=div(class="logo", tags$img(class="logo-img",
                                                src="chinook (1).svg", width="140px"), 
                         "SHOWR"), 
               collapsible = TRUE, 
               inverse = FALSE,
               theme = shinythemes::shinytheme("cosmo"),
               windowTitle = "Shasta Operation Winter Run", 
               header = includeCSS("styles.css"),
               id = "showrapp",
               
               # refacor the names of UI functions
               tabPanel("Home", value = "welcome_tab", welcome_UI('app')),
               tabPanel("Dashboard", value = "dash_tab", dashboardUI('app')),
               tabPanel("Temperature", value = "temp_tab", temp_pageUI('app')),
               tabPanel("Flow", value = "flow_tab", flow_UI('app')),
               navbarMenu(
                 title = "Winter Run",
                 tabPanel("Redd Monitoring", value = "winter_run_tab", winter_run_UI('app')), 
                 tabPanel("Shallow Monitoring", value = "shallow_tab", shallow_redds_ui('app'))
               ),
               tabPanel("About", value = "about_tab", about_UI('app'))
    ))
  
}
