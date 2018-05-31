function(request) {tagList(
  useShinyjs(),
  useToastr(),
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
             tabPanel("Winter Run", value = "winter_run_tab", winter_run_UI('app')), 
             tabPanel("About", value = "about_tab", about_UI('app'))
  ))
  
}