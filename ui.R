tagList(useShinyjs(),
        navbarPage(title=div(class="logo", tags$img(class="logo-img",
                              src="chinook (1).svg", width="140px"), 
                     "SHOWR"), 
           collapsible = TRUE, 
           inverse = FALSE,
           theme = shinythemes::shinytheme("cosmo"),
           windowTitle = "Shasta Operation Winter Run", 
           header = includeCSS("styles.css"),
           id = "main_nav",

           # refacor the names of UI functions
           tabPanel("Home", welcome_UI('app')),
           tabPanel("Dashboard", dashboardUI('app')),
           tabPanel("Temperature", temp_pageUI('app')),
           tabPanel("Flow", flow_UI('app')),
           tabPanel("Winter Run Chinook", winter_run_UI('app')), 
           tabPanel("About", about_UI('app'))
))











