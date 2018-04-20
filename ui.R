tagList(useShinyjs(),
        navbarPage(title=div(class="logo", tags$img(class="logo-img",
                              src="chinook (1).svg", width="140px"), 
                     "SHO-WR App"), 
           collapsible = TRUE, 
           inverse = FALSE,
           theme = shinythemes::shinytheme("cosmo"),
           windowTitle = "SHO-WR App", 
           header = includeCSS("styles.css"),
           
           
           # refacor the names of UI functions
           tabPanel("Home", value="about_page", welcome_UI('app')),
           tabPanel("Dashboard", value="home_page", homeUI('app')),
           tabPanel("Temperature", value="temperature_page", temp_pageUI('app')),
           tabPanel("Flow", value="flow_page", flow_UI('app')), 
           tabPanel("Winter Run Chinook", value="chinook_page", winter_run_UI('app'))
))











