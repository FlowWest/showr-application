welcome_UI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate("templates/welcome.html", 
               select_year_dropdown = selectInput(ns("welcome_summary_year_select"), 
                                                  label=NULL, choices = 2010:2017)
  )
}

welcome_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$dog_link, {
    updateNavbarPage(session, 
                     inputId = "main_nav", 
                     selected = "About")
  })
}