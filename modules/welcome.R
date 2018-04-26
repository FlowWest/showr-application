welcome_UI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate("templates/welcome.html", 
               link_to_about = tagList(tags$div(class = "home-about",
                                                actionLink(inputId = ns("dog_link"), "Find Out More"),
                                                tags$p("Click here to learn more about
                                 data sources and organizations involved in the project."))
                                       
               )
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