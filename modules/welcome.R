welcome_UI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate("templates/welcome.html")
}

welcome_server <- function(input, output, session) {
  
}