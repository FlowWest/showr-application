welcome_UI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate("templates/welcome.html", 
               select_year_dropdown = selectInput(ns("welcome_summary_year_select"), 
                                                  label=NULL, choices = 2010:2017, 
                                                  selected = 2017), 
               summary_water_year_type = textOutput(ns("summary_water_year_type_value")),
               summary_total_winter_redds = textOutput(ns("summary_total_winter_redds_value")),
               summary_eos = textOutput(ns("summary_eos_value"))
  )
}

welcome_server <- function(input, output, session) {
  ns <- session$ns
  
  # metrics 
  water_year_classification_summary <- reactive({
    get_year_classification(input$welcome_summary_year_select)
  })
  
  total_redds_summary <- reactive({
    redd_data %>% 
      filter(year(date) == as.numeric(input$welcome_summary_year_select)) %>% 
      pull(counts) %>% sum()
  })
  
  eos_summary <- reactive({
    shasta_storage_data %>% 
      filter(parameter_id == 15, year(datetime) == input$welcome_summary_year_select, 
             month(datetime) == 9, day(datetime) == 30) %>% 
      pull(parameter_value)/1000 
  })
  
  # metrics output
  output$summary_water_year_type_value <- renderText({
    water_year_classification_summary()
  })
  
  output$summary_total_winter_redds_value <- renderText({
    total_redds_summary()
  })
  
  output$summary_eos_value <- renderText({
   paste(round(eos_summary(), digits = 0), "taf")
  })
  
  observeEvent(input$dog_link, {
    updateNavbarPage(session, 
                     inputId = "main_nav", 
                     selected = "About")
  })
}