welcome_UI <- function(id) {
  ns <- NS(id)
  
  htmlTemplate("templates/welcome.html", 
               select_year_dropdown = selectInput(ns("welcome_summary_year_select"), 
                                                  label=NULL, choices = 2010:2018, 
                                                  selected = 2018), 
               summary_water_year_type = textOutput(ns("summary_water_year_type_value")),
               summary_total_winter_redds = textOutput(ns("summary_total_winter_redds_value")),
               summary_eos = textOutput(ns("summary_eos_value")),
               summary_out_of_compliance = textOutput(ns("summary_out_of_compliance_value")) 
               # action_link_to_about_page = actionLink(ns("goto_about_page"), label="Learn more about SHOWR")
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
      pull(counts) %>% sum(na.rm = TRUE)
  })
  
  eos_summary <- reactive({
    shasta_storage_data %>% 
      filter(parameter_id == 15, year(datetime) == input$welcome_summary_year_select, 
             month(datetime) == 9, day(datetime) == 30) %>% 
      pull(parameter_value)/1000 
  })
  
  days_out_of_compliance <- reactive({
    temp_compliance_points_daily_mean %>%
      filter(
             datetime >= as_date(paste0(input$welcome_summary_year_select, "-05-15")),
             datetime <= as_date(paste0(input$welcome_summary_year_select, "-10-31")), 
             location_id == "bsf", parameter_value > 56) %>% 
      nrow()
  })
  
  # metrics output
  output$summary_water_year_type_value <- renderText({
    water_year_classification_summary()
  })
  
  output$summary_out_of_compliance_value <- renderText({
    days_out_of_compliance()
  })
  
  output$summary_total_winter_redds_value <- renderText({
    total_redds_summary()
  })
  
  output$summary_eos_value <- renderText({
   paste(round(eos_summary(), digits = 0), "taf")
  })
  
  observeEvent(input$goto_about_page, {
    updateTabsetPanel(session = session, inputId = "showrapp", selected = "about_tab")
  })
}