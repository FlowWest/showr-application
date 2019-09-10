shallow_redds_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(width = 6,
           tags$h2("Shallow Redds Monitoring"),
           tags$hr(),
           leafletOutput(ns("shallow_redds_map"))), 
    column(width = 6, 
           tags$h4("Shallow Redd at river mile 256"),
           tags$p("Redd is expected to emerge on September 26 2019"),
           plotlyOutput(ns("shallow_flow_plot")) 
           )
  )
}

shallow_redds_server <- function(input, output, session) {
  
  output$shallow_redds_map <- renderLeaflet({
    shallow_color <- leaflet::colorNumeric(
      c("#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"), 
      domain = shallow_redds_danger$total)
    
    shallow_redds_danger %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(popup = ~paste0("<b>Shallow Redd</b><br>", 
                                       "Total in danger: ", total, "<br>", 
                                       "Dewater Threshold: 5,000 cfs"), 
                       color=~shallow_color(total), 
                       fillColor = ~shallow_color(total), 
                       fillOpacity = .7)
    
    
  })
  
  selected_redd_in_danger <- reactive({
    if (is.null(input$shallow_redds_map_marker_click)) {
      NULL
    } else {
      c(input$shallow_redds_map_marker_click$lat, 
        input$shallow_redds_map_marker_click$lng)
    }
  })
  
  
  last_emergence_data <- reactive({
    
    if (is.null(selected_redd_in_danger())) return(NULL)
    
    shallow_redds_danger %>% 
      filter(lat == selected_redd_in_danger()[1], 
             lon == selected_redd_in_danger()[2]) %>% 
      pull(last_emergence)
  })
  
  average_flow <- reactive({
    flow_data_daily_mean %>% 
      filter(location_id %in% c("kwk", "wlk")) %>% 
      mutate(datetime = `year<-`(datetime, 2019)) %>% 
      group_by(location_id, datetime) %>% 
      summarise(
        parameter_value = mean(parameter_value, na.rm = TRUE) 
      ) %>% 
      filter(datetime > today(), 
             datetime <= last_emergence_data()) %>% ungroup()
  })
  
  output$shallow_flow_plot <- renderPlotly({
    
    validate(
      need(!is.null(last_emergence_data()), "Select a shallow redd from the map to view details")
    )
    
    p <- flow_data_daily_mean %>% 
      filter(year(datetime) == 2019, 
             location_id %in% c("kwk", "wlk")) %>% 
      bind_rows(average_flow()) %>% 
      plot_ly(x=~datetime, y=~parameter_value, color=~location_id) %>% 
      add_lines() 
    
    
    if (is.null(last_emergence_data())) return(p)
    
    p %>% 
      add_segments(x = as_date("2019-01-01"), xend=last_emergence_data(), 
                   y = 5000, yend = 5000, name="Dewater Threshold", 
                   inherit = FALSE, 
                   line = list(color= "red", dash = "dash")) %>% 
      add_segments(x = last_emergence_data(), xend=last_emergence_data(), 
                   y = 0, yend = 7000, name = "Expected Emergence", 
                   inherit = FALSE)
    
    
  })
  
}