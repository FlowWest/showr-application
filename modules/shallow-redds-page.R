shallow_redds_ui <- function(id) {
  ns <- NS(id)
  
  # layout distributes 1/2 the screen to the map (100% height) and 1/2
  # the screen to the metrics and plot (50% height each)
  tagList(
    column(width = 6,
           tags$h3("Shallow Redds Monitoring"),
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

  
  # Shallow redds map
  output$shallow_redds_map <- renderLeaflet({
    shallow_color <- leaflet::colorNumeric(
      palette = "OrRd", 
      domain = shallow_redds_danger$total)
    
    shallow_redds_danger %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(popup = ~paste0("<b>Shallow Redds at RM: ", river_mile, "</b><br>", 
                                       "Total in danger: ", total, "<br>", 
                                       "Dewater Threshold: 5,000 cfs <br>", 
                                       "Estimated Emergence: ", days_to_emergece, 
                                       " (", format(last_emergence, "%b %d)")), 
                       color= "#666666", 
                       fillColor = ~shallow_color(total), 
                       layerId = ~river_mile,
                       fillOpacity = .7) %>% 
      addLegend(pal=shallow_color, values = ~total, 
                title = "Total Redds")
    
    
  })
  
  # lat longs for selected redd in danger
  selected_redd_in_danger <- reactive({
    if (is.null(input$shallow_redds_map_marker_click)) {
      NULL
    } else {
      c(input$shallow_redds_map_marker_click$lat, 
        input$shallow_redds_map_marker_click$lng)
    }
  })
  
  # map data for the selected redd in danger 
  # this is used for adding color the selected marker
  selected_marker <- reactive({
    shallow_redds_danger %>% 
      filter(river_mile == input$shallow_redds_map_marker_click$id)
  })
  
  # the last emergence within a river mile for the selected redd in danger
  last_emergence_date <- reactive({
    
    if (is.null(selected_redd_in_danger())) return(NULL)
    
    shallow_redds_danger %>% 
      filter(lat == selected_redd_in_danger()[1], 
             lon == selected_redd_in_danger()[2]) %>% 
      pull(last_emergence)
  })
  
  # calculate the historical flows up to the last emergence data
  # this is used for the timer series plot
  average_flow <- reactive({
    flow_data_daily_mean %>% 
      filter(location_id %in% c("kwk", "wlk")) %>% 
      mutate(datetime = `year<-`(datetime, 2019)) %>% 
      group_by(location_id, datetime) %>% 
      summarise(
        parameter_value = mean(parameter_value, na.rm = TRUE) 
      ) %>% 
      filter(datetime > today(), 
             datetime <= last_emergence_date()) %>% ungroup()
  })
  
  output$shallow_flow_plot <- renderPlotly({
    
    validate(
      need(!is.null(last_emergence_date()), "Select a shallow redd from the map to view details")
    )
    
    p <- flow_data_daily_mean %>% 
      filter(year(datetime) == 2019, 
             location_id %in% c("kwk", "wlk")) %>% 
      bind_rows(average_flow()) %>% 
      plot_ly(x=~datetime, y=~parameter_value, color=~location_id) %>% 
      add_lines() 
    
    
    if (is.null(last_emergence_date())) return(p)
    
    p %>% 
      add_segments(x = as_date("2019-01-01"), xend=last_emergence_date(), 
                   y = 5000, yend = 5000, name="Dewater Threshold", 
                   inherit = FALSE, 
                   line = list(color= "red", dash = "dash")) %>% 
      add_segments(x = last_emergence_date(), xend=last_emergence_date(), 
                   y = 0, yend = 7000, name = "Expected Emergence", 
                   inherit = FALSE)
    
    
  })
  
  observeEvent(input$shallow_redds_map_marker_click$id, {
    leafletProxy("shallow_redds_map") %>% 
      clearGroup("selected_shallow_redd") %>% 
      addCircleMarkers(data=selected_marker(), color = "#57a0ff",
                       weight = 5,
                       opacity = 1,
                       group = "selected_shallow_redd")
  })
  
  
  
  
  
  
}