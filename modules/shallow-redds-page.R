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
  
  shallow_redds_flow <- reactive({
    flow_data_daily_mean %>% 
      filter(year(datetime) == 2019, month(datetime) >= 5, 
             location_id == "kwk") %>% 
      arrange(datetime)
  })
  
  # calculate the historical flows up to the last emergence data
  # this is used for the timer series plot
  projected_flows <- reactive({
    flow_data_daily_mean %>% 
      filter(location_id == "kwk") %>% 
      mutate(datetime = `year<-`(datetime, 2019)) %>% 
      group_by(location_id, datetime) %>% 
      summarise(
        min_value = min(parameter_value, na.rm = TRUE), 
        max_value = max(parameter_value, na.rm = TRUE),
        avg_value = mean(parameter_value, na.rm = TRUE) 
      ) %>% 
      filter(datetime > max(shallow_redds_flow()$datetime), 
             datetime <= last_emergence_date()) %>% ungroup()
  })
  
  
  output$shallow_flow_plot <- renderPlotly({
    validate(need(!is.null(input$shallow_redds_map_marker_click$id), 
                  "Select a river mile from the map"))
    shallow_redds_flow() %>% 
      plot_ly(x = ~datetime, y = ~parameter_value, type='scatter', mode='lines', 
              name = "Keswick flow") %>%
      add_segments(x = as_date("2019-05-01"), xend=last_emergence_date(), 
                   y = 5000, yend = 5000, name="Dewater Threshold", 
                   inherit = FALSE, 
                   line = list(color= "red", dash = "dash")) %>% 
      add_ribbons(data=projected_flows(), 
                  x = ~datetime, ymin = ~min_value, ymax = ~max_value, 
                  fillcolor = 'rgba(7, 164, 181, 0.2)', inherit = FALSE, 
                  line = list(color = 'rgba(7, 164, 181, 0.5)'),
                  name = "Historical Range 2010-2018") %>% 
      add_trace(data=projected_flows(),
                x = ~datetime, y = ~avg_value,
                type='scatter', mode='lines', name="Historical Average (2010-2018)", 
                line = list(color = '#666666')) %>% 
      layout(xaxis = list(title = ""), 
             yaxis = list(title = "Flow (cfs)", rangemode="tozero"), 
             legend = list(orientation = 'h'))
    
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