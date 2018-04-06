winter_run_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 1, 
             radioButtons(inputId = ns("fish_view"),
                          label = "Select Dataset",
                          choices = c("Aerial Redd",
                                      "Carcass"))), 
      column(width = 2, 
             uiOutput(ns("fish_year_filter"))), 
      column(width = 3, 
             uiOutput(ns("fish_location_filter")))
    ), 
    fluidRow(
      column(width = 12,
             class = "col-md-3",
             leafletOutput(ns("chinook_map"), height = "500px")), 
      column(width = 12, 
             class = "col-md-7",
             tabsetPanel(
               tabPanel(title = "Plot", 
                        plotlyOutput(ns("chinook_plot"))), 
               tabPanel(title = "Tabular", 
                        dataTableOutput(ns("chinook_tabular")))
             ), 
             tags$h5("Historical Counts at ", textOutput(ns('location_selected_print'))),
             plotlyOutput(ns("year_counts"), height=200)), 
      column(width = 12, 
             class = "col-md-2", 
             tags$h3("Year totals"), 
             tags$table(class="table",tags$tbody(
               tags$tr(tags$td("In Reach"), tags$td(textOutput(ns("redd_year_counts")))), 
               tags$tr(tags$td("In System"), tags$td(textOutput(ns("year_total_in_system"))))
             )))
    ), 
    fluidRow(
      column(width = 12, 
             tags$h6("Note: the final aerial redd survey was conducted on 08/16/2017"),
             tags$h6("Data source: CDFW provided through calfish.org"), 
             tags$h6("Update schedule: data is updated by CDFW on a weekly basis"))
    )
  )
} 

# TODO pick either switch or ifelse from now on
winter_run_server <- function(input, output, session, g_date) {
  ns <- session$ns
  
  observeEvent(g_date(), {
    updateSelectInput(session = session, 
                      inputId = "fish_year", 
                      selected = year(g_date()))
  })
  
  output$fish_year_filter <- renderUI({
    
    if (input$fish_view == "Aerial Redd") {
      selectInput(ns("fish_year"), "Select a Year",
                  choices = 1970:2017, selected = 2017, width = '300px')
    } else { 
      selectInput(ns("fish_year"), "Select a Year",
                  choices = 2012:2017, selected = 2017, width = '300px')
    }
  }) 
  
  output$fish_location_filter <- renderUI({
    switch(input$fish_view, 
           "Aerial Redd" = {
             selectInput(ns("fish_location"), "Select a Location",
                         choices = redd_locations,
                         width = '900px')
           },  
           "Carcass" = {
             selectInput(ns("fish_location"), "Select a Location",
                         choices = carcass_location, width = '900px')
           }) 
  })
  
  total_redd_counted_in_year <- reactive({
    req(input$fish_year)
    redd_data %>% 
      filter(year(date) == input$fish_year) %>% 
      select(counts) %>% 
      sum()
  })
  
  total_carcass_counted_in_year <- reactive({
    req(input$fish_year)
    carcass_data %>% 
      filter(year(date) == input$fish_year) %>% 
      select(total_count) %>% 
      sum()
  })
  
  selected_redd_data <- reactive({
    req(input$fish_location, input$fish_year)
    redd_data %>% 
      filter(location == input$fish_location, year(date) == input$fish_year, 
             counts > 0) # maybe move counts > 0 to sql query?
  })
  
  selected_temp_data_with_temps <- reactive({
    d1 <- selected_redd_data() %>% 
      mutate(cdec_loc = redd_cdec_lookup[location])
    
    left_join(d1, ungroup(temp_daily))
  })
  
  redd_year_counts <- reactive({
    req(input$fish_location, input$fish_year)
    redd_data %>% 
      filter(location == input$fish_location) %>% 
      group_by(yr = year(date)) %>% 
      summarise(
        total = sum(counts)
      ) %>% ungroup()
  })
  
  carcass_year_counts <- reactive({
    carcass_data %>% 
      filter(section_name == input$fish_location) %>% 
      group_by(yr = year(date)) %>% 
      summarise(
        total = sum(total_count)
      ) %>% 
      ungroup()
  })
  
  selected_redd_year_total <- reactive({
    if (nrow(selected_redd_data()) == 0) return(0)
    
    selected_redd_data() %>% 
      dplyr::select(counts) %>% 
      sum()
  })
  
  selected_carcass_data <- reactive({
    req(input$fish_location, input$fish_year)
    carcass_data %>% 
      filter(section_name == input$fish_location, year(date) == input$fish_year, 
             total_count > 0)
  })
  
  selected_carcass_year_total <- reactive({
    req(input$fish_location, input$fish_year)
    if (nrow(selected_carcass_data()) == 0) return(0)
    selected_carcass_data() %>% 
      select(total_count) %>% 
      sum()
  })
  
  # Winter Run Chinook Plots ---------------------------------------------------
  output$chinook_plot <- renderPlotly({
    switch (input$fish_view,
      "Aerial Redd" = {
        validate(need(nrow(selected_redd_data()) > 0, 
                      paste0("0 redds counted in ", 
                             input$fish_year,
                             " at ", input$fish_location)))
        selected_redd_data() %>% 
          plot_ly(x=~date, y=~counts, type = 'bar', 
                  text = ~paste0(
                    date, "<br>", 
                    "Total Count: ", counts
                  ), 
                  hoverinfo = "text") %>% 
          layout(xaxis=list(title=""))
      },
      "Carcass" = {
        validate(need(nrow(selected_carcass_data()) > 0, 
                      paste0("Combination of year ", 
                             input$fish_year,
                             " and location ", input$fish_location, 
                             " returned no Carcass data.")))
        
        selected_carcass_data() %>% 
          plot_ly(x=~date, y=~total_count, type='bar', 
                  text = ~paste0(
                    date, "<br>", 
                    "Total Count: ", total_count
                  ), 
                  hoverinfo = "text") %>%
          layout(xaxis = list(title=""), 
                 yaxis = list(title="counts"))
        
      }
    )
  })
  
  output$chinook_tabular <- renderDataTable({
    switch (input$fish_view,
      "Aerial Redd" = selected_redd_data(), 
      "Carcass" = selected_carcass_data()
    )
  })
  
  # Chinook Map ----------------------------------------------------------------
  
  redd_reach_selected <- reactive({
    subset(redd_reach, Reach == input$fish_location)
  })
  
  carcass_reach_selected <- reactive({
    subset(carcass_reach, Reach == input$fish_location)
  })
  
  output$chinook_map <- renderLeaflet({
    switch (input$fish_view,
      "Aerial Redd" = {
        leaflet(redd_reach) %>% 
          addProviderTiles(providers$Esri.WorldTopoMap) %>% 
          addPolylines(color = "grey", weight = 7, opacity = 1, label = ~Reach, 
                       group = "aerial_redd",
                       layerId=~Reach) %>% 
          addPolylines(data=redd_reach_selected(), 
                       color = "red") %>% 
          setView(lat = 40.4720495, lng = -122.2945346, zoom = 9)
      }, 
      "Carcass" = {
        leaflet(carcass_reach) %>% 
          addProviderTiles(providers$Esri.WorldTopoMap) %>% 
          addPolylines(color = "grey", weight = 7, opacity = 1, label = ~Reach, 
                       group = "carcass",
                       layerId=~Reach) %>% 
          addPolylines(data=carcass_reach_selected(), color = "red") %>% 
          setView(lat = 40.4720495, lng = -122.2945346, zoom = 9)
      }
    )
  })
  
  
  output$location_selected_print <- renderText({input$fish_location})
  
  
  output$year_counts <- renderPlotly({
    switch(input$fish_view, 
      "Aerial Redd" = {
        redd_year_counts() %>% 
          plot_ly(x=~yr, y=~total, type='scatter', mode='lines', fill="tozeroy") %>% 
          add_markers(data=filter(redd_year_counts(), yr==input$fish_year), 
                      x=~yr, y=~total, inherit = FALSE, 
                      marker = list(size=9)) %>% 
          layout(xaxis=list(title=""), yaxis=list(title="Year Total"), 
                 showlegend=FALSE)
      }, 
      "Carcass" = {
        carcass_year_counts() %>% 
          plot_ly(x=~yr, y=~total, type='scatter', mode='lines', fill="tozeroy") %>% 
          add_markers(data=filter(carcass_year_counts(), yr==input$fish_year), 
                      x=~yr, y=~total, inherit = FALSE, 
                      marker = list(size=9)) %>% 
          layout(xaxis=list(title=""), yaxis=list(title="Year Total"), 
                 showlegend=FALSE)
      }
    )
  })
  
  
  # Year to date total ---------------------------------------------------------
  output$redd_year_counts <- renderText({
    switch (input$fish_view,
      "Aerial Redd" = selected_redd_year_total(), 
      "Carcass" = selected_carcass_year_total()
    )
  })
  output$year_total_in_system <- renderText({
    switch (input$fish_view,
      "Aerial Redd" = total_redd_counted_in_year(), 
      "Carcass" = total_carcass_counted_in_year()
    )
  })
}