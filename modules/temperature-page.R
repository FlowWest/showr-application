temp_pageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # context sidebar
      column(width = 12, class = "col-md-3", 
             tags$div(class = "temp-sidebar", 
                      tags$h2("Temperature Compliance"),
                      tags$hr(),
                      tags$p(
                        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ac scelerisque quam. Fusce eget risus eros. Cras elementum nulla velit, in lacinia mauris euismod ut. Praesent ut semper nunc. Cras porttitor elit sem, id molestie purus fringilla nec. Aliquam vehicula lacinia aliquam. Curabitur et leo elit. Sed egestas massa sit amet turpis faucibus blandit. Curabitur vel efficitur tellus, accumsan dapibus diam. Vivamus tincidunt leo vel placerat facilisis. Duis id augue ac dui posuere hendrerit."
                      ),
                      tags$h3("Download Data in View"),
                      downloadButton(ns("download_temp_data")),
                      tags$h5("Shaded area in the plot above indicates temperature target period"),
                               tags$h5("Data Source: hourly data from CDEC, updated to the application daily"),
                               tags$h5("Temperature Statistics Definitions:"),
                               tags$h5("-Daily Mean: Daily mean for temperatures observed in a given day"),
                               tags$h5("-7DADM: 7 day average of daily maxes"),
                               tags$h5("-Daily Max: the max observed temperature for a given day") 
                      )),
      # main interface
      column(width = 12, class = "col-md-9",
             fluidRow(
               # temp controls
               column(width = 12, class = "col-md-3",
                      style="display: inline-flex;",
                      dateRangeInput(inputId = ns("temp_daterange"),
                                     label = "Select Date Range",
                                     min = "1999-01-01",
                                     start = paste0(year(today()), "-01-01"),
                                     end = today()-1)),
               column(width = 12, class = "col-md-2",
                      selectInput(inputId = ns("temp_add_year"),
                                  label = "Add Year",
                                  choices = c("None", 2010:2017),
                                  width = "75px")
               ),
               column(width = 12, class = "col-md-3",
                      selectInput(inputId = ns("temp_gage_location"),
                                  label = "Select Gage(s)",
                                  choices = c("Keswick" = "kwk",
                                              "Sac. River Hwy 44" = "sac",
                                              "Clear Creek" = "ccr",
                                              "Balls Ferry" = "bsf",
                                              "Jellys Ferry" = "jlf",
                                              "Bend Bridge" = "bnd"),
                                  multiple = TRUE, selected = c("kwk","ccr","bsf"))),
               column(width = 12, class = "col-md-3",
                      radioButtons(inputId = ns("temp_summary_choice"),
                                   label = "Select Temperature Statistic",
                                   choices = c("Daily Mean", "7DADM", "Daily Max")))
             ), 
             fluidRow(
               # temp plot
               column(width = 12, class = "col-md-9", 
                      plotlyOutput(ns("temperature_ts_plot"), height = "500px")),
               # temp schematic
               column(width = 12, class = "col-md-3", 
                      tags$img(class="gage_map", src="temp_sites.png"), 
                      tags$h6("Gray locations are below compliance point"))
             ))
    )

  )
} 

temp_page_server <- function(input, output, session, g_date) {
  
  ns <- session$ns
  
  # upadte the daterange input depending on the home page date
  observeEvent(g_date(), {
    updateDateRangeInput(session = session, 
                         inputId = "temp_daterange", 
                         start = paste0(year(g_date()), "-01-01"), 
                         end = g_date() - 1)
  })
  
  # compute temperature statistics here
  selected_temp_summary <- function(summary_f) {
    reactive({
      temp_data %>% 
        filter(location_id %in% input$temp_gage_location,
               datetime >= input$temp_daterange[1], 
               datetime <= input$temp_daterange[2]) %>% 
        mutate(date = as_date(datetime)) %>% 
        group_by(location_id, date) %>% 
        summarise(
          summary_value = round(summary_f(parameter_value, na.rm = TRUE), 2)
        ) %>% 
        ungroup()
    })}
  
  # reactive temperature summaries 
  selected_daily_means <- selected_temp_summary(mean)
  selected_daily_max <- selected_temp_summary(max)
  selected_7dadm <- reactive({
    selected_daily_max() %>% 
      group_by(location_id) %>% 
      mutate(seven_dadm = round(rollmean(summary_value, k = 7, fill = NA), 2))
  })
  
  
  # TODO: this currently works for when we span one year, when we start to span 
  # multiple years, it only creates the first compliance rectangle
  make_compliance_rect <- reactive({
    
    if (year(input$temp_daterange[1]) == year(input$temp_daterange[2])) {
      # depending on the daterange we need to draw a certain number 
      # of start and stop dates for the rectangle start and stop
      
      current_compliance_range <- c(
        as_date(paste0(year(input$temp_daterange[1]), "-05-15")), 
        as_date(paste0(year(input$temp_daterange[1]), "-10-31"))
      )
      
      if (input$temp_daterange[2] < current_compliance_range[1]) {
        return(NULL)
      }
      
      return(list(
        list(
          type = "line",
          line = list(color = "grey", dash="solid", wiidth=2),
          opacity = 0.3,
          x0 = ifelse(input$temp_daterange[1] > current_compliance_range[1], 
                      as.character(input$temp_daterange[1]), 
                      as.character(current_compliance_range[1])),
          x1 = ifelse(input$temp_daterange[2] < current_compliance_range[2], 
                      as.character(input$temp_daterange[2]), 
                      as.character(current_compliance_range[2])), 
          xref="x", y0 = 56, y1 = 56, yref = "y"
        ),
        list(
          type = "rect", fillcolor = "#cecece", 
          line = list(color = "#cecece"), opacity = 0.3, 
          x0 = ifelse(input$temp_daterange[1] > current_compliance_range[1], 
                      as.character(input$temp_daterange[1]), 
                      as.character(current_compliance_range[1])),
          x1 = ifelse(input$temp_daterange[2] < current_compliance_range[2], 
                      as.character(input$temp_daterange[2]), 
                      as.character(current_compliance_range[2])), 
          xref = "x", 
          y0 = 0, y1 = 56, yref = "y", 
          layer = "below")
      ))
    } else {
      return(NULL)
    }
    
    })
  
  

  span_multiple_years <- reactive({
    year(input$temp_daterange[1]) != year(input$temp_daterange[2])
  })
  
  observe({
    if (span_multiple_years()) {
      updateSelectInput(session, "temp_add_year", selected = "None")
      shinyjs::disable("temp_add_year")
    } else {
      shinyjs::enable("temp_add_year")
    }
  })
  
  selected_add_year_data_mean <- reactive({
    
    if (input$temp_add_year == "None") return(NULL)
    
    this_daterange <- input$temp_daterange
    year(this_daterange[1]) <- as.numeric(input$temp_add_year)
    year(this_daterange[2]) <- as.numeric(input$temp_add_year)
    
    this_temp_data <- temp_data %>% 
      filter(location_id %in% input$temp_gage_location,
             datetime >= this_daterange[1], 
             datetime <= this_daterange[2]) %>% 
      mutate(date = as_date(datetime)) %>% 
      group_by(location_id, date) %>% 
      summarise(
        summary_value = round(mean(parameter_value, na.rm = TRUE), 2)
      ) %>% 
      ungroup() 
    
    year(this_temp_data$date) <- year(input$temp_daterange[1])
    return(this_temp_data)
  })
  
  selected_add_year_data_max <- reactive({
    
    if (input$temp_add_year == "None") return(NULL)
    
    this_daterange <- input$temp_daterange
    year(this_daterange[1]) <- as.numeric(input$temp_add_year)
    year(this_daterange[2]) <- as.numeric(input$temp_add_year)
    
    this_temp_data <- temp_data %>% 
      filter(location_id %in% input$temp_gage_location,
             datetime >= this_daterange[1], 
             datetime <= this_daterange[2]) %>% 
      mutate(date = as_date(datetime)) %>% 
      group_by(location_id, date) %>% 
      summarise(
        summary_value = round(max(parameter_value, na.rm = TRUE), 2)
      ) %>% 
      ungroup() 
    
    year(this_temp_data$date) <- year(input$temp_daterange[1])
    return(this_temp_data)
  })
  
  selected_add_year_data_7dadm <- reactive({
    if (input$temp_add_year == "None") return(NULL)
    
    this_temp_data <- selected_add_year_data_max() %>% 
      group_by(location_id) %>% 
      mutate(seven_dadm = round(rollmean(summary_value, k = 7, fill = NA), 2))

    return(this_temp_data)
  })
  
  output$temperature_ts_plot <- renderPlotly({
    validate(
      need(input$flow_daterange[1] < input$flow_daterange[2], "Selected dates returned no data")
    )
    
    cat(unlist(make_compliance_rect()))
    
    switch (input$temp_summary_choice,
            "Daily Mean" = {
              validate(need(nrow(selected_daily_means()) > 0,
                            "Selected date range and or station combination returned no data"))
              
              base_plot <- selected_daily_means() %>% 
                plot_ly(x=~date, y=~summary_value, type='scatter', mode='lines', 
                        color = ~station_code_to_name_temps[location_id], 
                        colors = "Dark2",
                        line=list(width=3),
                        text=~paste0("<b>",date, "</b><br><b>", station_code_to_name_temps[location_id], 
                                     "</b><br><b>", summary_value, " °F</b>"), 
                        hoverinfo = "text") 
              
              if (input$temp_add_year != "None") {
                base_plot <- base_plot %>% 
                  add_trace(data = selected_add_year_data_mean(),
                            x=~date, y=~summary_value, type='scatter', mode='lines', 
                            name =~ paste0(input$temp_add_year),
                            text=~paste0("<b>",input$temp_add_year, "</b><br><b>", station_code_to_name_temps[location_id], 
                                         "</b><br><b>", summary_value, " °F</b>"), 
                            hoverinfo = "text",
                            line=list(dash="dot", width = 2, showlegend = FALSE))
              }
              
              base_plot %>% 
                layout(xaxis = list(title=""), 
                       yaxis = list(title = "Temperature (F)", range = c(40, 65)), 
                       shapes = make_compliance_rect(), 
                       legend = list(orientation = 'h')) %>%
                config(displayModeBar = FALSE)
              
            }, 
            "7DADM" = {
              validate(need(nrow(selected_7dadm()) > 0,
                            "Selected date range and or station combination returned no data"))
              base_plot <- selected_7dadm() %>% 
                plot_ly(x=~date, y=~seven_dadm, type='scatter', mode='lines', 
                        color = ~station_code_to_name_temps[location_id], 
                        colors = "Dark2",
                        line=list(width=3), 
                        text=~paste0("<b>",date, "</b><br><b>", station_code_to_name_temps[location_id], 
                                     "</b><br><b>", seven_dadm, " °F</b>"), 
                        hoverinfo = "text")
              if (input$temp_add_year != "None") {
                base_plot <- base_plot %>% 
                  add_trace(data = selected_add_year_data_7dadm(), 
                            x=~date, y=~seven_dadm, type='scatter', mode='lines', 
                            name =~ paste0(input$temp_add_year),
                            text=~paste0("<b>",input$temp_add_year, "</b><br><b>", station_code_to_name_temps[location_id], 
                                         "</b><br><b>", seven_dadm, " °F</b>"), 
                            hoverinfo = "text",
                            line=list(dash="dot", width = 2))
              }
              
              base_plot %>% 
                layout(xaxis = list(title=""), 
                       yaxis = list(title = "Temperature (F)", range = c(40, 65)), 
                       shapes = make_compliance_rect(), 
                       legend = list(orientation = 'h')) %>%
                config(displayModeBar = FALSE)
            },
            "Daily Max" = {
              validate(need(nrow(selected_daily_max()) > 0,
                            "Selected date range and or station combination returned no data"))
              base_plot <- selected_daily_max() %>% 
                plot_ly(x=~date, y=~summary_value, type='scatter', mode='lines', 
                        color = ~station_code_to_name_temps[location_id],
                        colors = "Dark2",
                        line=list(width=3), 
                        text=~paste0("<b>",date, "</b><br><b>", station_code_to_name_temps[location_id], 
                                     "</b><br><b>", summary_value, " °F</b>"), 
                        hoverinfo = "text")
              
              if (input$temp_add_year != "None") {
                base_plot <- base_plot %>% 
                  add_trace(data = selected_add_year_data_max(), 
                            x=~date, y=~summary_value, type='scatter', mode='lines', 
                            name =~ paste0(input$temp_add_year),
                            text=~paste0("<b>",input$temp_add_year, "</b><br><b>", station_code_to_name_temps[location_id], 
                                         "</b><br><b>", summary_value, " °F</b>"), 
                            hoverinfo = "text",
                            line=list(dash="dot", width = 2))
              }
              
              base_plot %>% 
                layout(xaxis = list(title=""), 
                       yaxis = list(title = "Temperature (F)", range = c(40, 65)), 
                       shapes = make_compliance_rect(), 
                       legend = list(orientation = 'h')) %>%
                config(displayModeBar = FALSE)
            }
    ) 
  })
  
  output$temperature_tabular <- renderDataTable({
    switch (input$temp_summary_choice,
            "Daily Mean" = {
              selected_daily_means()
            }, 
            "7DADM" = {
              selected_7dadm()
            }, 
            "Daily Max" = {
              selected_daily_max()
            } 
    )
  }, style = "bootstrap")
}