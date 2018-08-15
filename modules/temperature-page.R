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
                        tags$a("Shasta Dam", href="https://www.usbr.gov/projects/index.php?id=241", target = "_blank"), 
                        "and its downstream re-regulating", 
                        tags$a("Keswick Dam", href="https://www.usbr.gov/projects/index.php?id=185", target = "_blank"),  
                        "are managed in part to optimize use of cold water for Winter Run Chinook salmon. Cold water management is guided by State Water Resource Control Board (SWRCB)",
                        tags$a("Water Rights Order (WR) 90-5", href="https://www.waterboards.ca.gov/waterrights/board_decisions/adopted_orders/orders/1990/wro90-05.pdf", target = "_blank"),
                        "and Reasonable and Prudent Alternatives (RPA) specified in the", 
                        tags$a("2009 Biological Opinion", href="http://www.westcoast.fisheries.noaa.gov/central_valley/water_operations/ocap.html", target = "_blank")
                        ,"on the long term operations of the Central Valley Project and State Water Project."), 
                      tags$br(),
                      tags$p("Sacramento River water temperatures downstream of Keswick dam are controlled by the temperature of flows released from Shasta Dam and ambient air temperature. Water temperature typically increases with distance downstream of Keswick Dam. Between May 15 and October 31, flow management from Shasta Dam attempts to prevent daily average temperatures from exceeding 56°F at compliance locations between Balls Ferry and Bend Bridge to maintain suitable conditions for Winter Run Chinook Salmon spawning, egg incubation, emergence, and rearing."),
                      tags$hr(),
                      tags$h4("Download Data in View"),
                      downloadButton(ns("download_temp_data"), class="subpage-buttons"),
                      tags$br(),
                      tags$br(),
                      bookmarkButton(label = "Share page", id = ns("temp_page_bookmark"), 
                                     class="subpage-buttons"),
                      tags$hr(),
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
                                  label = "Add Previous Year",
                                  choices = c("None", 2010:2017),
                                  width = "140px")
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
                                   choices = c("Daily Mean", "7DADM", "Daily Max")), 
                      shinyWidgets::materialSwitch(ns("show_air_temp"), 
                                                   label = "Show Redding Air Temp", 
                                                   right = TRUE))
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
  # observeEvent(g_date(), {
  #   updateDateRangeInput(session = session, 
  #                        inputId = "temp_daterange", 
  #                        start = paste0(year(g_date()), "-01-01"), 
  #                        end = g_date() - 1)
  # })
  
  
  selected_air_temp <- reactive({
    switch (input$temp_summary_choice,
      "Daily Mean" = {
        redd_air_temp %>% group_by(date = as_date(datetime)) %>% 
          summarise(parameter_value = round(mean(parameter_value, na.rm = TRUE), 0))
      },
      "7DADM" = {
        redd_air_temp %>% group_by(date = as_date(datetime)) %>% 
          summarise(parameter_value = round(max(parameter_value, na.rm = TRUE), 0))
      }, 
      "Daily Max" = {
        redd_air_temp %>% group_by(date = as_date(datetime)) %>% 
          summarise(parameter_value = round(max(parameter_value, na.rm = TRUE), 0))
      }
    ) %>% 
      filter(date >= input$temp_daterange[1], 
             date <= input$temp_daterange[2])
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
    
    # cat(unlist(make_compliance_rect()))
    
    p <- switch (input$temp_summary_choice,
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
              
              base_plot

              
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
              
              base_plot

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
              base_plot
            }
    ) 
    
    if (!input$show_air_temp) {
      p %>% 
        layout(xaxis = list(title=""), 
               yaxis = list(title = "Temperature (F)", range = c(40, 65)), 
               shapes = make_compliance_rect(), 
               legend = list(orientation = 'h')) %>%
        config(displayModeBar = FALSE) 
    } else {
      switch (input$temp_summary_choice,
        "Daily Mean" = {
          p %>% 
            add_trace(data=selected_air_temp(), 
                      x=~date, y=~parameter_value, 
                      name = "Redding Air Temp (daily avg.)", type='scatter', mode='lines', 
                      inherit = FALSE, 
                      text=~paste0(date, "<br>",
                                   "Redding Daily Average <br>", 
                                   parameter_value, "°F"), 
                      hoverinfo = "text") %>% 
            layout(xaxis = list(title=""), 
                   yaxis = list(title = "Temperature (F)"), 
                   shapes = make_compliance_rect(), 
                   legend = list(orientation = 'h')) %>%
            config(displayModeBar = FALSE)
        },
        "7DADM" = {
          p %>% 
            add_trace(data=selected_air_temp(), 
                      x=~date, y=~parameter_value, 
                      name = "Redding Air Temp (daily max)", type='scatter', mode='lines', 
                      inherit = FALSE, 
                      text=~paste0(date, "<br>",
                                   "Redding Daily Max <br>", 
                                   parameter_value, "°F"), 
                      hoverinfo = "text") %>% 
            layout(xaxis = list(title=""), 
                   yaxis = list(title = "Temperature (F)"), 
                   shapes = make_compliance_rect(), 
                   legend = list(orientation = 'h')) %>%
            config(displayModeBar = FALSE)
        },
        "Daily Max" = {
          p %>% 
            add_trace(data=selected_air_temp(), 
                      x=~date, y=~parameter_value, 
                      name = "Redding Air Temp (daily max)", type='scatter', mode='lines', 
                      inherit = FALSE, 
                      text=~paste0(date, "<br>",
                                   "Redding Daily Max <br>", 
                                   parameter_value, "°F"), 
                      hoverinfo = "text") %>% 
            layout(xaxis = list(title=""), 
                   yaxis = list(title = "Temperature (F)"), 
                   shapes = make_compliance_rect(), 
                   legend = list(orientation = 'h')) %>%
            config(displayModeBar = FALSE)
        }
      ) 
    }
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
  
  output$download_temp_data <- downloadHandler(
    filename = function() {
      switch (input$temp_summary_choice,
              "Daily Mean" = "showr_temperature_daily_mean.csv", 
              "7DADM" = "showr_temperature_7dadm.csv", 
              "Daily Max" = "showr_temperature_daily_max.csv")
    },
    content = function(file) {
      switch (input$temp_summary_choice,
              "Daily Mean" = write.csv(selected_daily_means(), file, row.names = FALSE),
              "7DADM" = write.csv(selected_7dadm(), file, row.names = FALSE),
              "Daily Max" = write.csv(selected_daily_max(), file, row.names = FALSE)
      )
    }
  )
  
  # Bookmarking this page ----------
  setBookmarkExclude(c("flow_page_bookmark", "temp_page_bookmark"))
  
  observeEvent(input$temp_page_bookmark, {
    session$doBookmark()
  })
  
  onBookmark(function(state) {
    state$values$temp_daterange_hash <- digest::digest(input$temp_daterange, "md5")
    state$values$temp_add_year_hash <- digest::digest(input$temp_add_year, "md5")
    state$values$temp_gage_location_hash <- digest::digest(input$temp_gage_location, "md5")
  })
  
  
  
  
}

