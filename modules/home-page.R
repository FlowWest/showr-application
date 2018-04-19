# Author: Emanuel
# Details:
# This file defines both the UI and server for the home page of showr.

homeUI <- function(id){
  ns <- NS(id)
  tagList(
           htmlTemplate("templates/dashboard.html", 
                        shasta_storage_val = textOutput(ns("shasta_storage")), 
                        shasta_cons_curve_val = textOutput(ns("shasta_cons_curve")), 
                        water_year_type_val = textOutput(ns("water_year_class")),
                        mean_daily_temp_kwk = uiOutput(ns("kwk_temp_span")), 
                        mean_daily_temp_ccr = uiOutput(ns("ccr_temp_span")),
                        mean_daily_temp_bsf = uiOutput(ns("bsf_temp_span")), 
                        mean_daily_flow_kwk = textOutput(ns("kwk_flow")), 
                        mean_daily_flow_sha = textOutput(ns("sha_flow")), 
                        mean_daily_flow_wlk = textOutput(ns("wlk_flow")),
                        wr_presence = textOutput(ns("wr_chinook_presence")),
                        limiting_reach = textOutput(ns("redd_limiting_reach")), 
                        limiting_date = textOutput(ns("redd_limiting_date")),
                        shasta_elevation_plot = plotlyOutput(ns("elevation_plot"), height = '500px'),
                        chinook_summary_plot = plotlyOutput(ns("chinook_summary_plot"), height = '500px'),
                        date_button = dateInput(ns("global_date"), label = "Select Date", 
                                    min = "2010-01-01", max = (today(tzone = "America/Los_Angeles")), value = (today(tzone = "America/Los_Angeles")), 
                                    width = "150px")
                        ))
}


home_server <- function(input, output, session, g_date) {
  ns <- session$ns
  
  home_year <- reactive({ lubridate::year(g_date()) })

  # Reservoir Metrics ----------------------------------------------------------
  selected_shasta_storage <- reactive({
    
    shasta_storage_data %>% 
      filter(datetime == g_date(), parameter_id == "15") %>% 
      pull(parameter_value)
  })
  
  selected_shasta_cons_curve <- reactive({
    if (g_date() == today(tzone = "America/Los_Angeles")) {
      shasta_storage_data %>% 
        filter(datetime == g_date()-2, parameter_id == "94") %>% 
        pull(parameter_value)  
    } else if (g_date() == (today(tzone = "America/Los_Angeles")-1)) {
      shasta_storage_data %>% 
        filter(datetime == g_date()-1, parameter_id == "94") %>% 
        pull(parameter_value) 
    } else {
      shasta_storage_data %>% 
        filter(datetime == g_date(), parameter_id == "94") %>% 
        pull(parameter_value)
    }
  })
  
  selected_shasta_percent_of_full <- reactive({
    selected_shasta_storage()/selected_shasta_cons_curve()
  })
  
  output$shasta_storage <- renderText({
    validate(need(length(selected_shasta_storage()) > 0, "No Data"))
    x <- selected_shasta_storage()[1]
    paste(pretty_num(x/1000000), "MAF")
  })
  
  output$shasta_cons_curve <- renderText({
    validate(need(length(selected_shasta_cons_curve()) > 0, "No Data"))
    x <- selected_shasta_cons_curve()[1]
    paste(pretty_num(x/1000000), "MAF")
  })
  
  output$water_year_class <- renderText({
    get_year_classification(year(g_date()))
  })
  
  # Temperature Readings -------------------------------------------------------
  
  latest_temperature_reading <- function(location) {
    temp_compliance_points_daily_mean %>% 
      filter(location_id == location, datetime == g_date()) %>% 
      pull(parameter_value)
  }
  
  latest_kwk_temperature <- reactive({latest_temperature_reading('kwk')})
  latest_ccr_temperature <- reactive({latest_temperature_reading('ccr')})
  latest_bsf_temperature <- reactive({latest_temperature_reading('bsf')})
  
  # Temperature UI is reactive and depending on the value of temperature, the 
  # badge will change colors
  output$kwk_temp_span <- renderUI({
    validate(need(length(latest_kwk_temperature()) > 0, "No Data"))
    validate(need(!is.na(latest_kwk_temperature()), "No Data"))
    
    if (latest_kwk_temperature() >= 56) {
      tags$span(class="badge badge-warning", textOutput(ns("kwk_temp")))
    } else {
      textOutput(ns("kwk_temp"))
    }
  })
  output$ccr_temp_span <- renderUI({
    validate(need(length(latest_ccr_temperature()) > 0, "No Data"))
    validate(need(!is.na(latest_ccr_temperature()), "No Data"))
    
    if (latest_ccr_temperature() >= 56) {
      tags$span(class="badge badge-warning", textOutput(ns("ccr_temp")))
    } else {
      textOutput(ns("ccr_temp"))
    }
  })
  output$bsf_temp_span <- renderUI({
    validate(need(length(latest_bsf_temperature()) > 0, "No Data"))
    validate(need(!is.na(latest_bsf_temperature()), "No Data"))
    
    if (latest_bsf_temperature() >= 56) {
      tags$span(class="badge badge-warning", textOutput(ns("bsf_temp")))
    } else {
      textOutput(ns("bsf_temp"))
    }
  })
  
  output$kwk_temp <- renderText({
    validate(need(length(latest_kwk_temperature()) > 0, "No Data"))

    paste(latest_kwk_temperature(), "°F")
  })
  
  output$ccr_temp <- renderText({
    validate(need(length(latest_ccr_temperature()) > 0, "No Data"))
    paste(latest_ccr_temperature(), "°F")
  })
  output$bsf_temp <- renderText({
    validate(need(length(latest_bsf_temperature()) > 0, "No Data"))
    paste(latest_bsf_temperature(), "°F")
  })
  
  
  # Flow Readings --------------------------------------------------------------
  
  latest_flow_reading <- function(location) {
    
    if (location == 'sha') {
      if (g_date() == today(tzone = "America/Los_Angeles")) {
        flow_data_daily_mean %>% 
          filter(location_id == location, datetime == (g_date()-2)) %>%
          pull(parameter_value)
      } else if (g_date() == (today(tzone = "America/Los_Angeles")-1)) {
        flow_data_daily_mean %>% 
          filter(location_id == location, datetime == (g_date()-1)) %>%
          pull(parameter_value)
      } else {
        flow_data_daily_mean %>% 
          filter(location_id == location, datetime == g_date()) %>%
          pull(parameter_value)
      }
    } else {
      
      flow_data_daily_mean %>% 
        filter(location_id == location, datetime == g_date()) %>%
        pull(parameter_value) 
    }
  }
  
  latest_kwk_flow <- reactive({latest_flow_reading('kwk')})
  latest_sha_flow <- reactive({latest_flow_reading('sha')})
  latest_wlk_flow <- reactive({latest_flow_reading('wlk')})
  
  output$kwk_flow <- renderText({
    validate(need(length(latest_kwk_flow()) > 0, "No Data"))
    paste(latest_kwk_flow(), "cfs")
  })
  output$sha_flow <- renderText({
    validate(need(!is.na(latest_sha_flow()), "No Data"))
    paste(latest_sha_flow(), "cfs")
  })
  output$wlk_flow <- renderText({
    validate(need(length(latest_wlk_flow()) > 0, "No Data"))
    paste(latest_wlk_flow(), "cfs")
  })
  
  # Chinook Activity Panel -----------------------------------------------------
  
  # Recent redds with positive count, used for calculating 
  # limiting redds in the system
  selected_recent_redd_observations <- reactive({
    redd_data %>% 
      filter(date <= g_date(),
             year(date) == year(g_date()),
             counts > 0)
  })
  
  selected_redds_with_emergence <- reactive({
    validate(need(nrow(selected_recent_redd_observations()) > 0, 
                   "Preseason"))
    d <- selected_recent_redd_observations()$date
    l <- selected_recent_redd_observations()$location
    
    estimated_emergence <- 
      purrr::map2_dbl(d, l, estimate_emergence) %>% 
      as_date()
    
    bind_cols(date=d, location=l, emergence=estimated_emergence)
  })
  
  limititng_reach_in_system <- reactive({
    selected_redds_with_emergence() %>% 
      filter(emergence >= g_date()) %>% 
      arrange(emergence) %>% 
      tail(1)
  })
  
  selected_chinook_presence <- reactive({
    wr_presence_data %>% 
      filter(month == month(g_date())) %>% 
      pull(activity)
  })
  
  output$wr_chinook_presence <- renderText({
    selected_chinook_presence()
  })
  
  output$redd_limiting_reach <- renderText({
    limititng_reach_in_system()$location
  })
  
  output$redd_limiting_date <- renderText({
    x <- limititng_reach_in_system()$emergence
    paste(as.numeric(x - g_date()), "days")
  })
  
  
  
  # Visualizations -------------------------------------------------------------
  
  selected_shasta_storage_data <- reactive({
    shasta_storage_data %>% 
      filter(year(datetime) == year(g_date()), datetime <= g_date())
  }) 
  
  selected_chinook_data <- reactive({
    redd_data %>% 
      filter(date <= g_date(), year(date) == year(g_date()), counts > 0)
  })
  
  output$elevation_plot <- renderPlotly({
    selected_shasta_storage_data() %>% 
      spread(parameter_id, parameter_value) %>% 
      plot_ly(x = ~datetime, y=~`94`, name = "Flood Conservation Max Storage", type = 'scatter', 
              mode = 'lines', text = ~paste("<b>",datetime, "</b>",
                                            "<br>", 
                                            "<b>",round(`94`/1000000, 3), "MAF</b>"),
              hoverinfo = "text",
              connectgaps = TRUE,
              line = list(color = '#990000', width = 2, dash = 'dash')) %>% 
      add_trace(y = ~`15`, name ="Reservoir Storage", fill = 'tozeroy',
                text = ~paste("<b>",datetime, "</b>",
                              "<br>",
                              "<b>",round(`15`/1000000, 3), "MAF</b>"),
                hoverinfo = "text",
                connectgaps = TRUE,
                line = list(color = '#666', width = 3, dash = 'solid')) %>%
      layout(xaxis = list(title = '', fixedrange = TRUE),
             yaxis = list(title = "storage (AF)"), 
             legend = list(orientation = 'h')) %>% 
      config(displayModeBar = FALSE)
  })
  
  observe({
    
  })
  
  output$chinook_summary_plot <- renderPlotly({
    
    # 
    # validate(need(nrow(selected_chinook_data()) > 0, 
    #               "0 Counts of Redds to date this year"))
    
    if (nrow(selected_chinook_data()) == 0) {
      return(redd_data %>% 
        group_by(month = factor(month.abb[as.integer(month(date))], levels=month.abb), 
                 location) %>% 
        summarise(
          month_counts = sum(counts)
        ) %>% 
        plot_ly(x=~month, y=~month_counts, type='bar', color=~location) %>% 
        layout(barmode='stack'))
    }
    
    selected_chinook_data() %>% 
      group_by(location, month = month(date)) %>% 
      summarise(
        month_total = sum(counts, na.rm = TRUE)
      ) %>% 
      plot_ly(x=~month_total, y=~month, color =~ location, type='bar', orientation = 'h') %>% 
      layout(title = "Aerial Redd Counts",
             barmode = 'stack', 
             legend = list(orientation = 'h'),
             yaxis = list(tickmode = 'array', tickvals = 1:12, ticktext = month.abb),
             xaxis = list(title = '')) %>% 
      config(displayModeBar = FALSE)
  })
}

global_date <- function(input, output, session) {
  g_date <- reactive({ input$global_date })
  return(g_date)
}


