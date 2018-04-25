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
                        help_me_with_storage = actionButton(ns("help_me_with_storage"),
                                                          label = NULL,
                                                          class="help-btn pull-right",
                                                          icon = icon("question-circle")),
                        mean_daily_temp_kwk = uiOutput(ns("kwk_temp_span")), 
                        mean_daily_temp_ccr = uiOutput(ns("ccr_temp_span")),
                        mean_daily_temp_bsf = uiOutput(ns("bsf_temp_span")), 
                        help_me_with_temps = actionButton(ns("help_me_with_temps"),
                                                                label = NULL,
                                                                class="help-btn pull-right",
                                                                icon = icon("question-circle")),
                        mean_daily_flow_kwk = textOutput(ns("kwk_flow")), 
                        mean_daily_flow_sha = textOutput(ns("sha_flow")), 
                        mean_daily_flow_wlk = textOutput(ns("wlk_flow")),
                        help_me_with_flows = actionButton(ns("help_me_with_flows"),
                                                          label = NULL,
                                                          class="help-btn pull-right",
                                                          icon = icon("question-circle")),
                        wr_presence = textOutput(ns("wr_chinook_presence")),
                        limiting_reach = textOutput(ns("redd_limiting_reach")), 
                        limiting_date = textOutput(ns("redd_limiting_date")),
                        help_me_with_chinook_activity = actionButton(ns("help_me_with_chinook_activity"),
                                                          label = NULL,
                                                          class="help-btn pull-right",
                                                          icon = icon("question-circle")),
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
  
  
  # isothermal data for storage bin
  isothermal_percent_metric <- reactive({
    isothermal_data %>% 
      filter(date <= g_date(), year(date) == year(g_date())) %>% 
      arrange(desc(date)) %>% 
      group_by(date) %>% 
      mutate(total_storage = sum(volume_taf)) %>% 
      ungroup() %>% 
      mutate(proportion_of_total = volume_taf / total_storage) %>% 
      filter(temp <= 50) %>% 
      group_by(date) %>% 
      summarise(
        total_prop_below_50 = sum(proportion_of_total)
      ) %>% 
      arrange(desc(date)) %>% 
      head(1)
    
  })
  
  iso_thermal_data_for_plot <- reactive({
    
    iso_color_coded <- isothermal_data %>% 
      mutate(temp_code = case_when(
        (temp <= 48) ~ "dark blue", 
        (temp > 48 & temp <= 52) ~ "blue",
        (temp > 52 & temp <= 56) ~ "cyan",
        (temp > 56 & temp <= 60) ~ "green",
        (temp > 60 & temp <= 66) ~ "baige",
        (temp > 66 & temp <= 70) ~ "orange",
        (temp > 70) ~ "red",
        TRUE ~ as.character(NA)
      ), 
      temp_code = factor(temp_code, levels = c("dark blue", "blue", "cyan",
                                               "green", "baige", "orange", 
                                               "red")))
    t <- iso_color_coded %>%
      group_by(date, temp_code) %>%
      summarise(total_storage_in_bin = sum(volume_taf)) %>% ungroup() %>%
      mutate(volume_af = total_storage_in_bin * 1000) %>%
      arrange(date, temp_code) %>%
      group_by(date) %>%
      mutate(accum_volume = cumsum(volume_af)) %>% ungroup() %>% 
      filter(year(date) == year(g_date()), date <= g_date())
    
    list(
      "dark_blue" = t %>% 
        filter(temp_code == "dark blue"),
      
      "blue" =  t %>% 
        filter(temp_code == "blue"),
      
      "cyan" = t %>% 
        filter(temp_code == "cyan"),
      
      "green" = t %>% 
        filter(temp_code == "green"),
      
      "baige" =  t %>% 
        filter(temp_code == "baige"),
      
      "orange" = t %>% 
        filter(temp_code == "orange"),
      
      "red" = t %>% 
        filter(temp_code == "red")
    )
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
    validate(need(nrow(isothermal_percent_metric()) > 0, "No Data"))
    x <- isothermal_percent_metric()
    
    paste(pretty_num(x$total_prop_below_50*100), "%")
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
    shasta_spread_data <- selected_shasta_storage_data() %>% 
      spread(parameter_id, parameter_value) 
    
    plot_ly() %>% 
    add_trace(data = shasta_spread_data, 
              x=~datetime, y = ~`15`, name ="Reservoir Storage", fill = 'tozeroy',
              fillcolor="#bcbcbc",
              connectgaps = TRUE,
              type='scatter', mode='none') %>%
      add_trace(data = iso_thermal_data_for_plot()$red, 
                x=~date, y=~accum_volume, 
                fillcolor = '#d64646', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name=">71", 
                text = ~paste0(
                  "Storage Temperature Above 70 <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>% 
      add_trace(data = iso_thermal_data_for_plot()$orange, 
                x=~date, y=~accum_volume, 
                fillcolor = '#ed8e12', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name="67-70", 
                text = ~paste0(
                  "Storage Temperature Between 67 & 70 °F <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>% 
      add_trace(data = iso_thermal_data_for_plot()$baige, 
                x=~date, y=~accum_volume, 
                fillcolor = '#d9e0a3', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name="61-66", 
                text = ~paste0(
                  "Storage Temperature Between 61 & 66 °F <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>% 
      add_trace(data = iso_thermal_data_for_plot()$green, 
                x=~date, y=~accum_volume, 
                fillcolor = '#27824c', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name="57-60", 
                text = ~paste0(
                  "Storage Temperature Between 57 & 60 °F <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>%  
      add_trace(data = iso_thermal_data_for_plot()$cyan, 
                x=~date, y=~accum_volume, 
                fillcolor = '#56bfbd', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name="53-56",
                text = ~paste0(
                  "Storage Temperature Between 53 & 56 °F <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>%  
      add_trace(data = iso_thermal_data_for_plot()$blue, 
                x=~date, y=~accum_volume, 
                fillcolor = '#4C74C9', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name="49-52", 
                text = ~paste0(
                  "Storage Temperature Between 49 & 52 °F <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>%  
      add_trace(data = iso_thermal_data_for_plot()$dark_blue, 
                x=~date, y=~accum_volume, 
                fillcolor = '#274a82', inherit = FALSE, 
                type='scatter', mode = 'none', fill='tozeroy', 
                legendgroup="Isothermals", 
                name="<48", 
                text = ~paste0(
                  "Storage Temperature Below 48 <br>",
                  date, "<br>",
                  "Volume ", round(total_storage_in_bin, 0), " taf"
                ), 
                hoverinfo = "text") %>% 
      add_lines(data=shasta_spread_data, 
                x = ~datetime, y=~`94`, name = "Flood Conservation Max Storage", type = 'scatter', 
                mode = 'lines', text = ~paste("<b>",datetime, "</b>",
                                              "<br>", 
                                              "<b>",round(`94`/1000000, 3), "MAF</b>"),
                hoverinfo = "text",
                connectgaps = TRUE,
                line = list(color = '#990000', width = 2, dash = 'dash'), 
                inherit = FALSE) %>%
      add_lines(data=shasta_spread_data, 
                x = ~datetime, y=~`15`, name ="Reservoir Storage",
                text = ~paste("<b>",datetime, "</b>",
                              "<br>",
                              "<b>",round(`15`/1000000, 3), "MAF</b>"),
                hoverinfo = "text",
                connectgaps = TRUE,
                line = list(color="black", width = 2), 
                inherit = FALSE) %>%
      layout(xaxis = list(title = '', fixedrange = TRUE),
             yaxis = list(title = "storage (AF)") 
             ,legend = list(orientation = 'h')
             ) %>% 
      config(displayModeBar = FALSE)
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
  
  
  observeEvent(input$help_me_with_temps, {
    showModal(modalDialog(
      title = "Outflow Temperatures",
      tagList(
        tags$h4("Keswick Temperature Compliance"), 
        tags$p("Based on Action 1.2.4 Reclamation will manage Keswick Reservoir from", 
               tags$b("May 15 through October"), "such that daily average temperatures 
               do not exceed 56°F at compliance locations between", 
               tags$b("Ballls Ferry and Bend Bridge")), 
        tags$br(), 
        tags$p("You can view a full temperature profile by clicking on the", tags$b("Temperature"), 
               "tab above."), 
        tags$hr(), 
        tags$p("For more information you can view the report from the Sacramento Temperature 
               Task Group", tags$a("here",
                                   target="_blank",
                                   href="http://deltacouncil.ca.gov/sites/default/files/2017/11/17%20%20SRTTG%202017%20Annual%20Report.pdf"))
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_me_with_flows, {
    showModal(modalDialog(
      title = "Outflow Temperatures",
      tagList(
        tags$h4("Keswick Temperature Compliance"), 
        tags$p("Based on Action 1.2.4 Reclamation will manage Keswick Reservoir from", 
               tags$b("May 15 through October"), "such that daily average temperatures 
               do not exceed 56°F at compliance locations between", 
               tags$b("Ballls Ferry and Bend Bridge")), 
        tags$br(), 
        tags$p("You can view a full temperature profile by clicking on the", tags$b("Temperature"), 
               "tab above."), 
        tags$hr(), 
        tags$p("For more information you can view the report from the Sacramento Temperature 
               Task Group", tags$a("here",
                                   target="_blank",
                                   href="http://deltacouncil.ca.gov/sites/default/files/2017/11/17%20%20SRTTG%202017%20Annual%20Report.pdf"))
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_me_with_chinook_activity, {
    showModal(modalDialog(
      title = "Outflow Temperatures",
      tagList(
        tags$h4("Keswick Temperature Compliance"), 
        tags$p("Based on Action 1.2.4 Reclamation will manage Keswick Reservoir from", 
               tags$b("May 15 through October"), "such that daily average temperatures 
               do not exceed 56°F at compliance locations between", 
               tags$b("Ballls Ferry and Bend Bridge")), 
        tags$br(), 
        tags$p("You can view a full temperature profile by clicking on the", tags$b("Temperature"), 
               "tab above."), 
        tags$hr(), 
        tags$p("For more information you can view the report from the Sacramento Temperature 
               Task Group", tags$a("here",
                                   target="_blank",
                                   href="http://deltacouncil.ca.gov/sites/default/files/2017/11/17%20%20SRTTG%202017%20Annual%20Report.pdf"))
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_me_with_storage, {
    showModal(modalDialog(
      title = "Outflow Temperatures",
      tagList(
        tags$h4("Keswick Temperature Compliance"), 
        tags$p("Based on Action 1.2.4 Reclamation will manage Keswick Reservoir from", 
               tags$b("May 15 through October"), "such that daily average temperatures 
               do not exceed 56°F at compliance locations between", 
               tags$b("Ballls Ferry and Bend Bridge")), 
        tags$br(), 
        tags$p("You can view a full temperature profile by clicking on the", tags$b("Temperature"), 
               "tab above."), 
        tags$hr(), 
        tags$p("For more information you can view the report from the Sacramento Temperature 
               Task Group", tags$a("here",
                                   target="_blank",
                                   href="http://deltacouncil.ca.gov/sites/default/files/2017/11/17%20%20SRTTG%202017%20Annual%20Report.pdf"))
      ), 
      easyClose = TRUE
    ))
  })
}

global_date <- function(input, output, session) {
  g_date <- reactive({ input$global_date })
  return(g_date)
}


