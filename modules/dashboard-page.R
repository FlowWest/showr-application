# Author: Emanuel
# Details:
# This file defines both the UI and server for the dashboard page of showr.

dashboardUI <- function(id){
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
                 date_button = dateInput(ns("global_date"), 
                                         label = NULL, 
                                         min = "2010-01-01", 
                                         max = (today(tzone = "America/Los_Angeles")), 
                                         value = (today(tzone = "America/Los_Angeles")), 
                                         width = "145px", 
                                         format = "M d, yyyy"), 
                 help_me_button = actionButton(ns("dashboard_help_me"), label=NULL,
                                               icon = icon("question"), 
                                               class = "btn-sm dash_help btn-primary"), 
                 temp_go_to_details_button = actionButton(ns("go_to_temp_details"), 
                                                          label = "plot",
                                                          icon = icon("line-chart"), 
                                                          class = "btn-xs details_button btn-success pull-right"),
                 # storage_go_to_details_button = actionButton(ns("go_to_storage_details"), 
                 #                                             label = "plot",
                 #                                             icon = icon("line-chart"), 
                 #                                             class = "btn-xs details_button btn-success pull-right"),
                 flow_go_to_details_button = actionButton(ns("go_to_flow_details"), 
                                                          label = "plot",
                                                          icon = icon("line-chart"),
                                                          class = "btn-xs details_button btn-success pull-right"),
                 # chinook_go_to_details_button = actionButton(ns("go_to_chinook_details"), 
                 #                                             label = NULL,
                 #                                             icon = icon("arrow-right"), 
                 #                                             class = "btn-xs details_button btn-success pull-right"),
                 jump_to_year_button = dropdownButton(
                   actionButton(ns("jump_to_2016"), 
                                label = "Below Normal (2016)",
                                class = "btn-sm jump_year_button btn-success"),
                   actionButton(ns("jump_to_2015"), 
                                label = "Dry (2015)",
                                class = "btn-sm jump_year_button btn-success"),
                   circle = FALSE, status = "info", icon = icon("calendar-plus-o"), width = "300px",
                   actionButton(ns("jump_to_2017"), 
                                label = "Wet (2017)",
                                class = "btn-sm jump_year_button btn-success"),
                   tooltip = FALSE, label = "Select Water Year Type",
                   size = "sm"
                 )
    ))
}


dashboard_server <- function(input, output, session, g_date, x) {
  ns <- session$ns
  
  starting_date <- today(tzone = "America/Los_Angeles")
  home_year <- reactive({ lubridate::year(g_date()) })
  
  observeEvent(input$jump_to_2016, {
    if (input$jump_to_2016 %% 2 == 0) {
      updateDateInput(session, "global_date", value = starting_date)
      updateActionButton(session, inputId = "jump_to_2016", label = "Below Normal (2016)") # when reset it clicked
    } else {
      updateDateInput(session, "global_date", value = `year<-`(g_date(), 2016))
      updateActionButton(session, inputId = "jump_to_2016", label = "reset") # when last year is clicked
      
    }
  })
  
  observeEvent(input$jump_to_2015, {
    if (input$jump_to_2015 %% 2 == 0) {
      updateDateInput(session, "global_date", value = starting_date)
      updateActionButton(session, inputId = "jump_to_2015", label = "Dry (2015)") # when reset it clicked
    } else {
      updateDateInput(session, "global_date", value = `year<-`(g_date(), 2015))
      updateActionButton(session, inputId = "jump_to_2015", label = "reset") # when last year is clicked
      
    }
  })
  
  observeEvent(input$jump_to_2017, {
    if (input$jump_to_2015 %% 2 == 0) {
      updateDateInput(session, "global_date", value = starting_date)
      updateActionButton(session, inputId = "jump_to_2017", label = "Wet (2017)") # when reset it clicked
    } else {
      updateDateInput(session, "global_date", value = `year<-`(g_date(), 2017))
      updateActionButton(session, inputId = "jump_to_2017", label = "reset") # when last year is clicked
      
    }
  })
  
  # observeEvent(input$jump_to_similar_year, {
  #   similar_year <- get_similar_year("")
  #   if (input$jump_to_similar_year %% 2 == 0) {
  #     updateDateInput(session, "global_date", value = starting_date)
  #     updateActionButton(session, inputId = "jump_to_similar_year", label = "similar year") # when reset it clicked  
  #   } else {
  #     updateDateInput(session, "global_date", value = date_last_year())
  #     updateActionButton(session, inputId = "jump_to_similar_year", label = "reset") # when last year is clicked 
  #     
  #   }
  # })
  
  
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
      filter(temp <= 52) %>% 
      group_by(date) %>% 
      summarise(
        total_volume_below_50 = sum(volume_taf)
      ) %>% ungroup() %>% 
      arrange(desc(date)) %>% head(1)
    
    
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
    
    if (nrow(t) == 0) {
      return(NULL)
    }
    
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
    
    paste(pretty_num((x$total_volume_below_50*1000)/1e6), " MAF")
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
          pull(parameter_value) %>% 
          round()
      } else if (g_date() == (today(tzone = "America/Los_Angeles")-1)) {
        flow_data_daily_mean %>% 
          filter(location_id == location, datetime == (g_date()-1)) %>%
          pull(parameter_value) %>% round()
      } else {
        flow_data_daily_mean %>% 
          filter(location_id == location, datetime == g_date()) %>%
          pull(parameter_value) %>% 
          round()
      }
    } else {
      
      flow_data_daily_mean %>% 
        filter(location_id == location, datetime == g_date()) %>%
        pull(parameter_value) %>% 
        round()
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
  
  observeEvent(input$dashboard_plot_tabs, {
    x <- input$dashboard_plot_tabs
    if (x == "1a") return(NULL)
    else if (x == "2a") {
      if (nrow(selected_chinook_data()) == 0) {
        showNotification("You are viewing historical observations for Redds.
                         There are currently no observations this year.")
      } else {
        return(NULL)
      }
    }
  })
  
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
    
    p <- plot_ly() %>% 
      add_trace(data = shasta_spread_data, 
                x=~datetime, y = ~`15`, name ="Reservoir Storage", fill = 'tozeroy',
                fillcolor="#bcbcbc",
                connectgaps = TRUE,
                type='scatter', mode='none')
    
    # if iso data is available add them to the plot
    if (!is.null(iso_thermal_data_for_plot())) {
      p <- p %>% 
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
                  hoverinfo = "text")
    }
    p %>%
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
    
    # the plot shown here depends on whether there are active redds
    # in the system or not. If not show total observation of redds
    # by reach in all years, otherwise show plot
    
    if (nrow(selected_chinook_data()) == 0) {
      return(
        redd_data %>% 
          mutate(total_count = sum(counts, na.rm = TRUE), 
                 location = factor(location, levels = redd_locations)) %>% 
          group_by(month = month(date), location) %>% 
          summarise(
            proportion_in_month = sum(counts, na.rm = TRUE) / max(total_count)
          ) %>% 
          ungroup() %>% 
          group_by(month, location) %>% 
          summarise(
            prop_of_location_in_month = sum(proportion_in_month) * 100
          ) %>% ungroup() %>% 
          filter(prop_of_location_in_month > 0) %>% 
          mutate(month = factor(month.abb[month], levels = month.abb)) %>% 
          plot_ly(x=~month, y=~prop_of_location_in_month, color=~location, 
                  type='bar', 
                  text = ~paste0(location, "<br>", 
                                 round(prop_of_location_in_month), "%"), 
                  hoverinfo = "text", 
                  colors = "Dark2") %>% 
          layout(barmode='stack', 
                 yaxis = list(title = "Percent of Mapped Redds"), 
                 xaxis = list(title = ""), 
                 title = "Historic Proportions of Winter Run Redds")
      )
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
      title = "River Temperatures",
      tagList(
        tags$p("The Long Term Operations Biological Opinion requires Reclamation to manage flows from Shasta and Keswick between May 15 and October 31 such that daily average temperatures do not exceed 56°F at compliance locations between Balls Ferry and Bend Bridge."),
        tags$p("Keswick (",
               tags$a("KWK", href="http://cdec.water.ca.gov/dynamicapp/QueryF?s=kwk", target="_blank"),
               ") is Sacramento River water temperature at the outflow from", tags$a("Keswick Dam", href="https://en.wikipedia.org/wiki/Clear_Creek_(Sacramento_River_tributary)", target="_blank"), "."), 
        tags$br(),
        tags$p("Clear Creek (",
               tags$a("CCR", href="http://cdec.water.ca.gov/dynamicapp/QueryF?s=ccr", target="_blank"),
               ") is Sacramento River water temperature at the first major tributary to the Sacramento River downstream of Shasta Dam."),
        tags$br(),
        tags$p("Balls Ferry (",
               tags$a("KWK", href="http://cdec.water.ca.gov/dynamicapp/QueryF?s=bsf", target="_blank"),
               ") is Sacramento River water temperature at the most downstream compliance point location."),
        tags$br(),
        tags$p("Temperatures shaded red indicate non-compliance with Winter Run temperature management requirements."),
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
      title = "Flows",
      tagList(
        tags$p(tags$b("Shasta Reservoir"), "reports the full natural Sacramento River flow into Shasta Reservoir, as recorded by the", 
               tags$a("SHA", href="https://cdec.water.ca.gov/dynamicapp/QueryF?s=SHA", target="_blank"), 
               "gauge upstream of Shasta Reservoir."),
        tags$hr(),
        tags$p(tags$b("Keswick"), "reports Sacramento River flow, as recorded at the",
               tags$a("KWK", href="https://cdec.water.ca.gov/dynamicapp/QueryF?s=kwk", target="_blank"),
               "gauge downstream of Keswick Dam."),
        tags$hr(),
        tags$p(tags$b("Wilkins Slough"), "reports flow using CDEC gauge WLK"), 
        tags$hr(),
        tags$p("You can view detailed flow hydrographs for these and other Sacramento River flow gages by clicking on the", tags$b("Flow tab"), "above.")
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_me_with_chinook_activity, {
    showModal(modalDialog(
      title = "Chinook Activity",
      tagList(
        tags$p(tags$b("Life Stage"), "reports the Winter Run Chinook salmon life stages expected to be present on the selected date based on historical data on Winter Run migration, spawning, emergence, and rearing."), 
        tags$hr(),
        tags$p(tags$b("Furthest Redd"), "lists the river reach farthest downstream 
               of Shasta Dam with Winter Run redd(s) containing eggs or 
               juvenile Winter Run that have not yet emerged."),
        tags$hr(),
        tags$p(tags$b("Estimated Emergence"), "reports the number of days remaining until all juvenile Winter Run have emerged from the current most downstream redd."),
        tags$hr(),
        tags$p("Emergence is estimated using Accumulated Temperature Units, and either known river
               temperatures or modeled temperatures from", 
               tags$a(href="https://oceanview.pfeg.noaa.gov/CVTEMP/", "CVTemp")),
        tags$p("You can view detailes on current and historical Winter Run Chinook Salmon activity by clicking on the Winter Run Chinook tab above.")
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_me_with_storage, {
    showModal(modalDialog(
      title = "Shasta Dam Operation",
      tagList(
        tags$p(tags$b("Storage"), "is monitored by Reclamation and recorded by CDEC (", tags$a("SHA", href="http://cdec.water.ca.gov/dynamicapp/QueryF?s=SHA", target="_blank"),"). Storage is displayed in in Million Acre Feet (MAF). The plot to the right provides a time series of Shasta storage values."),
        tags$hr(),
        tags$p(tags$b("Storage below 50°F"), "reports the total volume of storage in Shasta below 50°F."),
        tags$hr(),
        tags$p(tags$b("Water Year Classification"), "reports the water year type according to the", 
               tags$a("Sacramento River Index", target="_blank", 
                      href="http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST"))
      ), 
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$dashboard_help_me, {
    showModal(modalDialog(
      title = "Welcome to the SHOWR Dash!",
      tagList(
        tags$h5("Quickly navigate through historical data"), 
        tags$img(src="dash-help-dates.gif"), 
        tags$hr(),
        tags$h5("Be alert for concerning river temperatures"), 
        tags$img(src="temp-alerts.gif",width= "670px"), 
        tags$hr()
      ),
      easyClose = TRUE, 
      size = "l"
    ))
  })
  
  # go to details pages 
  observeEvent(input$go_to_temp_details, {
    showModal(modalDialog(
      title = "Current Temperature Conditions", 
      tagList(
        radioButtons(ns("temp_modal_location_select"), label = NULL, inline = TRUE,
                     choices = c("Keswick"="kwk", "Clear Creek"="ccr", "Balls Ferry"="bsf")),
        plotlyOutput(ns("temp_modal_plot")), 
        actionButton(ns("temp_modal_to_temp_page_button"), "Show me more")), 
      size = "l", easyClose = TRUE
    ))
  })
  
  observeEvent(input$temp_modal_to_temp_page_button, {
    updateNavbarPage(x, inputId = "showrapp", selected = "temp_tab")
  })
  
  
  temp_daily_min_max <- reactive({
    historical_daily_min_max_temps %>% 
      mutate(
        datetime = ymd(paste(year(g_date()), m, d, sep = "/"))
      ) %>% 
      filter(
        location_id == input$temp_modal_location_select,
        datetime <= g_date(), 
        datetime >= g_date()-60)
  })
  
  output$temp_modal_plot <- renderPlotly({
    
    plot_ly() %>% 
      add_lines(data=temp_daily_min_max(), 
                x=~datetime, y=~min_value, 
                line=list(color='rgb(196, 196, 196)'), name="Historical Max", 
                hoverinfo="text", text=~paste(min_value)) %>% 
      add_lines(data=temp_daily_min_max(),
                x=~datetime, y=~max_value, 
                line=list(color='rgb(196, 196, 196)'), name="Historical Min", 
                hoverinfo="text", text=~paste(max_value)) %>% 
      add_lines(data=filter(temp_compliance_points_daily_mean, location_id ==input$temp_modal_location_select, 
                            datetime>=g_date()-60, datetime <= g_date()),
                x=~datetime, y=~parameter_value, 
                line=list(color="black"), name=~location_id) %>% 
      layout(xaxis=list(title=""), yaxis=list(title="Temperature (F)"))
    
  })
  
  
  observeEvent(input$go_to_flow_details, {
    showModal(modalDialog(
      title = "Current Flow Conditions", 
      tagList(
        radioButtons(ns("flow_modal_location_select"), label = NULL, inline = TRUE,
                     choices = c("Shasta Inflow"="sha", 
                                 "Keswick Outflow"="kwk", 
                                 "Wilkins Slough"="wlk")),
        plotlyOutput(ns("flow_modal_plot")), 
        actionButton(ns("flow_modal_to_flow_page_button"), "Show me more")), 
      size = "l", easyClose = TRUE
    ))
  })
  
  flow_daily_min_max <- reactive({
    historical_daily_min_max_flows %>% 
      mutate(
        datetime = ymd(paste(year(g_date()), m, d, sep = "/"))
      ) %>% 
      filter(
        location_id == input$flow_modal_location_select,
        datetime <= g_date(), 
        datetime >= g_date()-60)
  })
  
  output$flow_modal_plot <- renderPlotly({
    
    plot_ly() %>% 
      add_lines(data=flow_daily_min_max(), 
                x=~datetime, y=~min_value, 
                line=list(color='rgb(196, 196, 196)'), name="Historical Mean Max", 
                hoverinfo="text", text=~paste(min_value)) %>% 
      add_lines(data=flow_daily_min_max(),
                x=~datetime, y=~max_value, 
                line=list(color='rgb(196, 196, 196)'), name="Historical Mean Min", 
                hoverinfo="text", text=~paste(max_value)) %>% 
      add_lines(data=filter(flow_data_daily_mean, location_id ==input$flow_modal_location_select, 
                            datetime>=g_date()-60, datetime <= g_date()),
                x=~datetime, y=~parameter_value, 
                line=list(color="black"), name=~location_id) %>% 
      layout(xaxis=list(title=""), yaxis=list(title="Flow (cfs)"))
    
  })
  
  
  
}

global_date <- function(input, output, session) {
  g_date <- reactive({ input$global_date })
  return(g_date)
}


