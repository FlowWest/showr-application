flow_UI <- function(id) {
  ns <- NS(id) 
  
  
  tagList(
    fluidRow(
      # flow context sidebar
      column(width = 12, class = "col-md-3", 
             tags$div(class = "flow-sidebar", 
                      tags$h2("Flow"),
                      tags$hr(),
                      tags$p(
                        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ac scelerisque quam. Fusce eget risus eros. Cras elementum nulla velit, in lacinia mauris euismod ut. Praesent ut semper nunc. Cras porttitor elit sem, id molestie purus fringilla nec. Aliquam vehicula lacinia aliquam. Curabitur et leo elit. Sed egestas massa sit amet turpis faucibus blandit. Curabitur vel efficitur tellus, accumsan dapibus diam. Vivamus tincidunt leo vel placerat facilisis. Duis id augue ac dui posuere hendrerit."
                      ),
                      tags$h3("Download Data in View"),
                      downloadButton(ns("download_flow_data")), 
                      tags$h5("Data source: data obtained from CDEC and USGS(NWIS)"), 
                               tags$h5("Latest TCD configurations transcribed from",
                                       tags$a(target="_blank",
                                              "CVO TCD Configurations",
                                              href="https://www.usbr.gov/mp/cvo/vungvari/ShastaTCD2017.pdf")),
                               tags$h5("Update schedule: data is updated on daily basis with both hourly and daily data"))),
      # main interface
      column(width = 12, class = "col-md-9",
             # controls
             fluidRow( 
                      column(width = 12, class = "col-md-3", 
                             dateRangeInput(inputId = ns("flow_daterange"), 
                                            label = "Select a Date Range", 
                                            min = "1999-01-01", 
                                            start = paste0(year(today()), "-01-01"), 
                                            end = today(tzone = "America/Los_Angeles")-1)), 
                      column(width = 12, class = "col-md-2", 
                             selectInput(ns("flow_add_year"), label = NULL, choices = c("None", 2010:2017), 
                                         width = "75px")), 
                      column(width = 12, class = "col-md-3", 
                             selectInput(inputId = ns("flow_station_select"), 
                                         label = "Select Stations",
                                         choices = c("Shasta (Natural Flow)" = "sha", 
                                                     "Keswick" = "kwk", 
                                                     "Bend Bridge" = "bnd", 
                                                     "Wilkins Slough" = "wlk"),
                                         multiple = TRUE,
                                         selected = c("sha", "kwk"), width = "400px")), 
                      column(width = 12, class = "col-md-3", 
                             checkboxInput(ns("show_diversions"), label = "Show diversion"))
                      ), 
             
             fluidRow(
               # plot
               column(width = 12, class = "col-md-9", 
                      plotlyOutput(ns("flow_plot"))), 
               column(width = 12, 
                      class = "col-md-3", 
                      uiOutput(ns("table_title")),
                      tags$table(class="table",tags$tbody(
                        tags$tr(tags$td("McCloud"), 
                                tags$td(textOutput(ns("mccloud_summmary_flow"))),
                                tags$td(sparklineOutput(ns("mccloud_spark")), tags$h6("past 30 days"))), 
                        tags$tr(tags$td("Sac River at Delta"),
                                tags$td(textOutput(ns("delta_summary_flow"))),
                                tags$td(sparklineOutput(ns("sac_river_spark")), tags$h6("past 30 days")))
                      )))))
    )
  )
}

flow_server <- function(input, output, session, g_date) {
  ns <- session$ns
  observeEvent(g_date(), 
               updateDateRangeInput(session = session, 
                                    inputId = "flow_daterange", 
                                    start = paste0(year(g_date()), "-01-01"), 
                                    end = g_date()-1))
  
  selected_flow_data <- reactive({
    flow_data %>% 
      filter(
        location_id %in% input$flow_station_select,
        datetime >= input$flow_daterange[1], 
        datetime <= input$flow_daterange[2], 
        parameter_value > 0) %>% 
      group_by(location_id, date = as_date(datetime)) %>% 
      summarise(
        daily_flow = round(mean(parameter_value, na.rm = TRUE), 0)
      )
  })
  
  
  selected_flow_data_sparklines <- reactive({
    flow_data %>% 
      filter(
        location_id %in% c("mss", "dlt"),
        datetime >= input$flow_daterange[1], 
        datetime <= input$flow_daterange[2], 
        parameter_value > 0) %>% 
      group_by(location_id, date = as_date(datetime)) %>% 
      summarise(
        daily_flow = round(mean(parameter_value, na.rm = TRUE), 0)
      )
  })
  
  selected_diversion_data <- reactive({
    diversion_data %>% 
      filter(draft_date >= input$flow_daterange[1], 
             draft_date <= input$flow_daterange[2])
  })
  
  selected_tcd_config_data <- reactive({
    # tcd_configs_data %>% 
    #   filter(starting_date >= "dummy", 
    #          starting_date <= "dummier") %>% 
    #   mutate(
    #     hover_text = paste0("From ", starting_date, " to ", ending_date,"<br>", 
    #                        " ", operation_value, " gate(s) open at ", tcd_operation)
    #   )
    
    data.frame() # hacks

    
  })
  
  select_add_year_flow <- reactive({
    this_daterange <- input$flow_daterange
    year(this_daterange[1]) <- as.numeric(input$flow_add_year)
    year(this_daterange[2]) <- as.numeric(input$flow_add_year)
    
    this_flow_data <- flow_data %>% 
      filter(datetime >= this_daterange[1], 
             datetime <= this_daterange[2],
             location_id %in% input$flow_station_select,
             parameter_value > 0) %>% 
      group_by(location_id, date = as_date(datetime)) %>% 
      summarise(
        daily_flow = round(mean(parameter_value, na.rm = TRUE), 0)
      )
      
    year(this_flow_data$date) <- year(input$flow_daterange[1])
    return(this_flow_data)
  })
  
  flow_spans_multiple_years <- reactive({
    year(input$flow_daterange[1]) != year(input$flow_daterange[2])
  })
  
  observe({
    if (flow_spans_multiple_years()) {
      updateSelectInput(session, "flow_add_year", selected = "None")
      shinyjs::disable("flow_add_year")
    } else {
      shinyjs::enable("flow_add_year")
    }
  })
  
  max_flow_value_in_range <-reactive({
    max(selected_flow_data()$daily_flow, na.rm = TRUE)
  })
  
  # TODO -- when tcd out of range the plot does not work, it should be that 
  # even when there is not TCD data, the plot should show flow time series
  output$flow_plot <- renderPlotly({
    validate(need(length(input$flow_station_select) != 0, "Select at least one station to plot"))
    validate(need(nrow(selected_flow_data()) > 0, "Combination of date range and stations did not return any data"))
    
    if (nrow(selected_tcd_config_data()) == 0) {
      base_plot <- selected_flow_data() %>% 
        plot_ly(x=~date, y=~daily_flow, color=~station_code_to_name_flows[location_id], 
                type='scatter', mode='lines', 
                colors = "Dark2",
                text = ~paste0("<b>",date, "</b><br><b>",
                               station_code_to_name_flows[location_id], "</b><br><b>", 
                               daily_flow, " cfs</b>"), 
                hoverinfo = "text", 
                line = list(width=3)) 
      
      if (input$flow_add_year != "None") {
        base_plot <- base_plot %>% 
          add_trace(data = select_add_year_flow(), 
                    x=~date, y=~daily_flow, color=~station_code_to_name_flows[location_id], 
                    type='scatter', mode='lines', 
                    colors = "Dark2",
                    name =~ paste0(input$flow_add_year),
                    text = ~paste0("<b>",input$flow_add_year,"</b><br><b>",
                                   station_code_to_name_flows[location_id], "</b><br><b>", 
                                   daily_flow, " cfs</b>"), 
                    hoverinfo = "text", 
                    line = list(width=2, dash="dot"))
      }
      
      if (input$show_diversions) {
        base_plot <- base_plot %>% 
          add_trace(data = selected_diversion_data(), 
                    name = "SRSC Upstream Diversions",
                    x=~draft_date, y=~actual_upstream, 
                    type='scatter', mode='lines', 
                    colors = "Dark2", 
                    inherit = FALSE)
      }
      
      base_plot %>% 
        layout(xaxis=list(title=""), 
               yaxis=list(title="flow (cfs)"), 
               showlegend = TRUE, 
               legend = list(orientation = 'h'))
    } else {
      base_plot <- selected_flow_data() %>% 
        plot_ly(x=~date, y=~daily_flow, color=~station_code_to_name_flows[location_id], 
                type='scatter', mode='lines', 
                colors = "Dark2",
                text = ~paste0("<b>",date, "</b><br><b>",
                               station_code_to_name_flows[location_id], "</b><br><b>", 
                               daily_flow, " cfs</b>"), 
                hoverinfo = "text", 
                line = list(width=3)) %>% 
        add_trace(data=selected_tcd_config_data(), 
                  x=~starting_date, y=max_flow_value_in_range(),
                  color = ~tcd_operation,
                  colors = "Pastel2",
                  inherit = FALSE, type='bar', legendgroup = "tcd_operations", 
                  visible = "legendonly", 
                  text = ~hover_text, 
                  hoverinfo = "text") 
      
      if (input$flow_add_year != "None") {
        base_plot <- base_plot %>% 
          add_trace(data = select_add_year_flow(),
                    x=~date, y=~daily_flow, color=~station_code_to_name_flows[location_id], 
                    type='scatter', mode='lines', 
                    colors = "Dark2",
                    text = ~paste0("<b>",date, "</b><br><b>",
                                   station_code_to_name_flows[location_id], "</b><br><b>", 
                                   daily_flow, " cfs</b>"), 
                    hoverinfo = "text", 
                    line = list(width=2, dash="dot"))
      }
      
      if (input$show_diversions) {
        base_plot <- base_plot %>% 
          add_trace(data = selected_diversion_data(), 
                    name = "SRSC Upstream Diversions",
                    x=~draft_date, y=~actual_upstream, 
                    type='scatter', mode='lines', 
                    colors = "Dark2", 
                    inherit = FALSE)
      }
      
      base_plot %>% 
        layout(xaxis=list(title=""), 
               yaxis=list(title="flow (cfs)"), 
               showlegend = TRUE)
    }
  })    
  
  
  # Data Paragraph -------------------------------------------------------------
  output$table_title <- renderUI({
    tags$h3(format(input$flow_daterange[2], "%B %d, %Y"))
  })
  
  output$mccloud_summmary_flow <- renderText({
    x <- selected_flow_data_sparklines() %>% 
      filter(location_id == "mss", date == input$flow_daterange[2]) %>% 
      pull(daily_flow)
    paste(x, "cfs")
  })
  
  output$delta_summary_flow <- renderText({
    x <- selected_flow_data_sparklines() %>% 
      filter(location_id == "dlt", date == input$flow_daterange[2]) %>% 
      pull(daily_flow)
    paste(x, "cfs")
  })
  
  output$mccloud_spark <- sparkline::renderSparkline({
    selected_flow_data_sparklines() %>% 
      filter(location_id == "mss", date >= (input$flow_daterange[2]-30)) %>% 
      pull(daily_flow) %>% 
      sparkline()
  })
  
  output$sac_river_spark <- sparkline::renderSparkline({
    selected_flow_data_sparklines() %>% 
      filter(location_id == "dlt", date >= (input$flow_daterange[2]-30)) %>% 
      pull(daily_flow) %>% 
      sparkline()
  })
  
  
  output$flow_tabular <- renderDataTable({
    selected_flow_data()
  })
}