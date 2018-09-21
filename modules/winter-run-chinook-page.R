winter_run_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # winter run sidebar
      column(width = 12, class = "col-md-3", 
             tags$div(class = "wr-sidebar", 
                      tags$h2("Winter Run Emergence"),
                      tags$hr(),
                      tags$p(
                        "Sacramento River",
                        tags$a("Winter Run Chinook", href="https://www.wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Winter-run", target="_blank"),
                        "Salmon were listed as endangered under the California Endangered Species Act (CESA) and threatened under the Federal Endangered Species Act in 1989. Winter Run migrate between November and early August, and spawn in the upper mainstem Sacramento River from mid-April through August. Peak spawning typically occurs in June and July. The plot to the right shows the number and location (by river reach) of Winter Run redds mapped in the Upper Sacramento River. You can select “Show At Risk Redds” to view the redds potentially impacted by water temperatures exceeding the 56F compliance threshold."
                      ),
                      tags$hr(),
                      tags$h4("Download Data in View"),
                      downloadButton(ns("download_wr_data"), class="subpage-buttons"),
                      tags$br(),
                      tags$br(),
                      bookmarkButton(id=ns("chinook_bookmark"), "Share page", 
                                     class="subpage-buttons"),
                      # tags$h5("Note: the final aerial redd survey was conducted on 08/16/2017"),
                      tags$h5("Data source: CDFW provided through calfish.org"), 
                      tags$h5("Update schedule: data is updated by CDFW on a weekly basis") 
             )),
      # main interface
      column(width = 12, class = "col-md-9", 
             fluidRow(
               # Controls
               column(width = 12, class="col-md-2", 
                      selectInput(ns("wr_select_year"), label = "Select a Year", 
                                  choices = 2010:2018, selected = 2018)),
               column(width = 12, class="col-md-4", 
                      tags$div(style = "display:inline-block",
                               checkboxInput(ns("wr_show_temp_danger"), 
                                             label = "Show at risk redds"), 
                               checkboxInput(ns("wr_show_spawn_dates"), 
                                             label = "Plot by spawn dates"))), 
               column(width = 12, class = "col-md-6", 
                      tags$div(style="display:inline-block", 
                               uiOutput(ns("wr_select_spawn_data_ui"))))),
             fluidRow(
               # plot
               column(width = 12, class="col-md-9", 
                      plotlyOutput(ns("winter_run_plot"), height = "500px"), 
                      plotlyOutput(ns("winter_run_temp_plot"), height = "200px")), 
               column(width = 12, class = "col-md-3", 
                      tags$h3('Number of Redds'),
                      tableOutput(ns("wr_table")), 
                      leafletOutput(ns("wr_redd_reaches_map"))))
      )
    )
  )
} 

# TODO pick either switch or ifelse from now on
winter_run_server <- function(input, output, session, g_date) {
  ns <- session$ns
  
  dataModal <- function(...) {
    modalDialog(
      title = "Upper Sacramento Redd Reaches",
      radioButtons(ns("reaches_to_show"),
                   label = "Show",
                   choices = c("Reaches wtih Redds", "All Reaches"), 
                   inline = TRUE),
      leafletOutput(ns("wr_redd_reaches_map")),
      footer = tagList(
        modalButton("Close")
      ), ...
    )
  }
  
  output$download_wr_data <- downloadHandler(
    filename = function() {
      "showr-aerial-redd-survey.csv"
    },
    content = function(file){
      write.csv(redd_data, file, row.names = FALSE)
      
    }
  )
  
  redd_counts_today <- reactive({
    rd_yr() %>% 
      filter(date == lubridate::today(tzone="US/Pacific"))
  })
  
  output$wr_select_spawn_data_ui <- renderUI({
    spawn_choices <- as.Date.character(pull(distinct(spawn_dates_in_year(), seed_day)))
    if (input$wr_show_spawn_dates) {
      selectInput(ns("wr_select_spawn_date"), "Select a spawn date", 
                  choices = list("All" = "all", "Spawn Date" = spawn_choices))
    } else {
      return(NULL)
    }
  })
  
  # not working correctly
  output$wr_table <- renderTable(
    rd %>% 
      filter(year(date) == input$wr_select_year, counts > 0) %>% 
      mutate(temp_exceed = daily_mean > 56) %>% 
      group_by(redd_id) %>% 
      summarise(Total = max(counts), `Temperature Threatened` = max(temp_exceed) * max(counts)) %>% 
      ungroup() %>% 
      summarise(Total = as.integer(sum(Total)), 
                `Temperature Threatened` = as.integer(sum(`Temperature Threatened`, na.rm = TRUE))))
  
  
  
  rd_yr <- reactive({
    if (input$wr_show_temp_danger) {
      rd %>% 
        filter(year(date) == input$wr_select_year, daily_mean > 56) %>%  
        group_by(location, seed_day, date) %>% 
        summarise(total = sum(counts)) %>% 
        ungroup()
    } else {
      rd %>% 
        filter(year(date) == input$wr_select_year) %>%  
        group_by(location, seed_day, date) %>% 
        summarise(total = sum(counts)) %>% 
        ungroup()
    }
  })
  
  spawn_dates_in_year <- reactive({
    rd %>% 
      filter(year(date) == input$wr_select_year) %>% 
      distinct(seed_day, location, .keep_all = TRUE)
    
  })
  
  wr_temp_data <- reactive({
    d <- daily_temps %>%
      filter(year(date) == input$wr_select_year, cdec_gage %in% c("kwk", "ccr", "bsf"))
    
    if (is.null(daterange_from_presence_plot()$`xaxis.range[0]`)) {
      return(d)
    } else {
      return(d %>% filter(
        date >= as_date(daterange_from_presence_plot()$`xaxis.range[0]`), 
        date <= as_date(daterange_from_presence_plot()$`xaxis.range[1]`)
      ))
    }
  })
  
  output$winter_run_temp_plot <- renderPlotly({
    p <- plot_ly()  %>% 
      add_lines(data=wr_temp_data(), x=~date, y=~daily_mean, 
                color=~cdec_gage, inherit = FALSE, colors="Accent")
    
    if (input$wr_select_year == 2018) {
      p <- p %>% 
        add_segments(
          data=filter(wr_temp_data(), date==Sys.Date()), 
          x=Sys.Date(), 
          xend=Sys.Date(),
          y=~min(daily_mean)-5, 
          yend=~max(daily_mean)+5, showlegend=FALSE,
          line=list(color="#7c247f", width=4))
      
    }
    
     p %>%  
      layout(xaxis=list(title=""), yaxis=list(title="daily mean temperature (F)"))
  })
  
  daterange_from_presence_plot <- reactive({
    plotly::event_data("plotly_relayout", source="redd_presence_plot")
  })
  
  observe({
    cat(input$showrapp)
    cat("the first value of the range:", ifelse(
      is.null(daterange_from_presence_plot()$`xaxis.range[0]`), "its a null!",
        as_date(daterange_from_presence_plot()$`xaxis.range[0]`)), "\n")
    cat("the second value of the range:", daterange_from_presence_plot()$`xaxis.range[1]`, "\n")
    
  })
  
  
  # this whole thing needs some refactoring
  output$winter_run_plot <- renderPlotly({
    shiny::validate(errorClass = 'no-redds-alert', need(
      nrow(rd_yr()) > 0, "No redds at risk."
    ))
    
    p <- rd_yr() %>%
      plot_ly(source="redd_presence_plot") 
    
    if (input$wr_select_year == 2018) {
      p <- p %>% add_segments(
        data=filter(rd_yr(), date == Sys.Date()),
        x=~date, xend = ~date, y = 0, yend = ~sum(total),
        line=list(color="#7c247f", width=4), name="today", showlegend=FALSE, 
        hoverinfo = "text", text="Today!")  
    }
    p <- p %>% add_bars(data=rd_yr(), 
                        x = ~date, y = ~total, color = ~location, 
                        text = ~paste0(date, "<br>", 
                                       location, "<br>", 
                                       total), 
                        hoverinfo = "text", 
                        key = ~location) %>%
      layout(legend = list(orientation = 'h'), showlegend = TRUE, 
             xaxis = list(title = ""), yaxis = list(title = 'total redds'), 
             barmode='stack')
    
    if (input$wr_show_spawn_dates) {
      req(input$wr_select_spawn_date)
      if (input$wr_select_spawn_date == "all") {
        p <- rd_yr() %>%    
          plot_ly(x = ~date, y = ~total, color = ~as.character(seed_day), type='bar', 
                  text = ~paste0(date, "<br>", 
                                 location, "<br>", 
                                 total), 
                  hoverinfo = "text", colors = "Dark2")  %>%
          layout(legend = list(orientation = 'h'), showlegend = TRUE, 
                 xaxis = list(title = ""), yaxis = list(title = 'total redds'), 
                 barmode='stack')
      } else {
        p <- redd_data %>% 
          filter(date == input$wr_select_spawn_date, 
                 counts > 0) %>% 
          plot_ly(x=~location, y=~counts, type='bar', color=~location)
      }
      
    }
    
    return(p)
    
  })
  
  # Bookmarking this page ----------
  setBookmarkExclude(c("flow_page_bookmark", "temp_page_bookmark", "chinook_bookmark"))
  
  observeEvent(input$chinook_bookmark, {
    session$doBookmark()
  })
  
  onBookmark(function(state) {
    state$values$wr_year_hash <- digest::digest(input$wr_select_year, "md5")
  })
  
  reaches_to_show_in_map <- reactive({
    subset(redd_reach, Reach %in% pull(distinct(rd_yr(), location)))
  })
  
  output$wr_redd_reaches_map <- renderLeaflet({
    
    shiny::validate(errorClass = 'no-redds-alert', need(
      nrow(rd_yr()) > 0, "No redds at risk."
    ))
    
    leaflet(reaches_to_show_in_map()) %>%
      addTiles() %>% 
      addPolylines(label=~Reach, weight = 5,
                   color = "black",
                   highlight = highlightOptions(
                     weight = 7,
                     color = "red",
                     fillOpacity = 0.7,
                     bringToFront = TRUE) )
  })
  
  hovered_reach <- reactive({
    subset(redd_reach, Reach == event_data("plotly_hover", source = "redd_presence_plot")$key)
  })
  
  observe({
    leafletProxy("wr_redd_reaches_map", data=hovered_reach()) %>% 
      clearGroup("hovered_reach") %>% 
      addPolylines(
        # color = "#7a3d69", 
        group = "hovered_reach", 
        stroke = TRUE,
        weight = 12, 
        fillOpacity = .9, 
        fillColor = "#7a3d69")
    
    
  })
  
  
}