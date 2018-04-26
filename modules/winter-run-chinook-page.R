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
                        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam ac scelerisque quam. Fusce eget risus eros. Cras elementum nulla velit, in lacinia mauris euismod ut. Praesent ut semper nunc. Cras porttitor elit sem, id molestie purus fringilla nec. Aliquam vehicula lacinia aliquam. Curabitur et leo elit. Sed egestas massa sit amet turpis faucibus blandit. Curabitur vel efficitur tellus, accumsan dapibus diam. Vivamus tincidunt leo vel placerat facilisis. Duis id augue ac dui posuere hendrerit."
                      ),
                      tags$h3("Download Data in View"),
                      downloadButton(ns("download_wr_data")),
                      tags$h5("Note: the final aerial redd survey was conducted on 08/16/2017"),
                      tags$h5("Data source: CDFW provided through calfish.org"), 
                      tags$h5("Update schedule: data is updated by CDFW on a weekly basis") 
             )),
      # main interface
      column(width = 12, class = "col-md-9", 
             fluidRow(
               # Controls
               column(width = 12, class="col-md-2", 
                      selectInput(ns("wr_select_year"), label = "Select a Year", 
                                  choices = 2010:2017, selected = 2017)),
               column(width = 12, class="col-md-3", 
                      checkboxInput(ns("wr_show_temp_danger"), 
                                    label = "Show At Risk Redds"))),
             fluidRow(
               # plot
               column(width = 12, class="col-md-9", 
                      plotlyOutput(ns("winter_run_plot"))), 
               column(width = 12, class = "col-md-3", 
                      tableOutput(ns("wr_table"))))
      )
    )
  )
} 

# TODO pick either switch or ifelse from now on
winter_run_server <- function(input, output, session, g_date) {
  
  rd_yr <- reactive({
    if (input$wr_show_temp_danger) {
      rd %>% 
        filter(year(date) == input$wr_select_year, daily_mean > 56) %>%  
        group_by(location, date) %>% 
        summarise(total = sum(counts)) %>% 
        ungroup()
    } else {
      rd %>% 
        filter(year(date) == input$wr_select_year) %>%  
        group_by(location, date) %>% 
        summarise(total = sum(counts)) %>% 
        ungroup()
    }
    
  })
  
  output$winter_run_plot <- renderPlotly({
    validate(errorClass = 'no-redds-alert', need(
      nrow(rd_yr()) > 0, "No redds at risk."
    ))
    
    rd_yr() %>% 
      plot_ly(x = ~date, y = ~total, color = ~location, type='bar') %>% 
      layout(legend = list(orientation = 'h'), showlegend = TRUE, 
             xaxis = list(title = ""), yaxis = list(title = 'total redds'))
  })
  
}