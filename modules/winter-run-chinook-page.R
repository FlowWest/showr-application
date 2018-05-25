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
                      plotlyOutput(ns("winter_run_plot"), height = "500px")), 
               column(width = 12, class = "col-md-3", 
                      tags$h3('Number of Redds'),
                      tableOutput(ns("wr_table"))))
      )
    )
  )
} 

# TODO pick either switch or ifelse from now on
winter_run_server <- function(input, output, session, g_date) {


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