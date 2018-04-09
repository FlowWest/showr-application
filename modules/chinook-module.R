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
             selectInput(ns("chinook_year_selected"), label = "Select A Year", 
                         choices = 2010:2018, selected = 2017)), 
      column(width = 3, 
             selectInput(ns("chinook_reach_selected"), label = "Select a Reach", 
                         choices = c("Test")))
    ),
    fluidRow(
      column(width = 12, class = "col-md-5", 
             DT::dataTableOutput(ns("chinook_table")), 
             leafletOutput(ns("chinook_map"))), 
      column(width = 12, class = "col-md-7", 
             plotlyOutput(ns("chinook_plot")))
    )
  )
}

winter_run_server <- function(input, output, session, g_date) {
  
  selected_temp_data <- reactive({
    temp_data %>% 
      filter(year(datetime) >= as.numeric(input$chinook_year_selected)) %>% 
      group_by(location_id, date = as_date(datetime)) %>% 
      summarise(
        daily_mean = mean(parameter_value, na.rm = TRUE)
      )
  })
  
  selected_redds <- reactive({
    redd_data %>% 
      filter(year(date) == as.numeric(input$chinook_year_selected),
             counts > 0) %>% 
      rowwise() %>% 
      do(tibble(
        spawn_date = .$date, 
        spawn_location = .$location, 
        spawn_total = .$counts, 
        cdec_location = redd_cdec_lookup[.$location], 
        estimated_emergence = estimate_emergence(.$date, .$location)
      )) %>% ungroup() %>%
      left_join(selected_temp_data(), by = c("spawn_date" = "date", "cdec_location" = "location_id")) %>% 
      mutate(spawn_id = as.character(group_indices(., spawn_date, spawn_location))) %>% 
      ungroup() %>% 
      rowwise() %>% 
      do(
        tibble(
          spawn_id = .$spawn_id, 
          spawn_date = .$spawn_date,
          spawn_location = .$spawn_location,
          spawn_total = .$spawn_total,
          cdec_location = .$cdec_location, 
          estimated_emergence = .$estimated_emergence, 
          daily_temp = {
            pull(filter(selected_temp_data(), location_id == .$cdec_location, 
                        date >= .$spawn_date, date <= .$estimated_emergence), daily_mean)
          }, 
          temp_date = {
            pull(filter(selected_temp_data(), location_id == .$cdec_location, 
                        date >= .$spawn_date, date <= .$estimated_emergence), date)
          }
        ) 
      ) %>% ungroup() %>% 
      mutate(expired = as.integer(estimated_emergence < g_date()))
  })
  
  selected_redd_id_from_table <- reactive({
    if (is.null(input$chinook_table_rows_selected)) return(NULL)
    idx <- redds_for_table()[input$chinook_table_rows_selected, ]$`Spawn ID`
    return(idx)
  })
  
  
  selected_redd_spawn <- reactive({
    selected_redds() %>% 
      filter(spawn_id %in% selected_redd_id_from_table(), 
             spawn_date == temp_date) %>% 
      select(spawn_id, spawn_date, spawn_total, daily_temp, spawn_location, 
             estimated_emergence)
  })
  
  selected_redd_emergence <- reactive({
    selected_redds() %>% 
      filter(spawn_id %in% selected_redd_id_from_table(), 
             estimated_emergence == temp_date) %>% 
      select(spawn_id, esimated_emergence, spawn_total, daily_temp, spawn_location)
  })
  selected_redd_temps <- reactive({
    selected_redds() %>% 
      filter(spawn_id %in% selected_redd_id_from_table())
  })
  
  redds_for_table <- reactive({
    selected_redds() %>% 
      distinct(spawn_id, .keep_all = TRUE) %>% 
      select(`Spawn ID` = spawn_id,
        `Spawn Date` = spawn_date, 
             `Location` = spawn_location,
             `Total Redds` = spawn_total, 
             `Estimated Emergence` = estimated_emergence, 
             Expired = expired)
  })
  
  output$chinook_table <- DT::renderDataTable({
    redds_for_table() %>%
      DT::datatable(data=.,
                    extensions = c("Scroller"),
                    rownames = FALSE,
                    options = list(
                      order = list(list(0, 'desc')), 
                      dom = 't',
                      deferRender = TRUE,
                      scrollY = 300,
                      scroller = TRUE
                    )) %>% 
    DT::formatStyle(
      columns = "Expired",
      target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', '#cccccc'))
    ) 
  }, server = FALSE)
  
  output$chinook_plot <- renderPlotly({
    validate(need(
      nrow(selected_redd_spawn()) > 0, "Select a redd to view its temperature outlook"
    ))
    
    p <- plot_ly() %>% 
      add_markers(data=selected_redd_spawn(),
                  x=~spawn_date, 
                  y=~daily_temp, 
                  color=~spawn_location, 
                  showlegend = FALSE, 
                  text = ~paste0(
                    "Spawn Date: ", spawn_date, "<br>",
                    "Location: ", spawn_location, "<br>",
                    "Total: ", spawn_total, "<br>",
                    "Estimated Emergence: ", estimated_emergence
                  ), 
                  hoverinfo = "text") %>% 
      add_lines(data=selected_redd_temps(), 
                x=~temp_date, 
                y=~daily_temp, 
                color=~spawn_location)
    
    validate(need(
      !is.null(p), "Select a redd to view its temperature profile"
    ))
    p %>% layout(yaxis=list(domain=c(40,70)))
  })
  
}