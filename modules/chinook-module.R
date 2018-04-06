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
      column(width = 12, class = "col-md-4", 
             DT::dataTableOutput(ns("chinook_table")), 
             leafletOutput(ns("chinook_map"))), 
      column(width = 12, class = "col-md-8")
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
      ) %>% ungroup()
  })
  
  output$chinook_table <- DT::renderDataTable({
    selected_redds() %>% 
      distinct(spawn_id, .keep_all = TRUE) %>% 
      select(`Spawn Date` = spawn_date, 
             `Location` = spawn_location,
             `Total Redds` = spawn_total, 
             `Estimated Emergence` = estimated_emergence)
    },
    extensions = c("Scroller"),
    rownames = FALSE,
    options = list(
      order = list(list(0, 'desc')), 
      dom = 't',
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE
    ))
  
}