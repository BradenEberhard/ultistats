#' Get_Throws UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import dplyr
#' @importFrom shiny NS tagList 
mod_Update_Throws_Table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Date range input for selecting start and end date
    dateRangeInput(
      ns("date_range"),
      label = "Select Date Range",
      start = "2021-01-01",  # Default start date
      end = Sys.Date(),      # Default end date (current date)
      min = "2021-01-01",    # Minimum allowed date
      max = Sys.Date()       # Maximum allowed date (current date)
    ),
    
    # Button to trigger the fetch and display of game IDs
    actionButton(ns("update_throws_table_button"), 
                 label = "Update Throws Table", 
                 icon = icon("sync-alt"), 
                 class = "btn-outline-primary"),
    
    # Output to display the game IDs
    verbatimTextOutput(ns("game_data_output"))
  )
}

#' Get_Throws Server Functions
#'
#' @noRd 
mod_Update_Throws_Table_server <- function(id) {
  moduleServer(id, function(input, output, session) {   
    game_events_proxy <- GameEventsProxy$new()

    observeEvent(input$update_throws_table_button, {
      req(input$date_range)  # Make sure date range input is available
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]

      game_ids <- get_game_ids(start_date, end_date)
      output$game_data_output <- renderPrint({
        paste0("Successfully processed ", length(game_ids), " games.")
      })
      
      # Start the progress bar and loop through game ids
      withProgress(message = "Processing Game IDs", value = 0, {
        for (i in seq_along(game_ids)) {
          current_game_id <- game_ids[i]
          game_data <- add_time_left(current_game_id, game_events_proxy)
          game_data$current_line <- sapply(game_data$current_line, function(x) paste(x, collapse = '|'))
          game_data$update_date <- Sys.time()

          update_table("throws", game_data, current_game_id = current_game_id)
          incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
        }
      })
    })
  })
}
