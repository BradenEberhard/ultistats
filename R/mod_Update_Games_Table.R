#' API_Reload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Update_Games_Table_ui <- function(id) {
  ns <- NS(id)  # Namespace for module IDs
  tagList(
    actionButton(ns("update_games_table_button"), 
                 label = "Update Games Table", 
                 icon = icon("sync-alt"), 
                 class = "btn-outline-primary"),
    verbatimTextOutput(ns("api_output")),
  )
}
    
#' API_Reload Server Functions
#'
#' @noRd
mod_Update_Games_Table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # React to the button click
    observeEvent(input$update_games_table_button, {
      tryCatch({
        games_df <- fetch_games()
        write_db_table("games", games_df)
        output$api_output <- renderText({
          paste("Success! Number of games:", nrow(games_df))
        })
      }, error = function(e) {
        output$api_output <- renderText({
          paste("Failed to fetch data from API. Error: ", e$message)
        })
        message("An error occurred: ", e$message)
      })
    })
  })
}