#' API_Reload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_API_Reload_ui <- function(id) {
  ns <- NS(id)  # Namespace for module IDs
  tagList(
    actionButton(ns("reload_button"), 
    label = "Reload API", 
    icon = icon("sync-alt"),  # Add the refresh icon
    class = "btn-outline-primary"),
    verbatimTextOutput(ns("api_output")),
    mod_Get_Throws_ui("Get_Throws_1")
  )
}
    
#' API_Reload Server Functions
#'
#' @noRd 
mod_API_Reload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Function to fetch data from the API
    fetch_games <- function() {
      base_url <- get_golem_config("base_api_path")
      db_path <- get_golem_config("db_path")
      endpoint <- "games?date=2021:"
      api_url <- paste0(base_url, endpoint)
      games_tablename <- "games"
      
      # Use httr to fetch data
      response <- httr::GET(api_url)
      
      if (httr::status_code(response) == 200) {
        data <- httr::content(response, as = "parsed")
        conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)

        games_data <- lapply(data$data, function(x) {
          list(
            gameID = x$gameID,
            awayTeamID = x$awayTeamID,
            homeTeamID = x$homeTeamID,
            awayScore = x$awayScore,
            homeScore = x$homeScore,
            status = x$status,
            startTimestamp = format(lubridate::ymd_hms(gsub("Z$", "", x$startTimestamp), tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
            startTimezone = x$startTimezone,
            updateTimestamp = format(lubridate::ymd_hms(gsub("Z$", "", x$updateTimestamp), tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
            week = x$week
          )
        })
        games_df <- do.call(rbind, lapply(games_data, as.data.frame))
        DBI::dbWriteTable(conn = conn, name = games_tablename, value = games_df, row.names = FALSE, overwrite = TRUE)
        return(paste("Success! New table has", nrow(games_df), "games."))
      } else {
        return("Failed to fetch data from API.")
      }
    }
    
    # React to the button click
    observeEvent(input$reload_button, {
      result_str <- fetch_games()
      if (!is.null(result_str)) {
        # Display fetched game IDs in UI
        output$api_output <- renderText({
          paste(result_str)
        })
      } else {
        output$api_output <- renderText({
          "Failed to fetch data from API."
        })
      }
    })
    mod_Get_Throws_server("Get_Throws_1")
  })
}