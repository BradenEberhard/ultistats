#' GameEventsProxy Class
#'
#' This class extends the `Endpoint` class to provide specific functionality for
#' fetching and processing game event data from a given API.
#'
#' @description
#' The `GameEventsProxy` class is a subclass of `Endpoint`, designed to interact with the API endpoint for game events.
#' It includes methods for fetching throws, pulls, and penalties from a game based on its `gameID`.
#'
#' @import R6
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
GameEventsProxy <- R6::R6Class(
  "GameEventsProxy",
  inherit = Endpoint,
  
  public = list(
    base_path = NULL,
    current_request = NULL,
    
    #' @description
    #' Initializes the GameEventsProxy object, setting the base URL for game event API.
    initialize = function() {
      base_path <- get_golem_config("base_api_path")
      super$initialize(base_path)
    },
    
    #' @description
    #' Makes a GET request to the API and stores the response.
    #'
    #' @param endpoint The specific endpoint to query.
    get_request = function(endpoint) {
      self$current_request <- jsonlite::fromJSON(paste0(self$base_url, endpoint))$data
    },
    
    #' @description
    #' Fetches the throws from a game by gameID.
    #'
    #' @param gameID The ID of the game for which to fetch the throws.
    #' @return A tibble containing the throw events for the game.
    get_throws_from_id = function(gameID) {
      self$get_request(paste0('gameEvents?gameID=', gameID))
      
      rows <- list()
      process_event <- function(event, is_home_team) {
        if (event$type %in% c(18, 19, 20, 22, 23)) {
          turnover <- ifelse(event$type %in% c(20, 22, 23), 1, 0)
          event$turnover <- turnover
          if (!'receiverX' %in% names(event)) {
            event$receiverX <- event$turnoverX
            event$receiverY <- event$turnoverY
          }
          event$is_home_team <- is_home_team
          event$turnoverX <- NULL
          event$turnoverY <- NULL
          rows <<- append(rows, list(event))
        }
      }
      
      lapply(self$current_request$homeEvents, process_event, is_home_team = TRUE)
      lapply(self$current_request$awayEvents, process_event, is_home_team = FALSE)
      
      return(dplyr::bind_rows(rows) %>% dplyr::select(-turnoverX, -turnoverY))
    },
    
    #' @description
    #' Fetches the pulls from a game by gameID.
    #'
    #' @param gameID The ID of the game for which to fetch the pulls.
    #' @return A tibble containing the pull events for the game.
    get_pulls_from_id = function(gameID) {
      self$get_request(paste0('gameEvents?gameID=', gameID))
      
      rows <- list()
      process_pull_event <- function(event, is_home_team) {
        pull_row <- list()
        if (event$type == 7) {
          pull_row$pullX <- event$pullX
          pull_row$pullY <- event$pullY
          pull_row$pullMs <- event$pullMs
          pull_row$puller <- event$puller
          pull_row$in_bounds <- TRUE
          pull_row$is_home_team <- is_home_team
          rows <<- append(rows, list(pull_row))
        }
        if (event$type == 8) {
          pull_row$pullX <- NULL
          pull_row$pullY <- NULL
          pull_row$pullMs <- NULL
          pull_row$puller <- event$puller
          pull_row$in_bounds <- FALSE
          pull_row$is_home_team <- is_home_team
          rows <<- append(rows, list(pull_row))
        }
      }
      
      lapply(self$current_request$homeEvents, process_pull_event, is_home_team = TRUE)
      lapply(self$current_request$awayEvents, process_pull_event, is_home_team = FALSE)
      
      return(dplyr::bind_rows(rows))
    },
    
    #' @description
    #' Fetches the penalties from a game by gameID.
    #'
    #' @param gameID The ID of the game for which to fetch the penalties.
    #' @return A vector containing the count of home and away team penalties.
    get_penalties_from_id = function(gameID) {
      self$get_request(paste0('gameEvents?gameID=', gameID))
      
      home_penalties <- sum(sapply(self$current_request$homeEvents, function(event) event$type == 16))
      away_penalties <- sum(sapply(self$current_request$awayEvents, function(event) event$type == 16))
      
      return(c(home_penalties, away_penalties))
    }
  )
)

#' Fetch Game Data from the API and Store in Database
#'
#' @description
#' This function fetches game data from a specified API endpoint and stores it in a local SQLite database.
#' It makes an HTTP request to the API, parses the data, and writes the relevant game data to the database table.
#'
#' @return A character string indicating whether the operation was successful or failed.
#' @export
#'

fetch_games <- function(endpoint = "games?date=2021:") {
  # Retrieve base URL and database path from the configuration
  base_url <- get_golem_config("base_api_path")
  api_url <- paste0(base_url, endpoint)
  
  # Use httr to fetch data from the API
  response <- httr::GET(api_url)
  
  if (httr::status_code(response) == 200) {
    data <- httr::content(response, as = "parsed")
    
    # Parse the data into a structured format
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
    
    # Combine the list of game data into a data frame
    games_df <- do.call(rbind, lapply(games_data, as.data.frame))    
    return(games_df)
  } else {
    stop("Failed to fetch data from API.")
  }
}
