#' Get Modified Game Data
#'
#' @description Fetch the game data based on the given game ID and return the modified dataframe.
#' @param game_id A character string representing the unique game identifier.
#' @return A modified dataframe with interpolated time left and adjusted point start times.
#' 
#' @import dplyr
#' @noRd

add_time_left <- function(game_id, game_events_proxy) {
  # Fetch game events data
  game_events <- GameEvents$new(game_id, game_events_proxy)
  game_events$process_game_events()
  game_df <- game_events$get_events_df(gameID = TRUE, home_team = TRUE, away_team = TRUE)
  
  # Modify point start time for the entire dataframe
  game_df <- game_df %>%
    group_by(home_team_score, away_team_score, game_quarter) %>%
    mutate(
      point_start_time = ifelse(
        all(is.na(point_start_time)),  # If all values in the group are NA
        0,  # Impute 0 if all values are NA
        ifelse(
          is.na(point_start_time),  # If individual values are NA
          max(point_start_time, na.rm = TRUE),  # Impute with the max value within the group
          point_start_time  # Keep the original value if not NA
        )
      )
    ) %>%
    ungroup() %>%  # Ungroup after mutation
    mutate(
      point_start_time = mapply(modify_point_start_time, game_quarter, point_start_time)
    )
  # Group data by home_team_score, away_team_score, and game_quarter
  game_df <- game_df %>%
    group_by(home_team_score, away_team_score, game_quarter) %>%
    arrange(game_quarter, home_team_score, away_team_score, .by_group = TRUE) %>%
    group_split() %>%
    { 
      points <- list() # Store processed groups
      for (i in seq_along(.)) {
        current_group <- .[[i]]
        
        # Get the current group's start time and game quarter
        current_start_time <- current_group$point_start_time[1]
        current_quarter <- current_group$game_quarter[1]
        
        # Get the next group's start time and game quarter if it exists
        if (i < length(.)) {
          next_group <- .[[i + 1]]
          next_start_time <- next_group$point_start_time[1]
          next_quarter <- next_group$game_quarter[1]
          
          # If the next group is in a different quarter, set next_start_time to 0
          if (next_quarter != current_quarter) {
            next_start_time <- 0
          } else if (next_start_time >= current_start_time) {
            # Correct the next group's start time if invalid
            .[[i + 1]]$point_start_time <- current_start_time - 1
            next_start_time <- current_start_time - 1
          }
        } else {
          next_start_time <- 0 # Arbitrary increment for the last group
        }
        
        # Interpolate time left for the current group
        processed_group <- interpolate_time_left(current_group, next_start_time)
        points[[i]] <- processed_group
      } 
      
      # Combine the processed groups into a single data frame
      bind_rows(points)
    } %>%
    mutate(time_left = ifelse(game_quarter == 6, 252, time_left))
  
  return(game_df)
}

#' Get Game IDs Based on Date Range
#'
#' @description
#' This function queries the API to retrieve a list of game IDs based on a specified date range.
#' It constructs the URL for the API request using the provided start and end dates, sends the request,
#' and returns the game IDs from the response.
#'
#' @param start_date A string representing the start date of the date range (default is "2021-01-01").
#'                   The date should be in the format "YYYY-MM-DD".
#' @param end_date A string representing the end date of the date range (default is the current date).
#'                 The date should be in the format "YYYY-MM-DD".
#'
#' @return A character vector containing the game IDs.
#'         If the request is unsuccessful, an error is raised.
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#' @export
get_game_ids <- function(start_date = "2021-01-01", end_date = Sys.Date()) {
  # Construct the date range query parameter
  date_range <- paste(start_date, end_date, sep = ":")
  
  # Get the base API URL from the configuration
  base_url <- get_golem_config("base_api_path")
  
  # Construct the endpoint with the date range
  endpoint <- paste0("games?date=", date_range)
  
  # Send the GET request to the API
  response <- httr::GET(paste0(base_url, endpoint))
  
  # Check if the response status is OK (200)
  if (response$status == 200) {
    # Parse the response content and extract game IDs
    data <- httr::content(response, "parsed", type = "application/json")$data
    game_ids <- sapply(data, function(x) x$gameID)
    return(game_ids)
  } else {
    # Raise an error if the request fails
    stop("Failed to retrieve data: ", httr::status_code(response))
  }
}
