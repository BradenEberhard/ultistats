#' Fetch Player Data from the API and Store in Database
#'
#' @description
#' This function fetches player data from a specified API endpoint and stores it in a local SQLite database.
#' It makes an HTTP request to the API, parses the data, and writes the relevant player data to the database table.
#'
#' @return A character string indicating whether the operation was successful or failed.
#' @export
fetch_players <- function(base_url, years=NULL) {
  if (is.null(years)) {
    years=seq(2012, as.numeric(format(Sys.Date(), "%Y")))
  }
  endpoint <- paste0("players?years=", paste(years, collapse = ","))
  api_url <- paste0(base_url, endpoint)

  # Use httr to fetch data from the API
  response <- httr::GET(api_url)
  if (httr::status_code(response) == 200) {
    response_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    players_table <- response_data$data
    return(players_table)
  } else {
    stop("Failed to fetch data from API.")
  }
}


#' Fetch Game Data from the API and Store in Database
#'
#' @description
#' This function fetches game data from a specified API endpoint and stores it in a local SQLite database.
#' It makes an HTTP request to the API, parses the data, and writes the relevant game data to the database table.
#'
#' @return A character string indicating whether the operation was successful or failed.
#' @export
fetch_games <- function(base_url, endpoint = "games?date=2021:") {
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
    
    games_df <- do.call(rbind, lapply(games_data, as.data.frame))    
    return(games_df)
  } else {
    stop("Failed to fetch data from API.")
  }
}


#' Fetch Game Data from the API and Store in Database
#'
#' @description
#' This function fetches player data from a specified API endpoint
#'
#' @return A character string indicating whether the operation was successful or failed.
#' 
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @import dplyr
#' @export
fetch_teams <- function(base_url, years=NULL, teamID=NULL) {
  if (is.null(years)) {
    years=seq(2012, as.numeric(format(Sys.Date(), "%Y")))
  }
  endpoint <- paste0("teams?years=", paste(years, collapse = ","))
  api_url <- paste0(base_url, endpoint)

  # Use httr to fetch data from the API
  response <- httr::GET(api_url)
  if (httr::status_code(response) == 200) {
    response_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    teams_table <- response_data$data
    return(teams_table)
  } else {
    stop("Failed to fetch data from API.")
  }
}

fetch_game <- function(base_url, gameID) {
  endpoint <- paste0("gameevents?gameID=",gameID)
  api_url <- paste0(base_url, endpoint)
  response <- httr::GET(api_url)
  if (httr::status_code(response) == 200) {
    response_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    game_data <- response_data$data
    return(game_data)
  } else {
    stop("Failed to fetch data from API.")
  }
}


#' Fetch Player Stats Data from the API and Store in Database
#'
#' @description
#' This function fetches player data from a specified API endpoint and stores it in a local SQLite database.
#' It makes an HTTP request to the API, parses the data, and writes the relevant player data to the database table.
#'
#' @importFrom tidyr unnest
#' @return A character string indicating whether the operation was successful or failed.
#' @export
fetch_player_stats <- function(base_url, players) {
  split_into_batches <- function(players, batch_size = 100) {
    split(players, ceiling(seq_along(players) / batch_size))
  }

  # Split players into batches
  player_batches <- split_into_batches(players, 100)

  # Initialize an empty list to store results
  all_player_stats <- list()

  # Iterate over batches
  for (batch in player_batches) {
    endpoint <- paste0("playerstats?playerIDs=", paste(batch, collapse = ","))
    api_url <- paste0(base_url, endpoint)
    
    # Fetch data from the API
    response <- httr::GET(api_url)
    
    if (httr::status_code(response) == 200) {
      response_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      player_stats_table <- response_data$data
      player_stats_table <- tidyr::unnest(player_stats_table, cols = c(player))
      all_player_stats <- append(all_player_stats, list(player_stats_table))
    } else {
      warning("Failed to fetch data for batch: ", paste(batch, collapse = ","))
    }
  }
  do.call(rbind, all_player_stats)
}