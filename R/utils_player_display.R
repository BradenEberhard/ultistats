#' Retrieve and Process Player Throws
#'
#' Fetches throw data for a player from the database and processes it to add adjusted angles and year.
#'
#' @param db_path Path to the database file.
#' @param player_id Player's ID.
#' @return A data frame with processed throw data, including `adjusted_angle` and `year`.
#' @examples
#' \dontrun{
#'   get_filtered_throws("path/to/database.sqlite", 12345)
#' }
#' @export
get_filtered_throws <- function(db_path, player_id) {
  conn <- open_db_connection(db_path)
  player_throws <- get_player_throws(conn, player_id)
  close_db_connection(conn)
  
  player_throws$adjusted_angle <- (as.integer(player_throws$throw_angle) + 90) %% 360 - 180
  player_throws$year <- substr(player_throws$gameID, 1, 4)
  
  return(player_throws)
}

#' Filter Player Stats by Year
#'
#' Filters player statistics for a selected year or returns career statistics.
#'
#' @param all_player_stats A data frame containing player statistics with a `year` column.
#' @param year_selector A string indicating the selected year or "Career" for all stats.
#' @return A data frame with the filtered player statistics.
#' @examples
#' \dontrun{
#'   filtered_stats <- filter_stats_by_year(all_player_stats, "2023")
#'   filtered_stats <- filter_stats_by_year(all_player_stats, "Career")
#' }
#' @export
filter_stats_by_year <- function(all_player_stats, year_selector) {
  if (year_selector == "Career") {
    return(all_player_stats)
  } else {
    return(all_player_stats[all_player_stats$year == year_selector, ])
  }
}



convert_to_metric_df <- function(df) {
  metrics <- c("goals", "assists", "blocks", "completions", "hockeyAssists", 
               "yardsThrown", "yardsReceived", "completion_percentage", "cpoe", "xcp")
  
  percentiles <- paste0(metrics, "_percentile")
  
  # Create a data frame for the result
  metric_df <- data.frame(
    metric = metrics,
    value = as.numeric(df[metrics]),
    percentile = as.numeric(df[percentiles]),
    stringsAsFactors = FALSE
  )
  
  return(metric_df)
}
