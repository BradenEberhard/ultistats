#' Modify Point Start Time
#'
#' @description Modify the point start time based on the game quarter.
#'
#' @param game_quarter The current game quarter.
#' @param point_start_time The original point start time.
#' @return The modified point start time.
#' @noRd
modify_point_start_time <- function(game_quarter, point_start_time) {
  if (game_quarter <= 4) {
    return(720 - point_start_time)
  } else if (game_quarter == 5) {
    return(300 - point_start_time)
  } else if (game_quarter == 6) {
    return(252)
  } else {
    return(point_start_time)
  }
}

#' Interpolate Time Left
#'
#' @description Interpolate time left based on the number of rows in a group.
#'
#' @param data The game data for a given group.
#' @param next_start_time The start time of the next group.
#' @return The game data with the interpolated time left.
#' @noRd
interpolate_time_left <- function(data, next_start_time) {
  n <- nrow(data) # Number of rows in the group
  interpolated_times <- seq(data$point_start_time[1], next_start_time, length.out = n + 1)
  data$time_left <- interpolated_times[-length(interpolated_times)] # Exclude the last value (next point start time)
  return(data)
}
