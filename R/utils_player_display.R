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

get_HTML <- function() {
  output <- HTML("
  .selectize-input {
    color: white; /* Text color inside the input box */
  }
  .selectize-dropdown {
    color: white; /* Text color in the dropdown options */
  }
  .selectize-input > .selectize-control.single .selectize-input.focus {
    color: white !important; /* Color for the selected text */
  }
  .selectize-dropdown .option {
    color: white !important; /* Color for the dropdown options */
  }
")
  return(output)
}


convert_to_metric_df <- function(df, category) {
  addition <- ifelse(category == "Per Possession", "_per_possession", ifelse(category == "Per Game", "_per_game", ""))
  counting_metrics <- c("goals", "assists", "blocks", "completions", "hockeyAssists", 
               "yardsThrown", "yardsReceived")
  counting_metrics <-paste0(counting_metrics,addition)
  percentage_metrics <- c("completion_percentage", "cpoe", "xcp")

  all_metrics <- c(counting_metrics, percentage_metrics)
  percentiles <- paste0(all_metrics, "_percentile")
  
  # Create a data frame for the result
  metric_df <- data.frame(
    metric = all_metrics,
    value = as.numeric(df[all_metrics]),
    percentile = as.numeric(df[percentiles]),
    stringsAsFactors = FALSE
  )

  metric_df <- metric_df %>% mutate(value = ifelse(metric %in% percentage_metrics, round(value * 100, 2), value))

  return(metric_df)
}



rename_metrics <- function(data, column = "metric") {
  rename_map <- c(
    goals = "Goals",
    hockeyAssists = "Hockey Assists",
    assists = "Assists",
    yardsThrown = "Throwing Yards",
    yardsReceived = "Receiving Yards",
    cpoe = "CPOE",
    completion_percentage = "Completion Percentage",
    blocks = "Blocks",
    xcp = "xCP"
  )
  data %>%
    mutate(
      !!column := sub("_per_possession$", "", !!sym(column)),  # Remove "_per_possession"
      !!column := sub("_per_game$", "", !!sym(column)),        # Remove "_per_game"
      !!column := recode(!!sym(column), !!!rename_map)          # Apply the rename map
    )
}