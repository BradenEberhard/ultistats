#' Retrieve and Process Player Throws
#'
#' Fetches throw data for a player from the database and processes it to add adjusted angles and year.
#'
#' @param db_path Path to the database file.
#' @param player_full_name Player's fullName
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
  .selectize-input input {
    color: white; /* Typed text color */
  }
  .selectize-dropdown {
    color: white; /* Text color in the dropdown options */
  }
  .selectize-dropdown .option:hover {
    background-color: #1ABC9C !important;
  }
  .selectize-dropdown .active {
    background-color: #1ABC9C !important; 
    color: white !important; /* Text color of the active option */
  }
")
  return(output)
}

calc_percentile <- function(player_value, all_values) {
  return(mean(all_values <= player_value, na.rm = TRUE) * 100)
}

get_metrics <- function(category) {
  addition <- ifelse(category == "Per Possession", "_per_possession", 
                     ifelse(category == "Per Game", "_per_game", ""))
  counting_metrics <- c("goals", "assists", "blocks", "completions", "hockeyAssists", 
                        "yardsThrown", "yardsReceived")
  counting_metrics <- paste0(counting_metrics, addition)
  percentage_metrics <- c("offensive_efficiency", "defensive_efficiency", "completion_percentage", "cpoe", "xcp")
  return(c(counting_metrics, percentage_metrics))
}

filter_year <- function(df, year) {
  return(df[df$year == year, ])
}

rename_metrics <- function(data, keep_category=FALSE, column = "metric") {
  if (keep_category){
    rename_map <- c(
      games = "Games",
      yardsThrown = "Throwing Yards",
      yardsThrown_per_game = "Throwing Yards Per Game",
      yardsThrown_per_possession = "Throwing Yards Per Possession",
      yardsReceived = "Receiving Yards",
      yardsReceived_per_game = "Receiving Yards Per Game",
      yardsReceived_per_possession = "Receiving Yards Per Possession",
      completions = "Completions",
      completions_per_game = "Completions Per Game",
      completions_per_possession = "Completions Per Possession",
      oOpportunities = "Possessions",
      completion_percentage = "Completion Percentage",
      cpoe = "CPOE",
      xcp = "xCP",
      assists = "Assists",
      assists_per_possession = "Assists Per Possession",
      hockeyAssists = "Hockey Assists",
      hockeyAssists_per_possession = "Hockey Assists Per Possession",
      turnovers = "Turnovers",
      turnovers_per_possession = "Turnovers Per Possession"
    )
    data <- data %>%
    mutate(    
      !!column := recode(!!sym(column), !!!rename_map)
    )
    return(data)
  }
  rename_map <- c(
    goals = "Goals ",
    hockeyAssists = "Hockey Assists ",
    assists = "Assists ",
    yardsThrown = "Throwing Yards ",
    yardsReceived = "Receiving Yards ",
    cpoe = "CPOE ",
    completion_percentage = "Completion Percentage ",
    blocks = "Blocks ",
    completions = "Completions ",
    xcp = "xCP ",
    defensive_efficiency = "Defensive Efficiency",
    offensive_efficiency = "Offensive Efficiency",
    turnovers = "Turnovers"
  )
  data <- data %>%
    mutate(
      !!column := sub("_per_possession$", "", !!sym(column)),  
      !!column := sub("_per_game$", "", !!sym(column)),        
      !!column := recode(!!sym(column), !!!rename_map)
    )
  return(data)
}

adjust_for_role <- function(df, handler_value, offense_value, player_year, player_full_name) {
  if (handler_value) {
    player_handler <- df %>% filter(fullName == player_full_name, year == player_year) %>% pull(handler)
    df <- df %>% filter(handler == player_handler | fullName == player_full_name)
  }
  if (offense_value) {
    player_offense <- df %>% filter(fullName == player_full_name, year == player_year) %>% pull(offense)
    df <- df %>% filter(offense == player_offense | fullName == player_full_name)
  }
  
  return(df)
}

adjust_for_category <- function(df, category) {
  if (category == "Per Game") {
    df$goals_per_game <- df$goals / df$games
    df$assists_per_game <- df$assists / df$games
    df$blocks_per_game <- df$blocks / df$games
    df$completions_per_game <- df$completions / df$games
    df$hockeyAssists_per_game <- df$hockeyAssists / df$games
    df$yardsThrown_per_game <- df$yardsThrown / df$games
    df$yardsReceived_per_game <- df$yardsReceived / df$games
    
    # Add turnovers per game if the turnovers column exists
    if ("turnovers" %in% colnames(df)) {
      df$turnovers_per_game <- df$turnovers / df$games
    }
  } else if (category == "Per Possession") {
    df$goals_per_possession <- df$goals / df$oOpportunities
    df$assists_per_possession <- df$assists / df$oOpportunities
    df$blocks_per_possession <- df$blocks / df$dOpportunities
    df$completions_per_possession <- df$completions / df$oOpportunities
    df$hockeyAssists_per_possession <- df$hockeyAssists / df$oOpportunities
    df$yardsThrown_per_possession <- df$yardsThrown / df$oOpportunities
    df$yardsReceived_per_possession <- df$yardsReceived / df$oOpportunities
    
    # Add turnovers per possession if the turnovers column exists
    if ("turnovers" %in% colnames(df)) {
      df$turnovers_per_possession <- df$turnovers / df$oOpportunities
    }
  }
  return(df)
}


get_playerID_by_fullName <- function(data, fullName_filter) {
  # Filter the dataframe by fullName
  filtered_data <- data %>%
    filter(fullName == fullName_filter) %>%
    slice_head(n = 1)  # Get the first row
  
  # Return the playerID from the first row, or NA if no match found
  if (nrow(filtered_data) > 0) {
    return(filtered_data$playerID)
  } else {
    return(NA)  # Return NA if fullName does not exist
  }
}

prepare_metric_data <- function(df, player_full_name, thrower_handler_value, thrower_offense_value, all_metrics) {
  metric_data <- list()
  
  player_df <- df %>% filter(fullName == player_full_name, year != "Career") %>% arrange(year)
  
  for (i in 1:nrow(player_df)) {
    year <- player_df[i, "year"]
    df_year <- adjust_for_role(df, thrower_handler_value, thrower_offense_value, year, player_full_name)
    filtered_df <- df_year %>% filter(fullName == player_full_name, year != "Career") %>% arrange(year)
    player_value <- filtered_df[i, all_metrics]
    
    df_final_players <- df_year %>% filter(games >= 3)
    for (metric in all_metrics) {
      if (!is.na(player_value[[metric]])) {
        metric_values <- df_final_players[df_final_players$year == year, metric]
        percentile_value <- calc_percentile(player_value[[metric]], metric_values) %>% round(2)
        if (grepl("turnover", metric)) {
          percentile_value <- 100 - percentile_value
        }
        metric_data[[length(metric_data) + 1]] <- list(
          year = year,
          metric = metric,
          percentile = percentile_value,
          value = player_value[[metric]]
        )
      }
    }
  }
  
  return(metric_data)
}

format_metric_data <- function(metric_data) {
  metric_df <- bind_rows(lapply(metric_data, function(x) {
    data.frame(
      year = x$year,
      metric = x$metric,
      percentile = x$percentile,
      value = x$value
    )
  })) %>% arrange(year) %>% rename_metrics(keep_category = TRUE)
  
  return(metric_df)
}


generate_percentile_plot <- function(metric_df, title) {
  plot <- plot_ly(metric_df, 
    x = ~year, 
    y = ~percentile, 
    color = ~metric, 
    type = 'scatter', 
    mode = 'lines+markers', 
    line = list(width = 2),
    marker = list(size = 6),
    hoverinfo = 'text',
    text = ~paste0(
      metric, ": ", 
      ifelse(value %% 1 == 0, scales::comma(value, accuracy = 1), sprintf("%.2f", value)), 
      "\nPercentile: ", percentile
    )) %>%
    layout(title = title,
           xaxis = list(title = "Year"),
           yaxis = list(title = "Percentile", range = c(0, 102), tickvals = seq(0, 100, by = 20)),
           margin = list(t = 50),
           showlegend = TRUE) %>%
    config(displayModeBar = FALSE)
  
  return(plot)
}

