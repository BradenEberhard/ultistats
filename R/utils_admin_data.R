update_score <- function(event_type, is_home_team, current_state) {
  if (event_type %in% c(19, 12)) { # score - recording team, callahan caught
    current_state$quarter_point <- current_state$quarter_point + 1
    if (is_home_team) current_state$home_team_score <- current_state$home_team_score + 1 else current_state$away_team_score <- current_state$away_team_score + 1
  } else if (event_type %in% c(15, 23)) { # score - opposing team, callahan thrown
    current_state$quarter_point <- current_state$quarter_point + 1
    if (is_home_team) current_state$away_team_score <- current_state$away_team_score + 1 else current_state$home_team_score <- current_state$home_team_score + 1
  }
  
  return(current_state)
}

update_quarter <- function(event_type, current_state) {
  if (event_type %in% c(28, 29, 30, 31, 32)) { # end of first, end of second, end of third, end of fourth, end of ot
    current_state$game_quarter <- current_state$game_quarter + 1
    current_state$quarter_point <- 1
  }
  return(current_state)
}

start_point <- function(event, current_state) {
  if (event$type %in% c(1,2)) { # start D point, start O point
    current_state$point_start_time <- event$time
    current_state$offensive_point <- ifelse(event$type == 1, TRUE, FALSE)
    current_state$possession_throw <- 0
    current_state$possession_num <- 1
  }
  return(current_state)
}

update_line <- function(event, current_state) {
  if (event$type %in% c(1,2,3,4,5,25)) { # start D point, start O point, midpoint TO - recording team, midpoint TO - opposing team, injury
    if (!is.null(event$time) && !is.na(event$time)) {
      current_state$point_start_time <- event$time
    }
    current_state$current_line <- event$line 
  }
  return(current_state)
}

update_possession_throw <- function(event_type, current_state) {
  if (event_type %in% c(18, 19, 20, 22, 23, 24)) { # pass, score - recording team, drop, throwaway - recording team, callahan thrown, stall - recording team
    current_state$possession_throw <- current_state$possession_throw + 1
  }
  return(current_state)
}

reset_possession_throw <- function(event_type, current_state) {
  if (event_type %in% c(19, 20, 22, 23, 24)) { # pass, score - recording team, drop, throwaway - recording team, callahan thrown, stall - recording team
    current_state$possession_throw <- 0
  }
  return(current_state)
}

update_possession_num <- function(event_type, current_state) {
  if (event_type %in% c(11, 13, 14, 20, 22, 24)) { # block, throwaway - opposing team, stall - opposing team, drop, throwaway - recording team, stall - recording team
    current_state$possession_num <- current_state$possession_num + 1
  }
  return(current_state)
}

reset_state <- function() {
  state <- list(
    home_team_score = 0,
    away_team_score = 0,
    game_quarter = 1,
    possession_num = 1,
    possession_throw = 0,
    offensive_point = NULL,
    last_event_is_penalty = FALSE,
    last_event_is_block = FALSE,
    current_line = c(),
    point_start_time = NULL,
    quarter_point = 1
  )
  return(state)
}


# Function to process and update games
update_games <- function(conn, base_url) {
  games_data <- fetch_games(base_url)
  games_data <- games_data %>%
    mutate(insertTimestamp = format(
      lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
      "%Y-%m-%d %H:%M:%S"
    ))
  create_table(conn=conn, table_name='games', data=games_data, index_col="gameID", override=TRUE)
  update_table(conn=conn, table_name='games', data=games_data, index_col="gameID", whole_table = TRUE)
}

# Function to process and update player stats
#' @importFrom tidyr separate_rows replace_na
update_player_stats <- function(conn, base_url) {
  players <- get_player_ids(conn)
  player_stats_data <- fetch_player_stats(base_url, players)
  player_stats_data <- player_stats_data %>%
    mutate(insertTimestamp = format(
      lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
      "%Y-%m-%d %H:%M:%S"
    ))
  
  throws_data <- get_throws_data(conn)
  games_per_player <- throws_data %>% 
    mutate(year = as.numeric(substr(gameID, 1, 4))) %>% 
    separate_rows(line, sep = ",") %>% 
    distinct(line, year, gameID) %>% 
    group_by(line, year) %>% 
    summarise(games = n_distinct(gameID), .groups = "drop")

  player_stats_data <- player_stats_data %>%
    left_join(games_per_player, by = c("playerID" = "line", "year" = "year")) %>%
    mutate(games = replace_na(games, 0))

  player_stats_data <- player_stats_data %>% 
    mutate(
        total_points = oPointsPlayed + dPointsPlayed,
        offensive_efficiency = oOpportunityScores / oOpportunities,
        defensive_efficiency = dOpportunityStops / dOpportunities, 
        
        # Per-game stats
        goals_per_game = goals / games,
        assists_per_game = assists / games,
        blocks_per_game = blocks / games,
        completions_per_game = completions / games,
        hockey_assists_per_game = hockeyAssists / games,
        yards_thrown_per_game = yardsThrown / games,
        yards_received_per_game = yardsReceived / games,
        
        # Per-possession stats based on oOpportunities
        offensive_goals_per_possession = goals / oOpportunities,
        offensive_assists_per_possession = assists / oOpportunities,
        offensive_completions_per_possession = completions / oOpportunities,
        offensive_hockey_assists_per_possession = hockeyAssists / oOpportunities,
        offensive_yards_thrown_per_possession = yardsThrown / oOpportunities,
        offensive_yards_received_per_possession = yardsReceived / oOpportunities,
        
        # Defensive per-possession stats based on dOpportunities
        defensive_blocks_per_possession = blocks / dOpportunities,
    )
  browser()
  columns_to_percentile <- c(
    "defensive_efficiency", "offensive_efficiency", 
    "goals", "assists", "blocks", "completions", "hockeyAssists", "yardsThrown", 
    "yardsReceived", "goals_per_game", "assists_per_game", "blocks_per_game", 
    "completions_per_game", "hockey_assists_per_game", 
    "yards_thrown_per_game", "yards_received_per_game", 
    "offensive_goals_per_possession", "offensive_assists_per_possession", 
    "offensive_completions_per_possession", 
    "offensive_hockey_assists_per_possession", "offensive_yards_thrown_per_possession", 
    "offensive_yards_received_per_possession", 
    "defensive_blocks_per_possession"
  )
  player_stats_data <- player_stats_data %>%
    group_by(year) %>%
    mutate(across(all_of(columns_to_percentile), 
                  ~ if_else(total_points >= 10, ntile(.x, 100), NA_real_), 
                  .names = "{.col}_percentile")) %>%
    ungroup()

  

  create_table(conn=conn, table_name='player_stats', data=player_stats_data, index_col="playerID", override=TRUE)
  update_table(conn=conn, table_name='player_stats', data=player_stats_data, index_col="playerID", whole_table = TRUE)
}

# Function to process and update players
update_players <- function(conn, base_url) {
  players_data <- fetch_players(base_url)
  players_data <- players_data %>% as_tibble() %>% unnest(teams)
  players_data <- players_data %>%
    mutate(insertTimestamp = format(
      lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
      "%Y-%m-%d %H:%M:%S"
    ))
  create_table(conn=conn, table_name='players', data=players_data, index_col="playerID", override=TRUE)
  update_table(conn=conn, table_name='players', data=players_data, index_col="playerID", whole_table = TRUE)
}

# Function to process and update teams
update_teams <- function(conn, base_url) {
  teams_data <- fetch_teams(base_url)
  teams_data <- teams_data %>% as_tibble() %>% unnest(division, names_sep = "_")
  teams_data <- teams_data %>%
    mutate(insertTimestamp = format(
      lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
      "%Y-%m-%d %H:%M:%S"
    ))
  create_table(conn=conn, table_name='teams', data=teams_data, index_col="teamID", override=TRUE)
  update_table(conn=conn, table_name='teams', data=teams_data, index_col="teamID", whole_table = TRUE)
}

update_pulls <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing Pulls", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      pull_data <- get_pulls_from_id(game_data, current_game_id)
      pull_data <- pull_data %>%
        mutate(insertTimestamp = format(
          lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
          "%Y-%m-%d %H:%M:%S"
        ))
      create_table(conn=conn, table_name='pulls', data=pull_data, index_col="gameID", override=FALSE)
      update_table(conn=conn, table_name='pulls', data=pull_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

update_throws <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing throws", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      throws_data <- get_throws_from_id(game_data, current_game_id)
      throws_data <- throws_data %>%
        mutate(insertTimestamp = format(
          lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
          "%Y-%m-%d %H:%M:%S"
        ))
      throws_data <- add_time_left(throws_data)
      throws_data$throw_distance <- sqrt((throws_data$receiver_x - throws_data$thrower_x)^2 + (throws_data$receiver_y - throws_data$thrower_y)^2)
      throws_data$x_diff <- throws_data$receiver_x - throws_data$thrower_x
      throws_data$y_diff <- throws_data$receiver_y - throws_data$thrower_y
      throws_data$throw_angle <- atan2(throws_data$y_diff, throws_data$x_diff) * (180 / pi)
      create_table(conn=conn, table_name='throws', data=throws_data, index_col=list("gameID","thrower", "throwID"), override=FALSE)
      update_table(conn=conn, table_name='throws', data=throws_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

update_blocks <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing Blocks", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      blocks_data <- get_blocks_from_id(game_data, current_game_id)
      blocks_data <- blocks_data %>%
        mutate(insertTimestamp = format(
          lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
          "%Y-%m-%d %H:%M:%S"
        ))
      create_table(conn=conn, table_name='blocks', data=blocks_data, index_col="gameID", override=FALSE)
      update_table(conn=conn, table_name='blocks', data=blocks_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

update_penalties <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing Penalties", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      penalties_data <- get_penalties_from_id(game_data, current_game_id)
      penalties_data <- penalties_data %>%
        mutate(insertTimestamp = format(
          lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
          "%Y-%m-%d %H:%M:%S"
        ))
      create_table(conn=conn, table_name='penalties', data=penalties_data, index_col="gameID", override=FALSE)
      update_table(conn=conn, table_name='penalties', data=penalties_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

get_throw_row <- function(current_state, thrower=NULL, thrower_x=NULL, thrower_y=NULL, receiver=NULL, receiver_x=NULL, receiver_y=NULL, turnover=0, drop=0, stall=0, is_home_team=NULL, gameID=NULL) {
  throw_row <- list()
  throw_row$thrower <- thrower
  throw_row$thrower_x <- thrower_x
  throw_row$thrower_y <- thrower_y
  throw_row$receiver <- receiver
  throw_row$receiver_x <- receiver_x
  throw_row$receiver_y <- receiver_y
  throw_row$turnover <- turnover
  throw_row$dropped_throw <- drop
  throw_row$stall <- stall
  throw_row$line <- paste(current_state$current_line, collapse=',')
  throw_row$is_home_team <- is_home_team
  throw_row$gameID <- gameID
  throw_row$home_team_score <- current_state$home_team_score
  throw_row$away_team_score <- current_state$away_team_score
  throw_row$game_quarter <- current_state$game_quarter
  throw_row$quarter_point <- current_state$quarter_point
  throw_row$point_start_time <- current_state$point_start_time
  throw_row$possession_num <- current_state$possession_num
  throw_row$possession_throw <- current_state$possession_throw
  throw_row$throwID <- paste(gameID, current_state$game_quarter, current_state$quarter_point, current_state$possession_num, current_state$possession_throw, sep = "-")
  return(throw_row)
}

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

# Function to get the group data based on group_id
get_group_data <- function(group_id, game_df) {
  return(game_df[game_df$group_id == group_id, ])
}

# Function to calculate interpolated times
interpolate_times <- function(start_time, end_time, num_points) {
  interpolated_times <- seq(start_time, end_time, length.out = num_points + 1)
  interpolated_times <- interpolated_times[-length(interpolated_times)]  # Remove the last value
  return(interpolated_times)
}

# Function to update the time_left column in the group data
update_time_left <- function(group_data, interpolated_times) {
  group_data$time_left <- interpolated_times
  return(group_data)
}

# Function to update the game_df with the modified group data
update_game_df <- function(group_data, game_df) {
  game_df[game_df$group_id == group_data$group_id[1], ] <- group_data
  return(game_df)
}