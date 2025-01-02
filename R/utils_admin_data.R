### General functions for processing tables

#' @importFrom lubridate ymd_hms
get_current_timestamp <- function() {
  format(ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}

# Fetch and preprocess games and player stats
#' @importFrom tidyr separate_rows replace_na
fetch_and_process_player_stats <- function(conn, base_url) {
  players <- get_player_ids(conn)
  player_stats_data <- fetch_player_stats(base_url, players)
  throws_data <- get_throws_data(conn)
  
  # Preprocess games per player
  games_per_player <- throws_data %>% 
    mutate(year = as.numeric(substr(gameID, 1, 4))) %>% 
    separate_rows(line, sep = ",") %>% 
    distinct(line, year, gameID) %>% 
    group_by(line, year) %>% 
    summarise(games = n_distinct(gameID), .groups = "drop")
  
  # Join and return
  player_stats_data <- player_stats_data %>%
    left_join(games_per_player, by = c("playerID" = "line", "year" = "year")) %>%
    mutate(games = replace_na(games, 0), year = as.character(year))
  
  return(player_stats_data)
}

# Compute advanced stats for throwers and receivers
compute_advanced_stats <- function(advanced_stats) {
  # Thrower stats
  yearly_advanced_stats_thrower <- advanced_stats %>%
    filter(.data$dropped_throw != 1) %>% 
    mutate(year = as.integer(substr(gameID, 1, 4))) %>% 
    group_by(thrower, year) %>%                        
    summarise(
      xcp = mean(cp, na.rm = TRUE),
      thrower_ec = sum(ec, na.rm = TRUE),
      thrower_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Receiver stats
  yearly_advanced_stats_receiver <- advanced_stats %>%
    mutate(year = as.integer(substr(gameID, 1, 4))) %>% 
    group_by(receiver, year) %>%                        
    summarise(
      receiver_ec = sum(ec, na.rm = TRUE),
      receiver_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join thrower and receiver stats
  yearly_advanced_stats <- yearly_advanced_stats_thrower %>%
    left_join(yearly_advanced_stats_receiver, by = c("thrower" = "receiver", "year" = "year"))
  yearly_advanced_stats$year <- as.character(yearly_advanced_stats$year)
  return(yearly_advanced_stats)
}

compute_career_data_from_player_stats <- function(player_stats_data) {
  # Filter players with at least one year >= 2021
  players_with_career_data <- player_stats_data %>%
    filter(year >= 2021) %>%
    select(playerID) %>%
    distinct()

  # Summing career stats for each player and recalculating percentages
  career_data <- player_stats_data %>%
    filter(playerID %in% players_with_career_data$playerID) %>%
    group_by(playerID) %>%
    summarise(
      firstName = first(firstName),
      lastName = first(lastName),
      year = "Career",  
      assists = sum(assists, na.rm = TRUE),
      goals = sum(goals, na.rm = TRUE),
      hockeyAssists = sum(hockeyAssists, na.rm = TRUE),
      completions = sum(completions, na.rm = TRUE),
      throwAttempts = sum(throwAttempts, na.rm = TRUE),
      throwaways = sum(throwaways, na.rm = TRUE),
      stalls = sum(stalls, na.rm = TRUE),
      callahansThrown = sum(callahansThrown, na.rm = TRUE),
      yardsReceived = sum(yardsReceived, na.rm = TRUE),
      yardsThrown = sum(yardsThrown, na.rm = TRUE),
      hucksAttempted = sum(hucksAttempted, na.rm = TRUE),
      hucksCompleted = sum(hucksCompleted, na.rm = TRUE),
      catches = sum(catches, na.rm = TRUE),
      drops = sum(drops, na.rm = TRUE),
      blocks = sum(blocks, na.rm = TRUE),
      callahans = sum(callahans, na.rm = TRUE),
      pulls = sum(pulls, na.rm = TRUE),
      obPulls = sum(obPulls, na.rm = TRUE),
      recordedPulls = sum(recordedPulls, na.rm = TRUE),
      recordedPullsHangtime = sum(recordedPullsHangtime, na.rm = TRUE),
      oPointsPlayed = sum(oPointsPlayed, na.rm = TRUE),
      oPointsScored = sum(oPointsScored, na.rm = TRUE),
      dPointsPlayed = sum(dPointsPlayed, na.rm = TRUE),
      dPointsScored = sum(dPointsScored, na.rm = TRUE),
      secondsPlayed = sum(secondsPlayed, na.rm = TRUE),
      oOpportunities = sum(oOpportunities, na.rm = TRUE),
      oOpportunityScores = sum(oOpportunityScores, na.rm = TRUE),
      dOpportunities = sum(dOpportunities, na.rm = TRUE),
      dOpportunityStops = sum(dOpportunityStops, na.rm = TRUE),
      games = sum(games, na.rm = TRUE),
    ) 
  return(bind_rows(player_stats_data, career_data))
}

# Compute career stats for throwers and receivers
compute_career_stats <- function(advanced_stats) {
  career_stats_thrower <- advanced_stats %>%
    filter(dropped_throw != 1) %>% 
    group_by(thrower) %>%  
    summarise(
      xcp = mean(cp, na.rm = TRUE),
      thrower_ec = sum(ec, na.rm = TRUE),
      thrower_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    )
  
  career_stats_receiver <- advanced_stats %>%
    group_by(receiver) %>%  
    summarise(
      receiver_ec = sum(ec, na.rm = TRUE),
      receiver_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    ) 
  
  # Join career stats
  career_stats <- career_stats_thrower %>%
    left_join(career_stats_receiver, by = c("thrower" = "receiver")) %>%
    mutate(year = "Career")
  
  return(career_stats)
}

# Add timestamp and other calculated stats
add_timestamp_and_calculate <- function(player_stats_data, advanced_stats) {
  player_stats_data <- player_stats_data %>%
    mutate(insertTimestamp = get_current_timestamp())
  
  player_stats_data <- player_stats_data %>%
    mutate(
      handler = ifelse(yardsThrown > yardsReceived, TRUE, FALSE),
      offense = ifelse(oPointsPlayed > dPointsPlayed, TRUE, FALSE)
    )
  
  player_stats_data <- player_stats_data %>%
    left_join(advanced_stats, by = c("playerID" = "thrower", "year" = "year"))
  
  return(player_stats_data)
}

# Function to process player stats data
process_player_stats <- function(player_stats_data) {
  # Filter and join additional info
  player_stats_data <- player_stats_data %>%
    mutate(
      total_points = oPointsPlayed + dPointsPlayed,
      offensive_efficiency = if_else(oOpportunities == 0, NA_real_, oOpportunityScores / oOpportunities),
      defensive_efficiency = if_else(dOpportunities == 0, NA_real_, dOpportunityStops / dOpportunities),
      completion_percentage = if_else(throwAttempts == 0, NA_real_, completions / throwAttempts),
      cpoe = completion_percentage - xcp
    ) %>%
    filter((oPointsPlayed + dPointsPlayed) > 0)
  
  player_stats_data <- get_full_name(player_stats_data)
  return(player_stats_data)
}

# Function to get full name for players
get_full_name <- function(players_data) {
  df_first_rows <- players_data %>%
    group_by(playerID) %>%
    slice_head(n = 1) %>%
    ungroup()

  df_first_rows <- df_first_rows %>%
    mutate(fullName = paste(firstName, lastName)) %>%
    mutate(
      fullName = ifelse(duplicated(fullName) | duplicated(fullName, fromLast = TRUE),
                        paste(fullName, "(", playerID, ")"), 
                        fullName)
    )

  players_data <- players_data %>%
    left_join(df_first_rows %>% select(playerID, fullName), by = "playerID")
  
  return(players_data)
}


### Additional functions specific to events, updating game state, etc.

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

# handle all info necessary for a throw
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


# interpolate time left
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

predict_fv_for_throws <- function(throws_data, fv_model_path, fv_preprocessing_info_path) {
  fv_df <- predict_fv(fv_model_path, fv_preprocessing_info_path, throws_data)
  fv_df$thrower <- throws_data$thrower
  fv_df$receiver <- throws_data$receiver
  return(fv_df)
}

predict_advanced_stats <- function(throws_data, fv_df, cp_model_path, cp_preprocessing_info_path) {
  advanced_stats_df <- predict_cp(cp_model_path, cp_preprocessing_info_path, throws_data)
  advanced_stats_df$thrower <- fv_df$thrower
  advanced_stats_df$fv_thrower <- fv_df$fv_thrower
  advanced_stats_df$receiver <- fv_df$receiver
  advanced_stats_df$fv_receiver <- fv_df$fv_receiver
  advanced_stats_df$fv_opponent <- fv_df$fv_opponent
  advanced_stats_df$year <- throws_data$year
  advanced_stats_df$gameID <- fv_df$gameID
  advanced_stats_df$game_quarter <- throws_data$game_quarter
  advanced_stats_df$quarter_point <- throws_data$quarter_point
  advanced_stats_df$possession_num <- throws_data$possession_num
  advanced_stats_df$possession_throw <- throws_data$possession_throw
  advanced_stats_df$is_home_team <- throws_data$is_home_team
  advanced_stats_df$turnover <- throws_data$turnover
  advanced_stats_df$receiver_y <- throws_data$receiver_y
  advanced_stats_df$dropped_throw <- throws_data$dropped_throw
  return(advanced_stats_df)
}

mutate_advanced_stats <- function(advanced_stats_df) {
  advanced_stats_df <- advanced_stats_df %>%
    mutate(
      fv_receiver = ifelse(receiver_y >= 100, 1, fv_receiver),
      insertTimestamp = get_current_timestamp(),
      ec = ifelse(turnover == 1, -fv_opponent, fv_receiver - fv_thrower)
    )
  return(advanced_stats_df)
}

calculate_aec <- function(advanced_stats_df) {
  advanced_stats_df <- advanced_stats_df %>%
    group_by(.data$gameID, .data$game_quarter, .data$quarter_point, .data$possession_num, .data$is_home_team) %>%
    arrange(.data$possession_throw, .by_group = TRUE) %>%
    mutate(
      prev_fv_receiver = lag(.data$fv_receiver), # Add the previous row's fv_receiver as its own column
      min_fv = min(pmin(c(ifelse(.data$turnover == 1, NA, .data$fv_receiver), first(.data$fv_thrower))), na.rm = TRUE),
      aec = ifelse(
        .data$turnover == 1, 
        -prev_fv_receiver, 
        ifelse(
          .data$receiver_y >= 100 & .data$turnover == 0, 
          (1 - .data$prev_fv_receiver) / (1 - min_fv), 
          (.data$fv_receiver - .data$prev_fv_receiver) / (1 - .data$min_fv)
        )
      ),
      aec = ifelse(is.na(.data$aec), .data$ec / (1 - .data$min_fv), .data$aec)
    ) %>%
    ungroup()
  return(advanced_stats_df)
}

clean_advanced_stats <- function(advanced_stats_df) {
  advanced_stats_df <- advanced_stats_df %>%
    select(-.data$min_fv, -.data$receiver_y, -.data$game_quarter, -.data$quarter_point, -.data$possession_throw, -.data$possession_num, -.data$is_home_team, -.data$turnover, -.data$prev_fv_receiver)
  return(advanced_stats_df)
}

