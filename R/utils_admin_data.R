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
  # add games
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

  career_data <- compute_career_data(player_stats_data, advanced_stats)
  player_stats_data$year <- as.character(player_stats_data$year); career_data$year <- as.character(career_data$year)
  player_stats_data <- bind_rows(player_stats_data, career_data)
  
  player_stats_data <- player_stats_data %>%
    mutate(insertTimestamp = format(
      lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
      "%Y-%m-%d %H:%M:%S"
    ))
  

  
  player_stats_data <- player_stats_data %>%
    mutate(
      handler = ifelse(yardsThrown > yardsReceived, TRUE, FALSE),
      offense = ifelse(oPointsPlayed > dPointsPlayed, TRUE, FALSE)
    )
  advanced_stats <- get_table(conn, "advanced_stats")

  # Compute thrower stats
  yearly_advanced_stats_thrower <- advanced_stats %>%
    filter(dropped_throw != 1) %>% 
    mutate(year = as.integer(substr(gameID, 1, 4))) %>% 
    group_by(thrower, year) %>%                        
    summarise(
      xcp = mean(cp, na.rm = TRUE),
      thrower_ec = sum(ec, na.rm = TRUE),
      thrower_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute receiver stats
  yearly_advanced_stats_receiver <- advanced_stats %>%
    mutate(year = as.integer(substr(gameID, 1, 4))) %>% 
    group_by(receiver, year) %>%                        
    summarise(
      receiver_ec = sum(ec, na.rm = TRUE),
      receiver_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    )

  # Join thrower and receiver stats by thrower and year
  yearly_advanced_stats <- yearly_advanced_stats_thrower %>%
    left_join(yearly_advanced_stats_receiver, by = c("thrower" = "receiver", "year" = "year"))


    
  career_stats_thrower <- advanced_stats %>%
    filter(dropped_throw != 1) %>% 
    group_by(thrower) %>%  # Group by player (thrower)
    summarise(
      xcp = mean(cp, na.rm = TRUE),
      thrower_ec = sum(ec, na.rm = TRUE),
      thrower_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute receiver career stats
  career_stats_receiver <- advanced_stats %>%
    group_by(receiver) %>%  # Group by player (receiver)
    summarise(
      receiver_ec = sum(ec, na.rm = TRUE),
      receiver_aec = sum(aec, na.rm = TRUE),
      .groups = "drop"
    ) 

  # Join thrower and receiver career stats by player
  career_stats <- career_stats_thrower %>%
    left_join(career_stats_receiver, by = c("thrower" = "receiver")) %>%
    mutate(year = "Career")
  
  yearly_advanced_stats$year <- as.character(yearly_advanced_stats$year); career_stats$year <- as.character(career_stats$year)
  advanced_stats <- bind_rows(yearly_advanced_stats, career_stats)

  player_stats_data <- player_stats_data %>%
    left_join(advanced_stats, by = c("playerID" = "thrower", "year" = "year"))

  player_stats_data <- player_stats_data %>% 
    mutate(
        total_points = oPointsPlayed + dPointsPlayed,
        offensive_efficiency = if_else(oOpportunities == 0, NA_real_, oOpportunityScores / oOpportunities),
        defensive_efficiency = if_else(dOpportunities == 0, NA_real_, dOpportunityStops / dOpportunities),
        completion_percentage = if_else(throwAttempts == 0, NA_real_, completions / throwAttempts),
        cpoe = completion_percentage - xcp,
    )

  player_stats_data <- player_stats_data %>% filter((oPointsPlayed + dPointsPlayed) > 0)

  df_first_rows <- player_stats_data %>%
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

  player_stats_data <- player_stats_data %>%
    left_join(df_first_rows %>% select(playerID, fullName), by = "playerID")
    
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

# Helper function to compute career stats for a player
compute_career_data <- function(player_stats_data, advanced_stats) {
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
  return(career_data)
}
