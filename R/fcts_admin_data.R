#' Generalized observeEvent Handler for Admin Panel
#' 
#' This function creates an observeEvent handler that listens for an event triggered by a UI input element. When the event is triggered, the function opens a database connection, calls the provided `update_function` to update data, and optionally shows a progress message.
#' 
#' @param input A list of input elements, typically from a Shiny UI, to listen for events.
#' @param input_id The ID of the input element to observe.
#' @param update_function The function to call when the input event is triggered. This function should take the database connection and base URL as parameters.
#' @param timestamps A function to reset timestamps or update progress. It is called after the update function is executed.
#' @param progress_message An optional message to display during progress. If provided, a progress bar is shown during execution.
#' @param conn_params A list of parameters for database connection, including `db_path` and `base_url`.
#' 
#' @return NULL This function does not return any value. It only triggers an update action when the event occurs.
#' 
#' @examples
#' # Example usage in a Shiny app:
#' create_observe_event(input, "updateButton", update_games, function(x) {}, progress_message = "Updating games...")
create_observe_event <- function(input, input_id, update_function, timestamps, progress_message = NULL, conn_params = list()) {
  observeEvent(input[[input_id]], {
    if (!is.null(progress_message)) {
      withProgress(message = progress_message, value = 0, {
        conn <- open_db_connection(conn_params$db_path)
        on.exit(close_db_connection(conn))
        update_function(conn, conn_params$base_url)
        
        timestamps(NULL)
      })
    } else {
      conn <- open_db_connection(conn_params$db_path)
      on.exit(close_db_connection(conn))
      update_function(conn, conn_params$base_url)
      timestamps(NULL)
    }
  })
}


# Function to update each table with progress
update_table_progress <- function(conn, table_update_func, table_name, progress_fn, base_url) {
  progress_fn(detail = paste("Processing", table_name))
  table_update_func(conn, base_url)
  progress_fn(detail = paste("Finished", table_name))
}

# Function to process and update games
update_games <- function(conn, base_url) {
  games_data <- fetch_games(base_url) %>% mutate(insertTimestamp = get_current_timestamp())
  create_table(conn=conn, table_name='games', data=games_data, index_col="gameID", override=TRUE)
  update_table(conn=conn, table_name='games', data=games_data, index_col="gameID", whole_table = TRUE)
}

#' Function to Process and Update Player Stats
#' 
#' This function fetches, processes, and updates the player statistics in a database. It computes advanced statistics, combines them with yearly and career data, adds timestamps, calculates additional stats, and processes data for final insertion or update in the `player_stats` table.
#' 
#' @param conn A database connection object to interact with the database.
#' @param base_url The base URL to fetch player data from an external API or service.
#' 
#' @return NULL This function does not return any value. It updates the `player_stats` table in the database.
#' 
#' @examples
#' # Example usage in the context of a Shiny app or scheduled task:
#' update_player_stats(conn, base_url)
update_player_stats <- function(conn, base_url) {
  # Fetch and process player stats data from external source
  player_stats_data <- fetch_and_process_player_stats(conn, base_url)
  
  # Compute advanced stats (ec, xcp, etc.) using the advanced_stats table
  advanced_stats <- get_table(conn, "advanced_stats")
  yearly_advanced_stats <- compute_advanced_stats(advanced_stats) # requires advanced_stats table
  career_stats <- compute_career_stats(advanced_stats) # aggregating career-level stats
  
  # Combine yearly and career stats into a single data frame
  advanced_stats <- bind_rows(yearly_advanced_stats, career_stats)
  
  # Add timestamps to player stats and calculate necessary derived statistics
  player_stats_data <- add_timestamp_and_calculate(player_stats_data, advanced_stats)
  
  # Process player stats for efficiency, full names, and other transformations
  player_stats_data <- process_player_stats(player_stats_data)
  
  # Insert or update player stats in the player_stats table
  create_table(conn=conn, table_name='player_stats', data=player_stats_data, index_col="playerID", override=TRUE)
  update_table(conn=conn, table_name='player_stats', data=player_stats_data, index_col="playerID", whole_table = TRUE)
}


# Function to process and update players
update_players <- function(conn, base_url) {
  players_data <- fetch_players(base_url) %>% as_tibble() %>% unnest(teams) %>% 
    mutate(insertTimestamp = get_current_timestamp()) %>%
    get_full_name()

  create_table(conn=conn, table_name='players', data=players_data, index_col="playerID", override=TRUE)
  update_table(conn=conn, table_name='players', data=players_data, index_col="playerID", whole_table = TRUE)
}

# Function to process and update teams
update_teams <- function(conn, base_url) {
  teams_data <- fetch_teams(base_url) %>% as_tibble() %>% unnest(division, names_sep = "_") %>%
    mutate(insertTimestamp = get_current_timestamp())

  create_table(conn=conn, table_name='teams', data=teams_data, index_col="teamID", override=TRUE)
  update_table(conn=conn, table_name='teams', data=teams_data, index_col="teamID", whole_table = TRUE)
}

# Function to process and update pulls
update_pulls <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing Pulls", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      pull_data <- get_pulls_from_id(game_data, current_game_id) %>% 
        mutate(insertTimestamp = get_current_timestamp())
      create_table(conn=conn, table_name='pulls', data=pull_data, index_col="gameID", override=FALSE)
      update_table(conn=conn, table_name='pulls', data=pull_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

# Function to process and update throws
update_throws <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing throws", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      throws_data <- get_throws_from_id(game_data, current_game_id) %>%
        mutate(insertTimestamp = get_current_timestamp())
      throws_data <- add_time_left(throws_data)
      
      # Engineer some additional features
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

# Function to process and update blocks
update_blocks <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing Blocks", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      blocks_data <- get_blocks_from_id(game_data, current_game_id) %>%
        mutate(insertTimestamp = get_current_timestamp())
      create_table(conn=conn, table_name='blocks', data=blocks_data, index_col="gameID", override=FALSE)
      update_table(conn=conn, table_name='blocks', data=blocks_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

# Function to process and update penalties
update_penalties <- function(conn, base_url) {
  game_ids <- get_game_ids(conn)
  withProgress(message = "Processing Penalties", value = 0, {
    for (i in seq_along(game_ids)) {
      current_game_id <- game_ids[[i]]
      game_data <- fetch_game(base_url, current_game_id)
      penalties_data <- get_penalties_from_id(game_data, current_game_id) %>%
        mutate(insertTimestamp = get_current_timestamp())
      create_table(conn=conn, table_name='penalties', data=penalties_data, index_col="gameID", override=FALSE)
      update_table(conn=conn, table_name='penalties', data=penalties_data, index_col="gameID", whole_table = FALSE)
      incProgress(1 / length(game_ids), detail = paste("Processing game ID", current_game_id))
    }
  })
}

#' Function to Process and Update Advanced Stats
#' 
#' This function fetches throws data, processes it through prediction models for both feature value (fv) and control points (cp), then applies additional transformations and calculations to derive advanced statistics. Finally, it updates or creates the `advanced_stats` table in the database with the processed data.
#' 
#' @param conn A database connection object to interact with the database.
#' @param base_url The base URL for fetching external data (currently unused in the function).
#' 
#' @return NULL This function does not return any value. It updates the `advanced_stats` table in the database.
#' 
#' @examples
#' # Example usage in the context of a Shiny app or scheduled task:
#' update_advanced_stats(conn, base_url)
update_advanced_stats <- function(conn, base_url) {
  fv_model_path <- "./inst/app/www/fv_xgb.model"
  fv_preprocessing_info_path <- "./inst/app/www/preprocessing_info_fv.rds"
  cp_model_path <- "./inst/app/www/cp_xgb.model"
  cp_preprocessing_info_path <- "./inst/app/www/preprocessing_info_cp.rds"

  throws_data <- get_throws_data(conn)
  fv_df <- predict_fv_for_throws(throws_data, fv_model_path, fv_preprocessing_info_path)
  advanced_stats_df <- predict_advanced_stats(throws_data, fv_df, cp_model_path, cp_preprocessing_info_path)
  advanced_stats_df <- mutate_advanced_stats(advanced_stats_df)
  advanced_stats_df <- calculate_aec(advanced_stats_df)
  advanced_stats_df <- clean_advanced_stats(advanced_stats_df)

  create_table(conn=conn, table_name="advanced_stats", data=advanced_stats_df, index_col="gameID", override=TRUE)
  update_table(conn=conn, table_name='advanced_stats', data=advanced_stats_df, index_col="gameID", whole_table=TRUE)
}



# Function to update all tables
update_all_tables <- function(conn, base_url) {
  withProgress(message = 'Processing All Tables', value = 0, { 
    num_tables <- 8 + 1
    
    table_updates <- list(
      list(update_func = update_games, table_name = "Games"),
      list(update_func = update_players, table_name = "Players"),
      list(update_func = update_throws, table_name = "Throws"),
      list(update_func = update_player_stats, table_name = "Player Stats"),
      list(update_func = update_teams, table_name = "Teams"),
      list(update_func = update_blocks, table_name = "Blocks"),
      list(update_func = update_pulls, table_name = "Pulls"),
      list(update_func = update_penalties, table_name = "Penalties"),
      list(update_func = update_advanced_stats, table_name = "Advanced Stats")
    )
    
    for (i in seq_along(table_updates)) { ##TODO increment progress isnt working correctly
      update_table_progress(conn, table_updates[[i]]$update_func, table_updates[[i]]$table_name, incProgress, base_url)
      incProgress(1/num_tables)  
    }
    
    incProgress(1/num_tables, detail = "All Processing Complete")
  })
}


### Functions to get individual table info from gameIDs

get_pulls_from_id = function(game, gameID) {
  rows <- list()
  current_state <- reset_state()
  process_pull_event <- function(event, is_home_team, gameID) {
    pull_row <- list()
    if (event$type == 7) {
      pull_row$pullX <- event$pullX
      pull_row$pullY <- event$pullY
      pull_row$pullMs <- event$pullMs
      pull_row$puller <- event$puller
      pull_row$in_bounds <- TRUE
      pull_row$is_home_team <- is_home_team
      pull_row$gameID <- gameID
      pull_row$home_team_score <- current_state$home_team_score
      pull_row$away_team_score <- current_state$away_team_score
      pull_row$game_quarter <- current_state$game_quarter
      rows <<- append(rows, list(pull_row))
    }
    else if (event$type == 8) {
      pull_row$pullX <- NULL
      pull_row$pullY <- NULL
      pull_row$pullMs <- NULL
      pull_row$puller <- event$puller
      pull_row$in_bounds <- FALSE
      pull_row$is_home_team <- is_home_team
      pull_row$gameID <- gameID
      pull_row$home_team_score <- current_state$home_team_score
      pull_row$away_team_score <- current_state$away_team_score
      pull_row$game_quarter <- current_state$game_quarter
      rows <<- append(rows, list(pull_row))
    }
    current_state <<- update_score(event$type, is_home_team, current_state)
    current_state <<- update_quarter(event$type, current_state)
  }
  apply(game$homeEvents, 1, function(row) process_pull_event(row, is_home_team = TRUE, gameID = gameID))
  current_state <- reset_state()
  apply(game$awayEvents, 1, function(row) process_pull_event(row, is_home_team = FALSE, gameID = gameID))      
  return(dplyr::bind_rows(rows))
}

get_blocks_from_id = function(game, gameID) {
  rows <- list()
  current_state <- reset_state()
  block_row <- list()
  process_block_event <- function(event, is_home_team, gameID) {
    if (!current_state$last_event_is_block){
      block_row <<- list()
    }
    else if (event$type %in% c(18, 19, 20, 22, 23, 24)) {
      block_row$turnover_x <<- event$throwerX
      block_row$turnover_y <<- event$throwerY
      rows <<- append(rows, list(block_row))
      current_state$last_event_is_block <<- FALSE 
    }
    else if (event$type %in% c(3, 5, 16,17, 25, 26, 27)) {
      current_state$last_event_is_block <<- TRUE 
    }
    else {
      rows <<- append(rows, list(block_row))
      current_state$last_event_is_block <<- FALSE 
    }
    if (event$type == 11) {
      block_row$defender <<- event$defender
      block_row$turnover_x <<- event$turnoverX
      block_row$turnover_y <<- event$turnoverY
      block_row$line <<- paste(current_state$current_line, collapse=',')
      block_row$is_home_team <<- is_home_team
      block_row$gameID <<- gameID
      block_row$home_team_score <<- current_state$home_team_score
      block_row$away_team_score <<- current_state$away_team_score
      block_row$game_quarter <<- current_state$game_quarter
      block_row$possession_num <<- current_state$possession_num
      current_state$last_event_is_block <<- TRUE 
    }
    current_state <<- update_line(event, current_state)
    current_state <<- update_score(event$type, is_home_team, current_state)
    current_state <<- update_quarter(event$type, current_state)
    current_state <<- update_possession_num(event$type, current_state)
  }
  apply(game$homeEvents, 1, function(row) process_block_event(row, is_home_team = TRUE, gameID = gameID))
  current_state <- reset_state()
  apply(game$awayEvents, 1, function(row) process_block_event(row, is_home_team = FALSE, gameID = gameID))      
  return(dplyr::bind_rows(rows))
}

get_penalties_from_id = function(game, gameID) {
  rows <- list()
  current_state <- reset_state()
  penalty_row <<- list()
  process_penalty_event <- function(event, is_home_team, gameID) {
    if (!current_state$last_event_is_penalty){
      penalty_row <<- list()
    }
    else if (event$type %in% c(18, 19, 20, 22, 23, 24)) {
      penalty_row$new_x <<- event$throwerX
      penalty_row$new_y <<- event$throwerY
      rows <<- append(rows, list(penalty_row))
      current_state$last_event_is_penalty <<- FALSE 
    }
    else if (event$type %in% c(3, 5, 16,17, 25, 26, 27)) {
      current_state$last_event_is_penalty <<- TRUE 
    }
    else {
      rows <<- append(rows, list(penalty_row))
      current_state$last_event_is_penalty <<- FALSE 
    }
    if (event$type == 11) {
      penalty_row$turnover_x <<- NULL
      penalty_row$turnover_y <<- NULL
      penalty_row$is_home_team <<- is_home_team
      penalty_row$gameID <<- gameID
      penalty_row$home_team_score <<- current_state$home_team_score
      penalty_row$away_team_score <<- current_state$away_team_score
      penalty_row$game_quarter <<- current_state$game_quarter
      penalty_row$possession_num <<- current_state$possession_num
      current_state$last_event_is_penalty <<- TRUE 
    }
    current_state <<- update_score(event$type, is_home_team, current_state)
    current_state <<- update_quarter(event$type, current_state)
    current_state <<- update_possession_num(event$type, current_state)
  }
  
  apply(game$homeEvents, 1, function(row) process_penalty_event(row, is_home_team = TRUE, gameID = gameID))
  current_state <- reset_state()
  apply(game$awayEvents, 1, function(row) process_penalty_event(row, is_home_team = FALSE, gameID = gameID))      
  return(dplyr::bind_rows(rows))
}

get_throws_from_id = function(game, gameID) {
  rows <- list()
  current_state <- reset_state()
  process_throw_event <- function(event, is_home_team, gameID) {
    current_state <<- update_possession_throw(event$type, current_state)

    if (event$type %in% c(18, 19)) { # pass, score - recording team
      throw_row <- get_throw_row(current_state, thrower=event$thrower, thrower_x=event$throwerX, thrower_y=event$throwerY, receiver=event$receiver, receiver_x=event$receiverX, receiver_y=event$receiverY, is_home_team=is_home_team, gameID=gameID)
      rows <<- append(rows, list(throw_row))
    } else if (event$type %in% c(22,23)) { # throwaway - recording team, callahan thrown
      throw_row <- get_throw_row(current_state, thrower=event$thrower, thrower_x=event$throwerX, thrower_y=event$throwerY, receiver_x=event$turnoverX, receiver_y=event$turnoverY, turnover=1, is_home_team=is_home_team, gameID=gameID)
      rows <<- append(rows, list(throw_row))
    } else if (event$type == 20) { # drop
      throw_row <- get_throw_row(current_state, thrower=event$thrower, thrower_x=event$throwerX, thrower_y=event$throwerY, receiver=event$receiver, receiver_x=event$receiverX, receiver_y=event$receiverY, turnover=1, drop=1, is_home_team=is_home_team, gameID=gameID)
      rows <<- append(rows, list(throw_row))
    } else if (event$type == 24) { # stall
      throw_row <- get_throw_row(current_state, thrower=event$thrower, thrower_x=event$throwerX, thrower_y=event$throwerY, receiver_x=event$throwerX, receiver_y=event$throwerY, turnover=1, stall=1, is_home_team=is_home_team, gameID=gameID)
      rows <<- append(rows, list(throw_row))
    }
    current_state <<- update_score(event$type, is_home_team, current_state)
    current_state <<- start_point(event, current_state)
    current_state <<- update_line(event, current_state)
    current_state <<- update_quarter(event$type, current_state)
    current_state <<- update_possession_num(event$type, current_state)
    current_state <<- reset_possession_throw(event$type, current_state)
  }
  apply(game$homeEvents, 1, function(row) process_throw_event(row, is_home_team = TRUE, gameID = gameID))
  current_state <- reset_state()
  apply(game$awayEvents, 1, function(row) process_throw_event(row, is_home_team = FALSE, gameID = gameID))      
  return(dplyr::bind_rows(rows))
}


add_time_left <- function(game_df) {
  game_df <- game_df %>% arrange(game_quarter, quarter_point, possession_num, possession_throw)
  game_df <- game_df %>%
    group_by(game_quarter, point_start_time, possession_num) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()
  unique_groups <- unique(game_df$group_id)
  window_size <- 3
  for (i in 1:(length(unique_groups) - window_size + 1)) {
    previous_time <- game_df[game_df$group_id == unique_groups[i], ]$point_start_time[1]
    current_time <- game_df[game_df$group_id == unique_groups[i+1], ]$point_start_time[1]
    next_time <- game_df[game_df$group_id == unique_groups[i+2], ]$point_start_time[1]
    if (current_time == 0 || next_time == 0) {
      next
    }
    if (current_time < previous_time || current_time > next_time) {
      new_time <- as.integer((previous_time + next_time) / 2)
      game_df[game_df$group_id == unique_groups[i+1], ]$point_start_time <- new_time
    }
  }
  game_df <- game_df %>%
    mutate(
      point_start_time = mapply(modify_point_start_time, game_quarter, point_start_time)
    )
  
  game_df <- game_df %>%
    group_by(game_quarter, point_start_time) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()
  game_df$time_left <- NA
  unique_groups <- unique(game_df$group_id)


  for (i in 1:(length(unique_groups) - 1)) {
    current_group <- get_group_data(unique_groups[i], game_df)
    next_group <- get_group_data(unique_groups[i + 1], game_df)
    
    next_start_time <- ifelse(current_group$game_quarter[1] != next_group$game_quarter[1], 0, next_group$point_start_time[1])

    interpolated_times <- interpolate_times(current_group$point_start_time[1], next_start_time, nrow(current_group))
    current_group <- update_time_left(current_group, interpolated_times)
    game_df <- update_game_df(current_group, game_df)
    if (i == (length(unique_groups) - 1)) {
      if (next_group$game_quarter[1] != 6) {
        next_start_time <- 252
      }
      interpolated_times <- interpolate_times(next_group$point_start_time[1], next_start_time, nrow(next_group))
      next_group <- update_time_left(next_group, interpolated_times)
      game_df <- update_game_df(next_group, game_df)
    }
  }
  game_df <- game_df %>% select(-group_id)
  game_df$time_left <- ifelse(game_df$time_left < 0, 10, game_df$time_left)
  return(game_df)
}