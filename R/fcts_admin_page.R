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