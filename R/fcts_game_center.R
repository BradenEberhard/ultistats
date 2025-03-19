get_win_probabilities <- function(throws_data) {
  fv_model_filename <- Sys.getenv("FV_MODEL_FILENAME")
  fv_preprocessing_info_filename <- Sys.getenv("FV_MODEL_INFO_FILENAME")
  in_game_win_model_filename <- Sys.getenv("IN_GAME_WIN_MODEL_FILENAME")
  fv_df <- predict_fv_for_throws(throws_data, fv_model_filename, fv_preprocessing_info_filename)
  throws_data <- throws_data %>%
    mutate(
      total_time_left = ifelse(game_quarter == 6, 100, time_left + ifelse(game_quarter < 4, (4 - game_quarter) * 720, 0)),
      fv = fv_df$fv_thrower,
      score_diff = (home_team_score - away_team_score) * ifelse(is_home_team == 1, 1, -1),
      game_is_lost = ifelse((score_diff + 1) < -(total_time_left / 15), 1, 0),
      game_is_won = ifelse((score_diff - 1) > (total_time_left / 15), 1, 0),
      game_time_left = ifelse(game_quarter == 5, -300 + total_time_left, total_time_left)
    )
  in_game_win_model <- get_model_data_from_db(in_game_win_model_filename)
  data_matrix = xgboost::xgb.DMatrix(data = as.matrix(throws_data %>% select("fv", "score_diff", "total_time_left", "thrower_y", "game_is_won", "game_is_lost")))
  throws_data$win_prob <- predict(in_game_win_model, data_matrix)
  throws_data <- throws_data %>% mutate(graphing_win_prob = ifelse(is_home_team == 1, win_prob, 1 - win_prob))
  throws_data <- throws_data %>% mutate(graphing_win_prob = graphing_win_prob * 2 - 1)

  return(throws_data)
}