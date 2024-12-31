#' Predict FV Positions for Thrower and Receiver
#'
#' Predicts feature values for thrower and receiver positions using a pre-trained XGBoost model.
#'
#' @param model_path Path to the XGBoost model file.
#' @param preprocessing_info_path Path to the preprocessing information file containing scaling params and features.
#' @param throws_data Data frame with thrower and receiver features.
#'
#' @return Data frame with columns: `throwID`, `gameID`, `fv_thrower_position`, `fv_receiver_position`.
#'
#' @examples
#' model_path <- "./inst/app/www/fv_xgb.model"
#' preprocessing_info_path <- "./inst/app/www/preprocessing_info.rds"
#' throws_data <- some_throws_data
#' output_df <- predict_fv(model_path, preprocessing_info_path, throws_data)
#' 
#' @importFrom xgboost xgb.load xgb.DMatrix
#' @importFrom dplyr select
predict_fv <- function(model_path, preprocessing_info_path, throws_data) {
  # Load model and preprocessing info
  xgb_model <- xgb.load(model_path)
  preprocessing_info <- readRDS(preprocessing_info_path)
  scaling_params <- preprocessing_info$scaling_params
  thrower_features <- preprocessing_info$features
  
  # Prepare features
  thrower_throws <- throws_data %>% select(all_of(thrower_features))
  receiver_throws <- throws_data %>% select(all_of(unlist(lapply(thrower_features, function(x) gsub("thrower", "receiver", x)))))
  
  # Rename receiver features
  names(receiver_throws) <- gsub("receiver", "thrower", names(receiver_throws))
  opponent_throws <- receiver_throws %>%
    mutate(
      thrower_x = -thrower_x, # Multiply thrower_x by -1
      thrower_y = pmin(pmax(120 - thrower_y, 20), 100) # Transform thrower_y and clip between 20 and 100
    )

  
  # Scale data
  thrower_throws_scaled <- xgb.DMatrix(data = as.matrix(predict(scaling_params, thrower_throws)))
  receiver_throws_scaled <- xgb.DMatrix(data = as.matrix(predict(scaling_params, receiver_throws)))
  opponent_throws_scaled <- xgb.DMatrix(data = as.matrix(predict(scaling_params, opponent_throws)))
  
  # Get predictions
  thrower_pred_probs <- predict(xgb_model, thrower_throws_scaled)
  receiver_pred_probs <- predict(xgb_model, receiver_throws_scaled)
  opponent_pred_probs <- predict(xgb_model, opponent_throws_scaled)

  
  # Create ID column and return output
  id_column <- paste(throws_data$gameID, throws_data$game_quarter, throws_data$quarter_point, throws_data$possession_num, throws_data$possession_throw, sep = "-")
  output_dataframe <- data.frame(
    throwID = id_column,
    gameID = throws_data$gameID,
    fv_thrower = thrower_pred_probs,
    fv_receiver = receiver_pred_probs,
    fv_opponent = opponent_pred_probs
  )
  
  return(output_dataframe)
}


#' Predict FV Positions for Thrower and Receiver
#'
#' Predicts feature values for thrower and receiver positions using a pre-trained XGBoost model.
#'
#' @param model_path Path to the XGBoost model file.
#' @param preprocessing_info_path Path to the preprocessing information file containing scaling params and features.
#' @param throws_data Data frame with thrower and receiver features.
#'
#' @return Data frame with columns: `throwID`, `gameID`, `fv_thrower_position`, `fv_receiver_position`.
#'
#' @examples
#' model_path <- "./inst/app/www/cp_xgb.model"
#' preprocessing_info_path <- "./inst/app/www/preprocessing_info_cp.rds"
#' throws_data <- some_throws_data
#' output_df <- predict_cp(model_path, preprocessing_info_path, throws_data)
#' 
#' @importFrom xgboost xgb.load xgb.DMatrix
#' @importFrom dplyr select
predict_cp <- function(model_path, preprocessing_info_path, throws_data) {
  xgb_model <- xgb.load(model_path)
  preprocessing_info <- readRDS(preprocessing_info_path)
  scaling_params <- preprocessing_info$scaling_params
  thrower_features <- preprocessing_info$features
  
  # Prepare features
  thrower_throws <- throws_data %>% select(all_of(thrower_features))
  # Scale data
  thrower_throws_scaled <- xgb.DMatrix(data = as.matrix(predict(scaling_params, thrower_throws)))
  # Get predictions
  thrower_pred_probs <- predict(xgb_model, thrower_throws_scaled)
  
  # Create ID column and return output
  id_column <- paste(throws_data$gameID, throws_data$game_quarter, throws_data$quarter_point, throws_data$possession_num, throws_data$possession_throw, sep = "-")
  output_dataframe <- data.frame(
    throwID = id_column,
    gameID = throws_data$gameID,
    cp = thrower_pred_probs,
    cpoe = ifelse(throws_data$turnover == 1, 0, 1) - thrower_pred_probs
  )
  
  return(output_dataframe)
}
