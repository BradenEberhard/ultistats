#' admin_models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_models_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("update_advanced_stats"), "Update Advanced Stats", class = "btn btn-primary")
  )
}
    
#' admin_models Server Functions
#' @noRd 
mod_admin_models_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    fv_model_path <- "./inst/app/www/fv_xgb.model"
    fv_preprocessing_info_path <- "./inst/app/www/preprocessing_info_fv.rds"
    cp_model_path <- "./inst/app/www/cp_xgb.model"
    cp_preprocessing_info_path <- "./inst/app/www/preprocessing_info_cp.rds"
    
    observeEvent(input$update_advanced_stats, { 
      showNotification("Processing Advanced Stats table.", type = "message", duration = 3)

      db_path <- get_golem_config("db_path")
      conn <- open_db_connection(db_path)
      throws_data <- get_throws_data(conn)
      
      fv_df <- predict_fv(fv_model_path, fv_preprocessing_info_path, throws_data)
      fv_df$thrower <- throws_data$thrower
      fv_df$receiver <- throws_data$receiver

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
      advanced_stats_df <- advanced_stats_df %>%
        mutate(
          fv_receiver = ifelse(receiver_y >= 100, 1, fv_receiver) 
        )

      advanced_stats_df <- advanced_stats_df %>%
        mutate(insertTimestamp = format(
          lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
          "%Y-%m-%d %H:%M:%S"
        ))
      
      advanced_stats_df$ec <- ifelse(
          advanced_stats_df$turnover == 1, 
          -advanced_stats_df$fv_opponent, 
          advanced_stats_df$fv_receiver - advanced_stats_df$fv_thrower
        )
              
      advanced_stats_df <- advanced_stats_df %>%
        group_by(gameID, game_quarter, quarter_point, possession_num, is_home_team) %>%
        arrange(possession_throw, .by_group = TRUE) %>%
        mutate(
          prev_fv_receiver = lag(fv_receiver), # Add the previous row's fv_receiver as its own column
          min_fv = min(
            pmin(c(ifelse(turnover == 1, NA, fv_receiver), first(fv_thrower))), 
            na.rm = TRUE
          ),
          aec = ifelse(
            turnover == 1, 
            -prev_fv_receiver, 
            ifelse(
              receiver_y >= 100 & turnover == 0, 
              (1 - prev_fv_receiver) / (1 - min_fv), 
              (fv_receiver - prev_fv_receiver) / (1 - min_fv)
            )
          ),
          aec = ifelse(is.na(aec), ec / (1 - min_fv), aec)
        ) %>%
        ungroup()
      
      advanced_stats_df$dropped_throw = throws_data$dropped_throw
      # advanced_stats_df <- advanced_stats_df %>% select(-min_fv, -receiver_y, -game_quarter, -quarter_point, -possession_throw, -possession_num, -is_home_team, -turnover, -prev_fv_receiver)
      create_table(conn=conn, table_name="advanced_stats", data=advanced_stats_df, index_col="gameID", override=TRUE)
      update_table(conn=conn, table_name='advanced_stats', data=advanced_stats_df, index_col="gameID", whole_table=TRUE)
      close_db_connection(conn)
      showNotification("Advanced Stats table successfully updated!", type = "message", duration = 3)
    })
    
  })
}