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
    actionButton(ns("update_fv"), "Update FV")
  )
}
    
#' admin_models Server Functions
#' @importFrom xgboost xgb.load xgb.DMatrix
#' @noRd 
mod_admin_models_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    fv_model_path <- "./inst/app/www/fv_xgb.model"
    fv_preprocessing_info_path <- "./inst/app/www/preprocessing_info_fv.rds"
    cp_model_path <- "./inst/app/www/cp_xgb.model"
    cp_preprocessing_info_path <- "./inst/app/www/preprocessing_info_cp.rds"
    
    observeEvent(input$update_fv, { 
      db_path <- get_golem_config("db_path")
      conn <- open_db_connection(db_path)
      throws_data <- get_throws_data(conn)
      

      fv_df <- predict_fv(fv_model_path, fv_preprocessing_info_path, throws_data)
      fv_df <- fv_df 

      cp_df <- predict_cp(cp_model_path, cp_preprocessing_info_path, throws_data)
      cp_df <- cp_df 
      advanced_stats_df <- merge(fv_df, cp_df, by = "throwID", all = FALSE)
      
      advanced_stats_df <- advanced_stats_df %>%
        mutate(insertTimestamp = format(
          lubridate::ymd_hms(gsub("Z$", "", Sys.time()), tz = "UTC"), 
          "%Y-%m-%d %H:%M:%S"
        ))
      
      create_table(conn=conn, table_name="advanced_stats", data=advanced_stats_df, index_col="gameID", override=TRUE)
      append_data(conn=conn, table_name='advanced_stats', data=advanced_stats_df, index_col="gameID")
      close_db_connection(conn)
      showNotification("Advanced Stats table successfully updated!", type = "message", duration = 3)
    })
    
  })
}
    
## To be copied in the UI
# mod_admin_models_ui("admin_models_1")
    
## To be copied in the server
# mod_admin_models_server("admin_models_1")
