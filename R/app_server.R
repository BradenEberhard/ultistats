#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_Data_Summary_server("Data_Summary")
  mod_Update_Games_Table_server("Update_Games_Table")
  mod_Update_Throws_Table_server("Update_Throws_Table")
}
