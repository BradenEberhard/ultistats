#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_admin_data_server("admin_data")
  mod_player_display_server("player_display")
  mod_team_display_server("team_display")
}