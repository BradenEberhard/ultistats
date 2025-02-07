#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_admin_data_server("admin_data")
  player_link_name <- mod_player_leaderboard_server("player_leaderboard")
  mod_team_display_server("team_display")
  mod_player_display_server("player_profile", player_link_name)
  mod_home_page_server("home_page")
  observeEvent(player_link_name(), {
    if(player_link_name() != ""){
      updateTabsetPanel(session, 'main_nav', selected = 'player_profile')
    }
  })  
}