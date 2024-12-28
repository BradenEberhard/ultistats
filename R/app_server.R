#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
# Player Breakdown tab logic
  output$player_summary <- renderText({
    "This is where the Player Breakdown summary will be displayed."
  })
  
  # Team Breakdown tab logic
  output$team_summary <- renderText({
    "This is where the Team Breakdown summary will be displayed."
  })
  
  # Admin Panel tab logic

  mod_admin_data_server("admin_data")
  mod_admin_models_server("admin_model")
  mod_player_display_server("player_display")
  mod_team_display_server("team_display")
}