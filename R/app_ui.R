#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
app_ui <- function(request) {

  
  golem_add_external_resources()
  bslib::page_navbar(
    title = "Game Dashboard",
    id = "main_nav",
    theme = bslib::bs_theme(
      preset = "darkly",   # Choose the theme (e.g., "lux", "cosmo", "flatly", etc.)
      version = 5       # Ensure you're using Bootstrap 5
    ),
    nav_menu(
      title = "Player", 
      value = "player", 
      icon = shiny::icon("chart-line"),
      nav_panel(
        title = "Profile",
        value = "player_profile",
        mod_player_display_ui("player_profile")
      ),
      nav_panel(
        title = "Leaderboard",
        value = "player_leaderboard",
        mod_player_leaderboard_ui("player_leaderboard")
      )
    ),
    nav_panel(
      title = "Team", 
      value = "team", 
      icon = shiny::icon("users"),
      mod_team_display_ui("team_display")
    ),
    nav_panel(
      title = "Admin", 
      value = "admin", 
      icon = shiny::icon("cogs"),
      mod_admin_data_ui("admin_data")
    ),
    nav_panel(
      title = "Home", 
      value = "home", 
      icon = shiny::icon("cogs"),
      mod_home_page_ui("home_page")
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ultistats"
    )
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/card-reveal-full-screen.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
