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
    theme = bslib::bs_theme(
      preset = "lux",   # Choose the theme (e.g., "lux", "cosmo", "flatly", etc.)
      version = 5       # Ensure you're using Bootstrap 5
    ),
    # First nav_panel: Player
    nav_panel(
      title = "Player", 
      value = "player", 
      icon = shiny::icon("chart-line"),
      mod_player_display_ui("player_display")
    ),
    # Second nav_panel: Team
    nav_panel(
      title = "Team", 
      value = "team", 
      icon = shiny::icon("users")
    ),
    # Admin section using nav_menu
    nav_menu(
      title = "Admin",
      value = "admin",
      icon = shiny::icon("cogs"),
      nav_panel(
        title = "Refresh Data", 
        value = "refresh_data", 
        icon = shiny::icon("refresh"),
        mod_admin_data_ui("admin_data")
      ),
      nav_panel(
        title = "Models", 
        value = "models", 
        icon = shiny::icon("project-diagram"),
        mod_admin_models_ui("admin_model")
      )
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
