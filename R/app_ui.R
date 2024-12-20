#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(
    title = "Game Dashboard",
    theme = bslib::bs_theme(
      preset = "lux",   # Choose the theme (e.g., "lux", "cosmo", "flatly", etc.)
      version = 5       # Ensure you're using Bootstrap 5
    ),
    
    # Add external resources if needed
    golem_add_external_resources(),
    
    # Team Section
    bslib::nav_panel(
      title = "Team",
      value = "team"
    ),
    bslib::nav_panel(
      title = "Player",
      value = "player",
      icon = shiny::icon("chart-line")
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
