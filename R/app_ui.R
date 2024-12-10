#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Add external resources if needed
    golem_add_external_resources(),
    
    # Shiny page layout with a `fluidPage` and a bslib theme
    fluidPage(
      theme = bslib::bs_theme(
        preset = "lux",   # Choose the theme (e.g., "lux", "cosmo", "flatly", etc.)
        version = 5       # Ensure you're using Bootstrap 5
      ),
      
      # Page title
      titlePanel("Game Dashboard"),
      mainPanel(
        # Main content area
        tabsetPanel(
          tabPanel("Live Games",
                    fluidRow(
                      h2("Data Section"),
                      mod_DB_Summary_ui("DB_Summary")
                    )),
          tabPanel("Admin",
                    fluidRow(
                      mod_API_Reload_ui("API_Reload")
                      # Add UI elements for player rankings here
                    ))
        )
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
