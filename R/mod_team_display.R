#' team_display UI Functions
#'
#' @noRd 
mod_team_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = sidebar(
        title="Controls",
        selectizeInput(
          inputId = ns("team_selector"),
          label = "Team",
          choices = get_teams_names(),
          selected = "shred"
        ),
        selectInput(ns("year_selector"), "Year", choices = current_year <- c(2021:as.numeric(format(Sys.Date(), "%Y"))), selected = 2024)
      ),
      page_fluid(
        verbatimTextOutput(ns("selected_team_id")),

      )
    )
  )
}

#' team_display Server Functions
#'
#' @noRd 
mod_team_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Display the selected team ID
    output$selected_team_id <- renderPrint({
      req(input$team_selector) # Ensure input is available
      pool <- get_db_pool()
      players <- get_team_players(pool, input$team_selector, input$year_selector)$fullName
      paste("Players: ", players)
    })
  })
}
