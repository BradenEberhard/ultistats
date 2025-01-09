#' player_leaderboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_leaderboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = sidebar(
        title="Controls",
        selectInput(ns("year_selector"), "Year", choices = current_year <- c(2021:as.numeric(format(Sys.Date(), "%Y")), "Career"), selected = 2024),
        selectInput(ns("stat_category"), "Category", choices = c("Total", "Per 100 Possessions"), selected = "Total"),
        selectizeInput(inputId = ns("metric_selector"), label = "Metric", choices = get_table_choices(), selected = "Plus Minus")
      ),
      page_fluid(
        layout_column_wrap(
          card(DT::dataTableOutput(ns("grade_table")), full_screen=TRUE) |> withSpinner() |> bslib::as_fill_carrier(),
          card(DT::dataTableOutput(ns("metrics_table")), full_screen=TRUE) |> withSpinner() |> bslib::as_fill_carrier()
        )
      )
    )
  )
}
    
#' player_leaderboard Server Functions
#'
#' @noRd 
mod_player_leaderboard_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    conn <- open_db_connection()
    session$onSessionEnded(function() {close_db_connection(conn)})

    all_player_stats <- get_all_player_stats(conn)  
    
    output$metrics_table <- DT::renderDataTable({
      get_metric_table(input, all_player_stats)
    })

    output$grade_table <- DT::renderDataTable({
      get_grade_table(input, all_player_stats)
    })

  })
}
    
## To be copied in the UI
# mod_player_leaderboard_ui("player_leaderboard_1")
    
## To be copied in the server
# mod_player_leaderboard_server("player_leaderboard_1")
