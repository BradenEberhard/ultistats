#' player_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = sidebar(
        selectInput(ns("year_selector"), "Year", choices = NULL), 
        open=FALSE
      ),
      title = "Player Stats",
      create_stat_value_box(title = "Goals", output_id = "goals", ns = ns),
      create_stat_value_box(title = "Assists", output_id = "assists", ns = ns),
      create_stat_value_box(title = "Hockey Assists", output_id = "hockeyAssists", ns = ns),
      create_stat_value_box(title = "Completions", output_id = "completions", ns = ns),
      create_stat_value_box(title = "Turnovers", output_id = "throwaways", ns = ns),
      create_stat_value_box(title = "Receiving Yards (Since 2021)", output_id = "yardsReceived", ns = ns),
    )
  )
}

    
#' player_display Server Functions
#'
#' @noRd 
mod_player_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    player_id <- "beberhard"  # You can dynamically set this player ID based on user input or another source
    db_path <- get_golem_config("db_path")
    
    # Reactive expression to get player stats
    player_stats <- reactive({
      conn <- open_db_connection(db_path)
      stats <- get_player_stats(conn, player_id)  # Assuming this function fetches the player data
      close_db_connection(conn)
      stats
    })

    filtered_stats <- reactive({
      stats <- player_stats()
      if (input$year_selector == "Career") {
        return(stats)  # Return all stats for the player
      } else {
        return(stats[stats$year == input$year_selector, ])  # Filter stats by year
      }
    })
    
    output$goals <- render_stat("goals", filtered_stats)
    output$assists <- render_stat("assists", filtered_stats)
    output$hockeyAssists <- render_stat("hockeyAssists", filtered_stats)
    output$completions <- render_stat("completions", filtered_stats)
    output$throwaways <- render_stat("throwaways", filtered_stats)
    output$yardsReceived <- render_stat("yardsReceived", filtered_stats)

    observe({
      stats <- player_stats()
      updateSelectInput(session, "year_selector", choices = c("Career", sort(stats$year)), selected = "Career")
    })
  })
}

    
## To be copied in the UI
# mod_player_display_ui("player_display_1")
    
## To be copied in the server
# mod_player_display_server("player_display_1")
