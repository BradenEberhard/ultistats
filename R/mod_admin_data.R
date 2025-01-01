#' admin_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import bslib
#' @importFrom shiny NS tagList 
mod_admin_data_ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    card(
      card_header("Refresh Tables"),
      layout_columns(
        actionButton(ns("update_all"), "Update All Tables", class = "btn btn-primary"),
        actionButton(ns("update_player_stats_button"), "Update Players Stats Table", class = "btn btn-primary"),
        actionButton(ns("update_players_button"), "Update Players Table", class = "btn btn-primary"),
        actionButton(ns("update_games_button"), "Update Games Table", class = "btn btn-primary"),
        actionButton(ns("update_teams_button"), "Update Teams Table", class = "btn btn-primary"),
        actionButton(ns("update_blocks_button"), "Update Blocks Table", class = "btn btn-primary"),
        actionButton(ns("update_pulls_button"), "Update Pulls Table", class = "btn btn-primary"),
        actionButton(ns("update_throws_button"), "Update Throws Table", class = "btn btn-primary"),
        actionButton(ns("update_penalties_button"), "Update Penalties Table", class = "btn btn-primary"),
        actionButton(ns("update_advanced_stats_button"), "Update Advanced Stats", class = "btn btn-primary")
      )
    ),
    card(
      card_header("Most Recent Updates"),
      uiOutput(ns("table_card_ui"))
    )
  )
}

    
#' admin_page Server Functions
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @import dplyr
#' @noRd 
mod_admin_data_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # set base variables
    source("./proxies/api_proxy.R")
    source("./proxies/database_proxy.R")
    source("./classes/game_events.R")
    source("./classes/event_handlers.R")
    source("./classes/team_events.R")
    base_url <- get_golem_config("base_api_path")
    db_path <- get_golem_config("db_path")
    conn_params <- list(db_path = db_path, base_url = base_url)

    # set reactive variables
    timestamps <- reactiveVal(NULL)
    tables <- reactive({
      db_path <- get_golem_config("db_path")
      conn <- open_db_connection(db_path)
      table_names <- get_table_names(conn)
      timestamps({sapply(table_names, function(table) {
        get_recent_timestamp(conn, table)
      })})
      close_db_connection(conn)
      tables <- data.frame(table_name = table_names, recent_timestamp = timestamps())
    })

    # create button observations
    create_observe_event(input, "update_pulls_button", update_pulls, timestamps, conn_params = conn_params)
    create_observe_event(input, "update_games_button", update_games, timestamps, "Processing Games", conn_params = conn_params)
    create_observe_event(input, "update_player_stats_button", update_player_stats, timestamps, "Processing Player Stats", conn_params = conn_params)
    create_observe_event(input, "update_players_button", update_players, timestamps, "Processing Players", conn_params = conn_params)
    create_observe_event(input, "update_teams_button", update_teams, timestamps, "Processing Teams", conn_params = conn_params)
    create_observe_event(input, "update_blocks_button", update_blocks, timestamps, conn_params = conn_params)
    create_observe_event(input, "update_penalties_button", update_penalties, timestamps, conn_params = conn_params)
    create_observe_event(input, "update_throws_button", update_throws, timestamps, conn_params = conn_params)
    create_observe_event(input, "update_advanced_stats_button", update_advanced_stats, timestamps, "Processing Advanced Stats", conn_params = conn_params)
    create_observe_event(input, "update_all", update_all_tables, timestamps, conn_params = conn_params)


    table_cards <- reactive({
      tables_and_timestamps <- tables()
      lapply(1:nrow(tables_and_timestamps), function(i) {
        bslib::card(
          card_header(tools::toTitleCase(gsub("_", " ", tables_and_timestamps$table_name[i]))),
          card_body(paste("Most Recent Update: ", tables_and_timestamps$recent_timestamp[i])),
          theme = "secondary"
        )
      })
    })

    output$table_card_ui <- renderUI({
      layout_columns(
        width = 1/2,
        class = "primary",
        !!!table_cards()
      )
    })
  })
}