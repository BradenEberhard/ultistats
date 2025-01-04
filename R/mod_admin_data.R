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
#' @importFrom googleCloudStorageR gcs_auth
#' @import dplyr
#' @noRd 
mod_admin_data_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    if (file.exists(".env")) {
      dotenv::load_dot_env(file = ".env")
    }

    token <- readRDS(Sys.getenv("GOOGLE_TOKEN_PATH"))
    gcs_auth(token=token)

    base_url <- Sys.getenv("BASE_API_PATH")

    # set reactive variables
    timestamps <- reactiveVal(NULL)
    tables <- reactive({
      conn <- open_db_connection()
      on.exit(close_db_connection(conn))
      table_names <- get_table_names(conn)
      if (is.null(table_names)) {
        return(NULL)
      }
      timestamps({sapply(table_names, function(table) {
        get_recent_timestamp(conn, table)
      })})
      tables <- data.frame(table_name = table_names, recent_timestamp = timestamps())
    })

    # create button observations
    create_observe_event(input, "update_pulls_button", update_pulls, timestamps, base_url)
    create_observe_event(input, "update_games_button", update_games, timestamps, base_url, "Processing Games")
    create_observe_event(input, "update_player_stats_button", update_player_stats, timestamps, base_url, "Processing Player Stats")
    create_observe_event(input, "update_players_button", update_players, timestamps, base_url, "Processing Players")
    create_observe_event(input, "update_teams_button", update_teams, timestamps, base_url, "Processing Teams")
    create_observe_event(input, "update_blocks_button", update_blocks, timestamps, base_url)
    create_observe_event(input, "update_penalties_button", update_penalties, timestamps, base_url)
    create_observe_event(input, "update_throws_button", update_throws, timestamps, base_url)
    create_observe_event(input, "update_all", update_all_tables, timestamps, base_url)


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