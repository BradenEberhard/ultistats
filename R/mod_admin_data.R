#' admin_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
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
        actionButton(ns("update_penalties_button"), "Update Penalties Table", class = "btn btn-primary")
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

    source("./proxies/api_proxy.R")
    source("./proxies/database_proxy.R")
    source("./classes/game_events.R")
    source("./classes/event_handlers.R")
    source("./classes/team_events.R")
    base_url <- get_golem_config("base_api_path")
    db_path <- get_golem_config("db_path")

    observeEvent(input$update_pulls_button, {
      conn <- open_db_connection(db_path)
      update_pulls(conn, base_url)
      close_db_connection(conn)
      timestamps(NULL)
    })
    
    observeEvent(input$update_games_button, {
      withProgress(message = 'Processing Games', value = 0, {
        conn <- open_db_connection(db_path)
        update_games(conn, base_url)
        close_db_connection(conn)
        timestamps(NULL)
      })
    })

    observeEvent(input$update_player_stats_button, {
      withProgress(message = 'Processing Player Stats', value = 0, {
        conn <- open_db_connection(db_path)
        update_player_stats(conn, base_url)
        close_db_connection(conn)
        timestamps(NULL)
      })
    })

    observeEvent(input$update_players_button, {
      withProgress(message = 'Processing Players', value = 0, {
        conn <- open_db_connection(db_path)
        update_players(conn, base_url)
        close_db_connection(conn)
        timestamps(NULL)
      })
    })

    observeEvent(input$update_teams_button, {
      withProgress(message = 'Processing Teams', value = 0, {
        conn <- open_db_connection(db_path)
        update_teams(conn, base_url)
        close_db_connection(conn)
        timestamps(NULL)
      })
    })

    observeEvent(input$update_blocks_button, {
      conn <- open_db_connection(db_path)
      update_blocks(conn, base_url)
      close_db_connection(conn)
      timestamps(NULL)
    })

    observeEvent(input$update_penalties_button, {
      conn <- open_db_connection(db_path)
      update_penalties(conn, base_url)
      close_db_connection(conn)
      timestamps(NULL)
    })

    observeEvent(input$update_throws_button, {
      conn <- open_db_connection(db_path)
      update_throws(conn, base_url)
      close_db_connection(conn)
      timestamps(NULL)
    })

    observeEvent(input$update_all, {
      withProgress(message = 'Processing All Tables', value = 0, {
        incProgress(0, detail = "Team Data ... Processing       Game Data ... Waiting       Pull Data ... Waiting")
        conn <- open_db_connection(db_path)
        update_games(conn, base_url)
        incProgress(0.25, detail = "Team Data ... Complete       Game Data ... Processing       Pull Data ... Waiting")
        update_teams(conn, base_url)
        incProgress(0.75, detail = "Team Data ... Complete       Game Data ... Complete       Pull Data ... Processing")
        update_pulls(conn, base_url)
        incProgress(0.75, detail = "Team Data ... Complete       Game Data ... Complete       Pull Data ... Processing")
        update_blocks(conn, base_url)
        incProgress(0.75, detail = "Team Data ... Complete       Game Data ... Complete       Pull Data ... Processing")
        update_penalties(conn, base_url)
        close_db_connection(conn)
        timestamps(NULL)
        incProgress(1, detail = "Team Data ... Complete       Game Data ... Complete       Pull Data ... Complete")
      })
    })

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