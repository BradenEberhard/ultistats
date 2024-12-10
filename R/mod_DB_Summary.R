#' DB_Summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_DB_Summary_ui <- function(id) {
  ns <- NS(id)  # Namespace for module IDs
  tagList(
    h2("Database Summary"),
    
    # Dropdown menu to select the table
    selectInput(ns("table_select"), 
                label = "Select a Table", 
                choices = NULL),  # Choices will be populated in the server
    
    uiOutput(ns("date_range_ui")),
    uiOutput(ns("team_filter_ui")),
    # A table output to display the selected table data
    DT::dataTableOutput(ns("db_summary")),

    downloadButton(ns("download_csv"), label = "Download CSV", class = "btn-outline-primary")
  )
}
    
#' DB_Summary Server Functions
#'
#' @noRd 
mod_DB_Summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {  # moduleServer should wrap your logic
    db_path <- get_golem_config("db_path")
    
    get_tables <- function() {
      conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      tables <- DBI::dbListTables(conn)
      DBI::dbDisconnect(conn)
      return(tables)
    }
    
    tables = get_tables()
    # Update the dropdown choices with the table names
    updateSelectInput(session, "table_select", choices = tables)
    
    # Reactive expression to fetch data from the selected table
    db_summary <- reactive({
      conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      req(input$table_select)  # Ensure table is selected
      query <- paste("SELECT * FROM", input$table_select)
      if (input$table_select == "games" && !is.null(input$date_range)) {
        start_date <- as.Date(input$date_range[1])
        end_date <- as.Date(input$date_range[2])
        
        # Add a WHERE clause to filter by date range
        query <- paste(query, 
                       "WHERE startTimestamp BETWEEN", 
                       paste("'", start_date, "'", sep = ""),
                       "AND", paste("'", end_date, "'", sep = ""))
      }
      if (input$table_select == "games" && !is.null(input$team_filter)) {
        # Filter the query to include rows where either awayteamID or hometeamID matches selected teams
        selected_teams <- paste0("'", input$team_filter, "'", collapse = ", ")
        query <- paste(query, 
                       "AND (awayteamID IN (", selected_teams, ") OR hometeamID IN (", selected_teams, "))")
      }
      display_table <- DBI::dbGetQuery(conn, query)  # Query the selected table
      DBI::dbDisconnect(conn)
      return(display_table)
    })

    # Send the database summary to the UI
    output$db_summary <- DT::renderDataTable({
      db_summary()
    })

    output$date_range_ui <- renderUI({
      ns <- NS(id)
      if (input$table_select == "games") {
        # Return the UI element for date range filter (e.g., date range input)
        return(dateRangeInput(ns("date_range"), 
                             label = "Filter Date Range", 
                             start = "2021-01-01",  # Default start date
                             end = Sys.Date()))  # Default end date
      } else {
        # Return NULL if "games" table is not selected
        return(NULL)
      }
    })
    output$team_filter_ui <- renderUI({
      ns <- NS(id)
      if (input$table_select == "games") {
        # Fetch unique team IDs from both awayteamID and hometeamID columns
        conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        team_query <- "SELECT DISTINCT awayteamID AS team FROM games UNION SELECT DISTINCT hometeamID AS team FROM games"
        teams <- DBI::dbGetQuery(conn, team_query)$team
        DBI::dbDisconnect(conn)
        
        # Create a multi-select dropdown for team selection
        return(selectInput(ns("team_filter"), 
                           label = "Select Teams", 
                           choices = teams, 
                           selected = NULL, 
                           multiple = TRUE))
      } else {
        return(NULL)
      }
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste("filtered_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Write the filtered data (db_summary()) to a CSV file
        write.csv(db_summary(), file, row.names = FALSE)
      }
    )
  })
}

