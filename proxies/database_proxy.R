#' Open a Database Connection
#'
#' This function opens a connection to a database. If the connection is already open,
#' it does nothing and returns the existing connection.
#'
#' @param db_path A string representing the path to the SQLite database file. This is
#' required for SQLite databases.
#'
#' @return An active database connection object.
#' @export
open_db_connection <- function(db_path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  return(conn)
}

#' Close the Database Connection
#'
#' This function closes the database connection if it is open. If the connection is
#' not open, it silently returns without doing anything.
#'
#' @return NULL
#' @export
close_db_connection <- function(conn) {
  if (!is.null(conn)) {
    DBI::dbDisconnect(conn)
  }
}

#' Get Table Names from the Database
#'
#' This function retrieves the names of all tables present in the SQLite database.
#'
#' @param conn An active database connection object.
#'
#' @return A vector of table names (character vector).
#' @export
get_table_names <- function(conn) {
  # Check if the connection is valid
  if (is.null(conn)) {
    stop("The database connection is not open.")
  }
  
  # Query the sqlite_master table to get all table names
  query <- "SELECT name FROM sqlite_master WHERE type = 'table';"
  result <- DBI::dbGetQuery(conn, query)
  
  # Return a vector of table names
  return(result$name)
}


#' Get the Most Recent Timestamp from a Table
#'
#' This function retrieves the most recent timestamp (from the insertTimestamp column) 
#' in a specific table.
#'
#' @param conn An active database connection object.
#' @param table A string representing the name of the table.
#'
#' @return The most recent timestamp (as POSIXct object).
#' @export
get_recent_timestamp <- function(conn, table) {
  query <- paste0("SELECT insertTimestamp FROM ", table, " ORDER BY insertTimestamp DESC LIMIT 1;")
  result <- DBI::dbGetQuery(conn, query)
  
  # Check if the result is empty
  if (nrow(result) == 0) {
    return(NA)  # Return NA if no timestamps are found
  }
  
  # Return the most recent timestamp
  return(result$insertTimestamp[1])
}

#' Get Unique Game IDs from the Games Table
#'
#' This function retrieves a list of unique game IDs from the 'games' table.
#'
#' @param conn An active database connection object.
#'
#' @return A vector of unique game IDs (character vector).
#' @export
get_game_ids <- function(conn) {
  # Check if the connection is valid
  if (is.null(conn)) {
    stop("The database connection is not open.")
  }
  
  # Query the games table to get unique gameID values
  query <- "SELECT DISTINCT gameID FROM games;"
  result <- DBI::dbGetQuery(conn, query)
  
  # Return the vector of unique gameIDs
  return(result$gameID)
}

#' Create a Table in the Database (Structure Only)
#'
#' This function creates a table in the database based on the structure of the provided data (without inserting any data).
#' If the table already exists, it will not stop execution.
#'
#' @param conn An active database connection object.
#' @param table_name A string representing the name of the table to create.
#' @param data A data frame (or tibble) containing the data structure to create the table.
#' @param index_cols A character vector of column names to create an index on (optional).
#' @param override A logical value (TRUE/FALSE). If TRUE, the existing table is dropped and replaced with the new one.
#'
#' @return NULL
#' @export
create_table <- function(conn, table_name, data, index_cols = NULL, override = FALSE) {
  # Check if the connection is valid
  if (is.null(conn)) {
    stop("The database connection is not open.")
  }
  
  # If override is TRUE, drop the table if it exists
  if (override) {
    DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ", table_name, ";"))
  }
  
  # Build the SQL query to create the table based on the column names of the provided data
  column_definitions <- paste(names(data), "TEXT", collapse = ", ")
  create_query <- paste0("CREATE TABLE IF NOT EXISTS ", table_name, " (", column_definitions, ");")
  DBI::dbExecute(conn, create_query)
  
  # Create the index if index_cols is provided
  if (!is.null(index_cols)) {
    index_cols <- if (!is.vector(index_cols)) c(index_cols) else index_cols
    index_query <- paste0("CREATE INDEX IF NOT EXISTS ", table_name, "_index ON ", table_name, "(", paste(index_cols, collapse = ", "), ");")
    DBI::dbExecute(conn, index_query)
  }
  
  return(NULL)
}


update_table <- function(conn, table_name, data, index_col, whole_table) {
  # Check if the index_col exists in the data
  if (!(index_col %in% colnames(data))) {
    stop(paste("Column", index_col, "does not exist in the provided data"))
  }
  
  # Extract the gameID value from the data (assuming all rows have the same gameID)
  game_id <- unique(data[[index_col]])
  
  # Ensure there's exactly one unique gameID in the data
  if (!whole_table && length(game_id) != 1) {
    stop(paste("Data must contain only one unique", index_col))
  }

  # Delete existing rows with the same gameID if not updating the whole table
  if (!whole_table) {
    delete_sql <- glue::glue("DELETE FROM {table_name} WHERE {index_col} = '{game_id}'")
    DBI::dbExecute(conn, delete_sql)
  } else {
    # Delete all data in the table if updating the whole table
    delete_sql <- glue::glue("DELETE FROM {table_name}")
    DBI::dbExecute(conn, delete_sql)
  }

  DBI::dbWriteTable(conn, table_name, data, append = TRUE, row.names = FALSE)
  invisible(TRUE)  # Return nothing, but indicate success
}

get_player_ids <- function(conn) {
  query <- "SELECT DISTINCT playerID FROM players"
  result <- DBI::dbGetQuery(conn, query)
  return(result$playerID)
}