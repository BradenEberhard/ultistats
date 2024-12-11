#' Open a Database Connection
#'
#' @description
#' This function opens a connection to an SQLite database specified by the given path.
#'
#' @param db_path A string representing the path to the SQLite database.
#' 
#' @return A database connection object.
#' @export
#'
#' @examples
#' conn <- open_db_connection("path/to/database.db")
open_db_connection <- function(db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- get_golem_config("db_path")
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  return(conn)
}

#' Close a Database Connection
#'
#' @description
#' This function closes the specified database connection.
#'
#' @param conn A database connection object to be closed.
#' 
#' @return NULL
#' @export
#'
#' @examples
#' close_db_connection(conn)
close_db_connection <- function(conn) {
  DBI::dbDisconnect(conn)
}

#' Write Data to a Database Table
#'
#' @description
#' This function writes data to a specified table in the database.
#'
#' @param conn A database connection object.
#' @param table_name A string representing the name of the table.
#' @param data A data frame to be written to the table.
#' @param overwrite A logical value indicating whether to overwrite the table if it exists (default is TRUE).
#' 
#' @return A character string indicating whether the operation was successful.
#' @export
#'
#' @examples
#' write_db_table("games", games_df)
write_db_table <- function(table_name, data, overwrite = TRUE) {
  conn <- open_db_connection()
  DBI::dbWriteTable(conn, name = table_name, value = data, row.names = FALSE, overwrite = overwrite)
  close_db_connection(conn)
  return(paste("Table", table_name, "has been written to the database."))
}

#' Delete a Database Table
#'
#' @description
#' This function deletes the specified table from the database.
#'
#' @param conn A database connection object.
#' @param table_name A string representing the name of the table to be deleted.
#' 
#' @return A character string indicating whether the operation was successful.
#' @export
#'
#' @examples
#' delete_db_table(conn, "games")
delete_db_table <- function(table_name) {
  conn <- open_db_connection()
  DBI::dbExecute(conn, paste("DROP TABLE IF EXISTS", table_name))
  close_db_connection(conn)
  return(paste("Table", table_name, "has been deleted from the database."))
}

#' Update Table in Database
#'
#' @description A function to update a specified table in the database by removing existing rows with a matching `gameID` and appending new data.
#'
#' @param conn The database connection object.
#' @param table_name The name of the table to update (e.g., "throws").
#' @param game_data The new data to append to the table (data frame).
#' @param game_id_column The column name for `gameID` (defaults to "gameID").
#' @param current_game_id The value of the `gameID` to delete from the table before appending new data.
#'
#' @return A message indicating whether the operation was successful or not.
#'
#' @import DBI
update_table <- function(table_name, game_data, game_id_column = "gameID", current_game_id) {
  conn <- open_db_connection()
  if (DBI::dbExistsTable(conn, table_name)) {
    DBI::dbExecute(conn, paste0("DELETE FROM ", table_name, " WHERE ", game_id_column, " = ?"), params = list(current_game_id))
  }
  DBI::dbWriteTable(conn, table_name, game_data, append = TRUE, row.names = FALSE)
  close_db_connection(conn)
}

