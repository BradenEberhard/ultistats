
open_db_connection <- function(db_path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  return(conn)
}


close_db_connection <- function(conn) {
  if (!is.null(conn)) {
    DBI::dbDisconnect(conn)
  }
}


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


create_table <- function(conn, table_name, data, index_cols = NULL, override = FALSE) {
  timestamp_cols <- grep("Timestamp", names(data), ignore.case = TRUE, value = TRUE)
  for (col in timestamp_cols) {
    data[[col]] <- as.POSIXct(data[[col]], format = "%Y-%m-%d %H:%M:%S")
  }
  # Check if the connection is valid
  if (is.null(conn)) {
    stop("The database connection is not open.")
  }
  
  # If override is TRUE, drop the table if it exists
  if (override) {
    DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ", table_name, ";"))
  }
  
  # Function to map R types to SQL types
  map_r_to_sql_type <- function(r_type) {
    switch(r_type,
           "logical" = "BOOLEAN",
           "integer" = "INTEGER",
           "numeric" = "REAL",
           "double" = "REAL",
           "character" = "TEXT",
           "factor" = "TEXT",
           "Date" = "DATE",
           "POSIXct" = "DATETIME",  # For timestamp data types
           stop("Unsupported column type: ", r_type)
    )
  }
  
  # Get the column names and types for the data
  column_definitions <- sapply(names(data), function(col_name) {
    col_type <- map_r_to_sql_type(class(data[[col_name]])[1])  # Get the first class of the column
    paste(col_name, col_type)
  })
  
  # Create the SQL query to create the table
  create_query <- paste0("CREATE TABLE IF NOT EXISTS ", table_name, " (", paste(column_definitions, collapse = ", "), ");")
  DBI::dbExecute(conn, create_query)
  
  # Create the index if index_cols is provided
  if (!is.null(index_cols)) {
    index_cols <- if (!is.vector(index_cols)) c(index_cols) else index_cols
    index_query <- paste0("CREATE INDEX IF NOT EXISTS ", table_name, "_index ON ", table_name, "(", paste(index_cols, collapse = ", "), ");")
    DBI::dbExecute(conn, index_query)
  }
  
  return(NULL)
}


#' @importFrom glue glue
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
    delete_sql <- glue("DELETE FROM {table_name} WHERE {index_col} = '{game_id}'")
    DBI::dbExecute(conn, delete_sql)
  } else {
    # Delete all data in the table if updating the whole table
    delete_sql <- glue("DELETE FROM {table_name}")
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


get_throws_data <- function(conn, table_name = "throws") {
  # Check if the connection is valid
  if (is.null(conn)) {
    stop("The database connection is not open.")
  }
  
  # Query the throws table to retrieve all data
  query <- glue("SELECT * FROM {table_name};")
  result <- DBI::dbGetQuery(conn, query)
  
  # Return the result as a data frame
  return(result)
}

get_player_stats <- function(conn, player_id) {
  # SQL query to fetch player stats by playerID
  query <- glue("SELECT * FROM player_stats WHERE playerID = '{player_id}'")
  
  # Execute the query and return the result
  player_stats <- DBI::dbGetQuery(conn, query)
  
  return(player_stats)
}

get_player_passes_by_role <- function(conn, player_id, role = "thrower") {
  # SQL query to fetch player stats by playerID with a join on advanced_stats
  query <- glue("
    SELECT t.*, a.* 
    FROM throws t
    LEFT JOIN advanced_stats a ON t.throwID = a.throwID
    WHERE t.{role} = '{player_id}'
  ")
  
  # Execute the query and return the result
  player_passes <- DBI::dbGetQuery(conn, query)
  player_passes <- player_passes[, !duplicated(names(player_passes))]
  return(player_passes)
}

get_all_player_stats <- function(conn) {
  # SQL query to fetch player stats by playerID
  query <- glue("SELECT * FROM player_stats WHERE year >= 2021")
  
  # Execute the query and return the result
  all_player_stats <- DBI::dbGetQuery(conn, query)
  
  return(all_player_stats)
}


get_table <- function(conn, table_name) {
  # Check if the connection is valid
  if (is.null(conn)) {
    stop("The database connection is not open.")
  }
  
  # Query the throws table to retrieve all data
  query <- glue("SELECT * FROM {table_name};")
  result <- DBI::dbGetQuery(conn, query)
  
  # Return the result as a data frame
  return(result)
}

get_team_id <- function(conn, player_id, year) {
  # Ensure the connection is valid
  if (is.null(conn)) {
    stop("Database connection is null.")
  }
  
  # Query the database
  query <- glue(
    "SELECT teamID 
     FROM players 
     WHERE playerID = {DBI::dbQuoteLiteral(conn, player_id)} 
       AND year = {DBI::dbQuoteLiteral(conn, year)}"
  )
  
  result <- DBI::dbGetQuery(conn, query)
  
  # Return the teamID, or NULL if no match
  if (nrow(result) > 0) {
    return(result$teamID[1])
  } else {
    warning("No matching teamID found for the given player_id and year.")
    return(NULL)
  }
}
