# Function to open the database connection
open_db_connection <- function() {
  # Get credentials from environment variables
  db_host <- Sys.getenv("DB_HOST")
  db_port <- Sys.getenv("DB_PORT")
  db_name <- Sys.getenv("DB_NAME")
  db_user <- Sys.getenv("DB_USER")
  db_password <- Sys.getenv("DB_PASSWORD")
  
  # Create the connection string
  conn <- DBI::dbConnect(Postgres(),
                         host = db_host,
                         port = db_port,
                         dbname = db_name,
                         user = db_user,
                         password = db_password)
  
  return(conn)
}

# Function to close the database connection
close_db_connection <- function(conn) {
  if (!is.null(conn)) {
    DBI::dbDisconnect(conn)
  }
}

#' @importFrom RPostgres Postgres
#' @importFrom pool poolClose
get_db_pool <- local({
  pool <- NULL
  function() {
    if (is.null(pool)) {
      # Initialize the pool if it hasn't been created
      pool <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = Sys.getenv("DB_NAME"),
        host = Sys.getenv("DB_HOST"),
        port = Sys.getenv("DB_PORT"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD")
      )
      # Ensure the pool is closed when the app stops
      onStop(function() {
        poolClose(pool)
      })
    }
    pool
  }
})


get_table_names <- function(pool) {
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  query <- "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';"
  result <- DBI::dbGetQuery(pool, query)
  
  if (nrow(result) == 0) {
    message("No tables found in the database.")
    return(NULL)  # Return NULL if no tables are found
  }
  
  return(result$table_name)  # Return the table names
}




get_recent_timestamp <- function(pool, table) {
  column_check_query <- paste0("SELECT column_name FROM information_schema.columns WHERE table_name = '", table, "' AND column_name = 'insertTimestamp';")
  column_check_result <- DBI::dbGetQuery(pool, column_check_query)
  
  # If the column does not exist, return an error message
  if (nrow(column_check_result) == 0) {
    return("insertTimestamp column does not exist in the table")
  }

  query <- paste0("SELECT \"insertTimestamp\" FROM \"", table, "\" ORDER BY \"insertTimestamp\" DESC LIMIT 1;")
  result <- DBI::dbGetQuery(pool, query)
  
  
  # Check if the result is empty
  if (nrow(result) == 0) {
    return("Empty Table")  # Return message if no timestamps are found
  }
  
  # Convert the timestamp to a human-readable date
  timestamp <- result$insertTimestamp[1]
  
  # If the timestamp is in Unix format (e.g., 1735940381), convert it
  readable_date <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
  
  # Format the date to a more human-readable format
  return(format(readable_date, "%Y-%m-%d %H:%M:%S"))
}

get_game_ids <- function(pool) {
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  # Query the games table to get unique gameID values
  query <- "SELECT DISTINCT \"gameID\" FROM \"games\" WHERE \"status\" = 'Final'"
  result <- DBI::dbGetQuery(pool, query)
  
  # Return the vector of unique gameIDs
  return(result$gameID)
}


create_table <- function(pool, table_name, data, index_cols = NULL, override = FALSE, primary_key_col = NULL) {
  # Convert timestamp columns to POSIXct
  timestamp_cols <- grep("Timestamp", names(data), ignore.case = TRUE, value = TRUE)
  for (col in timestamp_cols) {
    data[[col]] <- as.POSIXct(data[[col]], format = "%Y-%m-%d %H:%M:%S")
  }
  
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  # If override is TRUE, drop the table if it exists
  if (override) {
    DBI::dbExecute(pool, paste0("DROP TABLE IF EXISTS \"", table_name, "\";"))
  }
  
  # Function to map R types to PostgreSQL types
  map_r_to_sql_type <- function(r_type) {
    switch(r_type,
           "logical" = "BOOLEAN",
           "integer" = "INTEGER",
           "numeric" = "REAL",
           "double" = "REAL",
           "character" = "TEXT",
           "factor" = "TEXT",
           "Date" = "DATE",
           "POSIXct" = "TIMESTAMP",  # For timestamp data types
           stop("Unsupported column type: ", r_type)
    )
  }
  
  # Get the column names and types for the data
  column_definitions <- sapply(names(data), function(col_name) {
    col_type <- map_r_to_sql_type(class(data[[col_name]])[1])  # Get the first class of the column
    paste0('"', col_name, '"', " ", col_type)  # Add double quotes around the column name
  })

  if (!is.null(primary_key_col)) {
    column_definitions <- c(column_definitions, paste0('"', primary_key_col, '"', " SERIAL PRIMARY KEY"))
  }
  
  # Create the SQL query to create the table
  create_query <- paste0("CREATE TABLE IF NOT EXISTS \"", table_name, "\" (", paste(column_definitions, collapse = ", "), ");")
  DBI::dbExecute(pool, create_query)
  
  # Function to check if index exists
  check_index_exists <- function(pool, table_name, index_cols) {
    # Convert index columns into a format suitable for the query
    index_cols <- paste0("\"", index_cols, "\"")
    
    # Create the index name based on the table and columns
    index_name <- paste0(table_name, "_", paste(index_cols, collapse = "_"), "_index")
    
    # Query the system catalog to check if the index already exists
    query <- paste0("SELECT 1 FROM pg_indexes WHERE tablename = '", table_name, "' AND indexname = '", index_name, "';")
    result <- DBI::dbGetQuery(pool, query)
    
    # Return TRUE if the index exists, FALSE otherwise
    return(nrow(result) > 0)
  }
  
  # Function to create the index
  create_index <- function(pool, table_name, index_cols) {
    # Check if the index already exists
    if (!check_index_exists(pool, table_name, index_cols)) {
      # Create the index if it doesn't exist
      index_cols <- paste0("\"", index_cols, "\"")
      index_query <- paste0("CREATE INDEX IF NOT EXISTS \"", table_name, "_index\" ON \"", table_name, "\"(", paste(index_cols, collapse = ", "), ");")
      DBI::dbExecute(pool, index_query)
    } else {
      message("Index already exists.")
    }
  }
  
  # Create the index if index_cols is provided
  if (!is.null(index_cols)) {
    index_cols <- if (!is.vector(index_cols)) c(index_cols) else index_cols
    create_index(pool, table_name, index_cols)
  }
  
  return(NULL)
}




#' @importFrom glue glue
update_table <- function(pool, table_name, data, index_col, whole_table) {
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
    delete_sql <- glue("DELETE FROM {table_name} WHERE \"{index_col}\" = '{game_id}'")
    DBI::dbExecute(pool, delete_sql)
  } else {
    # Delete all data in the table if updating the whole table
    delete_sql <- glue("DELETE FROM {table_name}")
    DBI::dbExecute(pool, delete_sql)
  }

  # Insert the new data into the table
  DBI::dbWriteTable(pool, table_name, data, append = TRUE, row.names = FALSE)
  
  invisible(TRUE)  # Return nothing, but indicate success
}


get_player_ids <- function(pool) {
  query <- "SELECT DISTINCT \"playerID\" FROM \"players\""
  result <- DBI::dbGetQuery(pool, query)
  return(result$playerID)
}

#' @importFrom glue glue_sql
get_throws_from_db <- function(pool, gameID) {
  query <- glue_sql(
    "SELECT * FROM \"throws\" 
     {if (!is.null(gameID)) glue_sql('WHERE \"gameID\" = {gameID}', .con = pool)}",
    .con = pool
  )
  result <- DBI::dbGetQuery(pool, query)
  return(result)
}

get_game_prob <- function(pool, table1_name, table2_name, join_column, gameID) {
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  # Construct the SQL query to join the two tables on the specified column
  query <- glue("SELECT game_quarter, graphing_win_prob, game_time_left, 
                        time_left, is_home_team, 
                        thrower_y, home_team_score, away_team_score, 
                        possession_num, possession_throw
                 FROM \"{table1_name}\" t1
                 LEFT JOIN \"{table2_name}\" t2
                 ON t1.\"{join_column}\" = t2.\"{join_column}\"
                 WHERE t1.\"gameID\" = '{gameID}';")
  
  # Execute the query
  result <- DBI::dbGetQuery(pool, query)
  
  # Return the result as a data frame
  return(result)
}



get_table_from_db <- function(pool, table_name = "throws") {
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  # Query the throws table to retrieve all data
  query <- glue("SELECT * FROM {table_name};")
  result <- DBI::dbGetQuery(pool, query)
  
  # Return the result as a data frame
  return(result)
}

get_player_stats <- function(pool, player_id) {
  # SQL query to fetch player stats by playerID
  query <- glue("SELECT * FROM player_stats WHERE \"playerID\" = '{player_id}'")
  
  # Execute the query and return the result
  player_stats <- DBI::dbGetQuery(pool, query)
  
  return(player_stats)
}

get_player_passes_by_role <- function(pool, player_id, role = "thrower") {
  # SQL query to fetch player stats by playerID with a join on advanced_stats
  query <- glue("
    SELECT t.*, a.* 
    FROM throws t
    LEFT JOIN advanced_stats a ON t.\"throwID\" = a.\"throwID\"
    WHERE t.{role} = '{player_id}'
  ")
  
  # Execute the query and return the result
  player_passes <- DBI::dbGetQuery(pool, query)
  player_passes <- player_passes[, !duplicated(names(player_passes))]
  return(player_passes)
}

get_all_player_stats <- function(pool) {
  # SQL query to fetch player stats by playerID
  query <- glue("SELECT * FROM player_stats")

  # Execute the query and return the result
  all_player_stats <- DBI::dbGetQuery(pool, query)
  return(all_player_stats)
}


get_table <- function(pool, table_name) {
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  # Query the throws table to retrieve all data
  query <- glue("SELECT * FROM {table_name};")
  result <- DBI::dbGetQuery(pool, query)
  
  # Return the result as a data frame
  return(result)
}

get_game_info <- function(pool, game_id) {
  # Check if the connection is valid
  if (is.null(pool)) {
    stop("The database connection is not open.")
  }
  
  # Ensure game_id is safely quoted to prevent SQL injection
  query <- glue::glue_sql('SELECT * FROM games WHERE "gameID" = {game_id};', .con = pool)
  
  # Execute the query
  result <- DBI::dbGetQuery(pool, query)
  
  # Return the result as a data frame
  return(result)
}

get_team_id <- function(pool, player_id, year) {
  # Ensure the connection is valid
  if (is.null(pool)) {
    stop("Database connection is null.")
  }
  
  # Query the database
  query <- glue(
    "SELECT teamID 
     FROM players 
     WHERE playerID = {DBI::dbQuoteLiteral(pool, player_id)} 
       AND year = {DBI::dbQuoteLiteral(pool, year)}"
  )
  
  result <- DBI::dbGetQuery(pool, query)
  
  # Return the teamID, or NULL if no match
  if (nrow(result) > 0) {
    return(result$teamID[1])
  } else {
    warning("No matching teamID found for the given player_id and year.")
    return(NULL)
  }
}

get_model_data_from_db <- function(filename) {
  pool <- open_db_connection()
  on.exit(close_db_connection(pool))
  # Query to get the file data from the database
  query <- "SELECT file_data FROM model_files WHERE filename = $1"
  
  # Fetch the file data as raw bytes
  result <- DBI::dbGetQuery(pool, query, params = list(filename))
  
  if (nrow(result) == 0) {
    stop("File not found in the database.")
  }
  
  # Extract the file data (assuming it's the first column, modify if necessary)
  file_data <- result$file_data[[1]]
  
  # Check file extension and handle accordingly
  if (grepl("\\.model$", filename)) {
    # If the file ends with .model, deserialize as an XGBoost model
    model <- unserialize(file_data)
    model <- xgboost::xgb.Booster.complete(model)  # Complete the model if needed
    return(model)
  } else if (grepl("\\.rds$", filename)) {
    # If the file ends with .rds, unserialize as an RDS object
    rds_file <- unserialize(file_data)
    return(rds_file)
  } else {
    stop("Unsupported file extension.")
  }
}

get_team_players <- function(pool, team_ids, year) {
  # If team_ids is NULL or empty, retrieve players from all teams
  if (is.null(team_ids) || length(team_ids) == 0) {
    team_condition <- "IS NOT NULL"  # All teams
  } else {
    # Convert team_ids to a properly formatted SQL condition
    team_condition <- if (length(team_ids) > 1) {
      paste0("IN (", paste0("'", team_ids, "'", collapse = ","), ")")
    } else {
      paste0("= '", team_ids, "'")
    }
  }

  # Adjust query based on "Career" selection
  if (year == "Career") {
    query <- glue::glue("
      SELECT *
      FROM players
      WHERE \"teamID\" {team_condition}
    ")
  } else {
    query <- glue::glue("
      SELECT *
      FROM players
      WHERE \"teamID\" {team_condition} AND year = {year}
    ")
  }

  # Execute the query and return results
  players_data <- DBI::dbGetQuery(pool, query)
  return(players_data)
}



