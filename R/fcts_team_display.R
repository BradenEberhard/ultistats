get_teams_names <- function() {
  conn <- open_db_connection()
  on.exit(close_db_connection(conn))
  
  teams_data <- get_table_from_db(conn, table_name = "teams")
  teams_data <- teams_data %>% filter(year >= 2021)
  named_teams <- setNames(teams_data$teamID, teams_data$fullName)
  
  return(named_teams)
}
