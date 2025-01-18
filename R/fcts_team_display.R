get_teams_names <- function() {
  pool <- get_db_pool()
  
  teams_data <- get_table_from_db(pool, table_name = "teams")
  teams_data <- teams_data %>% filter(year >= 2021)
  named_teams <- setNames(teams_data$teamID, teams_data$fullName)
  
  return(named_teams)
}
