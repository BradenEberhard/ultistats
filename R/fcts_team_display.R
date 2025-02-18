get_teams_names <- function() {
  pool <- get_db_pool()
  
  teams_data <- get_table_from_db(pool, table_name = "teams")
  teams_data <- get_table_from_db(pool, table_name = "teams") %>%
    filter(year >= 2021, !grepl("All-Star", fullName)) %>%  # Exclude All-Star teams
    arrange(fullName)
  named_teams <- setNames(teams_data$teamID, teams_data$fullName)
  
  return(named_teams)
}

modify_metric_name <- function(metric, stat_category) {
  efficiency_metrics <- c("xcp", "cpoe", "completion_percentage", "offensive_efficiency", "offensive_efficiency_involved", "involved_efficiency_improvement", "offensive_involvement", "offensive_efficiency_above_replacement", "defensive_efficiency")
  if (metric %in% efficiency_metrics) {
    return(metric)
  }
  
  # Otherwise, modify based on stat category
  suffix <- ifelse(stat_category == "Total", "", 
                   ifelse(stat_category == "Per Game", "_per_game", "_per_possession"))
  
  return(paste0(metric, suffix))
}