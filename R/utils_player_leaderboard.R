# Function to calculate and add percentiles for specific metrics
add_percentiles <- function(df) {
  metrics_to_iterate <- c("completions", "completions_per_possession", "games", "offensive_points_per_game", 
                          "cpoe", "xcp", "offensive_efficiency", "completion_percentage",
                          "thrower_ec_per_possession", "thrower_aec_per_possession", "yardsThrown_per_possession",
                          "assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession",
                          "receptions", "receptions_per_possession", "receiver_aec_per_possession",
                          "goals_per_possession", "drops_per_possession", "yardsReceived_per_possession",
                          "defensive_efficiency", "blocks", "blocks_per_possession")
  
  for (metric in metrics_to_iterate) {
    df[[metric]] <- map_metrics_to_formula(df, metric)[[metric]]$value
    df[[paste0(metric, "_percentile")]] <- sapply(df[[metric]], function(x) {
      calc_percentile(x, all_values = df[[metric]])
    })
  }
  return(df)
}


calculate_group_medians <- function(df, metrics) {
  # Calculate the median percentile for each group by taking the median across all percentiles for the given metrics
  percentiles <- paste0(metrics, "_percentile")
  df$group_median <- apply(df[percentiles], 1, median, na.rm = TRUE)
  return(df)
}

get_table_choices <- function() {
  list(
    "Plus Minus" = "plus_minus",
    "Goals" = "goals",
    "Assists" = "assists",
    "Receiver Adjusted Expected Contribution" = "receiver_aec",
    "Receptions" = "receptions",
    "Drops" = "drops",
    "Receiver Expected Contribution" = "receiver_ec",
    "Turnovers" = "turnovers",
    "Hockey Assists" = "hockeyAssists",
    "Blocks" = "blocks",
    "Thrower Expected Contribution" = "thrower_ec",
    "Thrower Adjusted Expected Contribution" = "thrower_aec",
    "Throwing Yards" = "yardsThrown",
    "Receiving Yards" = "yardsReceived",
    "Games Played" = "games",
    "Completions" = "completions",
    "Possessions" = "possessions",
    "Defensive Possessions" = "defensive_possessions",
    "Offensive Points" = "offensive_points",
    "Defensive Points" = "defensive_points",
    "Expected Completion Percentage" = "xcp",
    "Completion Percentage Over Expected" = "cpoe",
    "Completion Percentage" = "completion_percentage",
    "Offensive Efficiency" = "offensive_efficiency",
    "Defensive Efficiency" = "defensive_efficiency"
  )
}

remove_low_opportunities <- function(metric_table, percentage = 0.2) {
  metric_table <- metric_table %>%
    mutate(total_opportunities = oOpportunities + dOpportunities)
  
  num_rows_to_remove <- ceiling(percentage * nrow(metric_table))
  
  metric_table <- metric_table %>%
    arrange(total_opportunities) %>%
    slice(-(1:num_rows_to_remove)) %>%
    select(-total_opportunities)
  
  return(metric_table)
}

calculate_thrower_percentile <- function(filtered_stats) {
  # Define metric categories
  usage_metrics <- c("completions", "completions_per_possession", "games")
  efficiency_metrics <- c("cpoe", "offensive_efficiency", "completion_percentage")
  contribution_metrics <- c("thrower_ec_per_possession", "thrower_aec_per_possession", "yardsThrown_per_possession")
  scores_metrics <- c("assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession")
  thrower_all_metrics <- c(usage_metrics, efficiency_metrics, contribution_metrics, scores_metrics)
  thrower_percentiles_columns <- paste0(thrower_all_metrics, "_percentile")
  
  # Calculate thrower percentile
  filtered_stats$thrower_percentile <- apply(filtered_stats[, thrower_percentiles_columns], 1, median, na.rm = TRUE)
  filtered_stats$thrower_percentile <- round(filtered_stats$thrower_percentile, 2)
  
  return(filtered_stats)
}

calculate_receiver_percentile <- function(filtered_stats) {
  usage_metrics <- c("receptions", "receptions_per_possession", "games")
  efficiency_metrics <- c("offensive_efficiency", "receiver_aec_per_possession")
  contribution_metrics <- c("goals_per_possession", "drops_per_possession", "yardsReceived_per_possession")
  all_metrics <- c(usage_metrics, efficiency_metrics, contribution_metrics)
  receiver_percentiles_columns <- paste0(all_metrics, "_percentile")
  
  # Calculate receiver percentile
  filtered_stats$receiver_percentile <- apply(filtered_stats[, receiver_percentiles_columns], 1, median, na.rm = TRUE)
  filtered_stats$receiver_percentile <- round(filtered_stats$receiver_percentile, 2)
  
  return(filtered_stats)
}

calculate_defense_percentile <- function(filtered_stats) {
  usage_metrics <- c("games")
  efficiency_metrics <- c("defensive_efficiency", "blocks", "blocks_per_possession")
  
  all_metrics <- c(usage_metrics, efficiency_metrics)
  defense_percentiles_columns <- paste0(all_metrics, "_percentile")
  
  # Calculate receiver percentile
  filtered_stats$defense_percentile <- apply(filtered_stats[, defense_percentiles_columns], 1, median, na.rm = TRUE)
  filtered_stats$defense_percentile <- round(filtered_stats$defense_percentile, 2)
  
  return(filtered_stats)
}

calculate_overall_percentile <- function(filtered_stats) {
  # Define metric categories
  usage_metrics <- c("completions", "completions_per_possession", "games", "receptions", "receptions_per_possession")
  efficiency_metrics <- c("cpoe", "offensive_efficiency", "completion_percentage", "xcp")
  contribution_metrics <- c("thrower_ec_per_possession", "thrower_aec_per_possession", "yardsThrown_per_possession", "receiver_aec_per_possession")
  scores_metrics <- c("assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession", "goals_per_possession", "drops_per_possession", "yardsReceived_per_possession")
  defensive_metrics <- c("defensive_efficiency", "blocks", "blocks_per_possession")
  all_metrics <- c(usage_metrics, efficiency_metrics, contribution_metrics, scores_metrics, defensive_metrics)
  percentiles_columns <- paste0(all_metrics, "_percentile")
  
  # Calculate thrower percentile
  filtered_stats$overall_percentile <- apply(filtered_stats[, percentiles_columns], 1, median, na.rm = TRUE)
  filtered_stats$overall_percentile <- round(filtered_stats$overall_percentile, 2)
  
  return(filtered_stats)
}