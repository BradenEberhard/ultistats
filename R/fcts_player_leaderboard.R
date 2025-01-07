# Function to calculate and add percentiles for specific metrics
add_percentiles <- function(df) {
  
  metrics_to_iterate <- c("completions", "completions_per_possession", "games", "offensive_points_per_game", 
                          "cpoe", "xcp", "offensive_efficiency", "completion_percentage",
                          "thrower_ec_per_possession", "thrower_aec_per_possession", "yardsThrown_per_possession",
                          "assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession")
  
  for (metric in metrics_to_iterate) {
    print(metric)
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