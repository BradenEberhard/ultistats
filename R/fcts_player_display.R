#' Calculate Percentiles for Stats
#'
#' Calculates the percentile ranks for specified stats (e.g., goals, assists) of a player,
#' and filters out rows where the sum of `dPointsPlayed` and `oPointsPlayed` is less than 50.
#'
#' @param stats Data frame of player stats.
#' @param player_id Player ID to filter.
#' @param stats_to_include Vector of stat columns (e.g., `c("goals", "assists")`).
#'
#' @importFrom tidyr pivot_longer
#' 
#' @return Data frame with player ID, stat name, stat value, and percentile.
#' @export
calculate_percentiles <- function(stats, player_id, per_possession) {
  # Filter out players with less than 10 total points, unless it's the selected player
  stats <- stats[(stats$oPointsPlayed + stats$dPointsPlayed) >= 10 | stats$playerID == player_id, ]
  
  if (per_possession) {
    sum_stats <- stats %>%
      group_by(playerID) %>%
      summarise(
        total_yards = coalesce(sum(yardsThrown, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_goals = coalesce(sum(goals, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_completions = coalesce(sum(completions, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_assists = coalesce(sum(assists, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_blocks = coalesce(sum(blocks, na.rm = TRUE) / sum(dOpportunities, na.rm = TRUE), 0),
        total_yardsReceived = coalesce(sum(yardsReceived, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0)
      )
   } else {
    sum_stats <- stats %>%
      group_by(playerID) %>%
      summarise(
        total_yards = sum(yardsThrown, na.rm = TRUE),
        total_goals = sum(goals, na.rm = TRUE),
        total_completions = sum(completions, na.rm = TRUE),
        total_assists = sum(assists, na.rm = TRUE),
        total_blocks = sum(blocks, na.rm = TRUE),
        total_yardsReceived = sum(yardsReceived, na.rm = TRUE)
      )
   }

  sum_stats <- sum_stats %>%
    # Calculate percentiles for sum stats
    mutate(
      yards_percentile = sum(total_yards < total_yards[playerID == player_id]) / n() * 100,
      goals_percentile = sum(total_goals < total_goals[playerID == player_id]) / n() * 100,
      completions_percentile = sum(total_completions < total_completions[playerID == player_id]) / n() * 100,
      assists_percentile = sum(total_assists < total_assists[playerID == player_id]) / n() * 100,
      blocks_percentile = sum(total_blocks < total_blocks[playerID == player_id]) / n() * 100,
      yardsReceived_percentile = sum(total_yardsReceived < total_yardsReceived[playerID == player_id]) / n() * 100
    )
  
  sum_stats <- sum_stats %>%
    filter(playerID == player_id) %>%
    pivot_longer(cols = c(total_yards, total_goals, total_completions, total_assists, total_blocks, total_yardsReceived),
                 names_to = "stats", values_to = "value") %>%
    mutate(
      stats = case_when(
        stats == "total_yards" ~ "Throwing Yards",
        stats == "total_goals" ~ "Goals",
        stats == "total_assists" ~ "Assists",
        stats == "total_completions" ~ "Completions",
        stats == "total_blocks" ~ "Blocks",
        stats == "total_yardsReceived" ~ "Yards Received",
        TRUE ~ stats
      ),
      percentile = case_when(
        stats == "Throwing Yards" ~ yards_percentile,
        stats == "Goals" ~ goals_percentile,
        stats == "Completions" ~ completions_percentile,
        stats == "Assists" ~ assists_percentile,
        stats == "Blocks" ~ blocks_percentile,
        stats == "Yards Received" ~ yardsReceived_percentile
      ),
      percentile = round(percentile, 2)
    )
  
  # Mean statistics
  mean_stats <- stats %>%
    group_by(playerID) %>%
    summarise(
      offensive_efficiency = mean(coalesce(oOpportunityScores / oOpportunities, 0) * 100, na.rm = TRUE),
      defensive_efficiency = mean(coalesce(dOpportunityStops / dOpportunities, 0) * 100, na.rm = TRUE),
      completion_percent = mean(coalesce(completions / throwAttempts, 0) * 100, na.rm = TRUE)
    ) %>%
    # Calculate percentiles for mean stats
    mutate(
      oOpportunities_percentile = sum(offensive_efficiency < offensive_efficiency[playerID == player_id]) / n() * 100,
      defensive_efficiency_percentile = sum(defensive_efficiency < defensive_efficiency[playerID == player_id]) / n() * 100,
      completion_percent_percentile = sum(completion_percent < completion_percent[playerID == player_id]) / n() * 100
    ) %>%
    filter(playerID == player_id) %>%
    pivot_longer(cols = c(offensive_efficiency, defensive_efficiency, completion_percent),
                 names_to = "stats", values_to = "value") %>%
    mutate(
      stats = case_when(
        stats == "offensive_efficiency" ~ "Offensive Efficiency",
        stats == "defensive_efficiency" ~ "Defensive Efficiency",
        stats == "completion_percent" ~ "Completion Percentage",
        TRUE ~ stats
      ),
      percentile = case_when(
        stats == "Offensive Efficiency" ~ oOpportunities_percentile,
        stats == "Defensive Efficiency" ~ defensive_efficiency_percentile,
        stats == "Completion Percentage" ~ completion_percent_percentile
      ),
      percentile = round(percentile, 2)
    )
  
  # Combine the sum and mean statistics
  combined_stats <- bind_rows(sum_stats, mean_stats)

  return(combined_stats)
}



#' Create Radial Histogram Plot
#'
#' Generates a radial histogram plot for throw angles.
#'
#' @param throws A numeric vector of adjusted throw angles.
#' @param bin_width Width of the bins for the histogram in degrees. Default is 24.
#' @return A `ggplot` object representing the radial histogram.
#' @examples
#' \dontrun{
#'   throws <- c(-45, 90, 180, -90, 0)
#'   radial_histogram_plot(throws)
#' }
#' @export
radial_histogram_plot <- function(throws, bin_width = 24) {
  bin_cutoffs <- seq(-180, 180, by = bin_width)
  ggplot(data.frame(throws), aes(x = throws)) +
    geom_histogram(breaks = bin_cutoffs, fill = "blue", color = "white", boundary = 0) +
    coord_polar(start = pi) +
    scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30)) +
    theme_minimal() +
    labs(
      x = "Angle (Degrees)",
      y = "Frequency"
    )
}

#' Create Percentiles Plot
#'
#' Generates an interactive plotly visualization of player percentiles.
#'
#' @param stats A data frame containing player statistics and percentiles.
#' @return A plotly object representing the categorical percentile distribution.
#' @examples
#' \dontrun{
#'   stats <- data.frame(
#'     stats = c("Speed", "Accuracy", "Stamina"),
#'     percentile = c(90, 75, 60),
#'     value = c(30, 20, 50)
#'   )
#'   percentiles_plot(stats)
#' }
#' @export
percentiles_plot <- function(player_percentiles, per_possession) {
  addition <- ifelse(per_possession, "Per Possession", "")
  p <- ggplot(player_percentiles, aes(
    x = reorder(stats, percentile), 
    y = percentile, 
    text = paste(
      stats, ":", 
      ifelse(value %% 1 == 0, scales::comma(value, accuracy = 1), sprintf("%.2f", value)), 
      "\nPercentile:", scales::percent(percentile / 100)
    )
  )) +
    geom_point(color = "black", size = 3) +
    geom_segment(aes(x = stats, xend = stats, y = 0, yend = percentile), 
                 color = "black", size = 1) +
    labs(
      x = "Metric",
      y = paste("Percentile",addition)
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal() +
    coord_flip()
  
  ggplotly(p, tooltip = c("text")) %>%
    config(
      displaylogo = FALSE,
      displayModeBar = FALSE
    )
}
