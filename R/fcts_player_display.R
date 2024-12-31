#' Calculate Percentiles for Stats
#'
#' Calculates the percentile ranks for specified stats (e.g., goals, assists) of a player,
#' and filters out rows where the sum of `dPointsPlayed` and `oPointsPlayed` is less than 50.
#'
#' @param stats Data frame of player stats.
#' @param player_full_name Player fullName to filter.
#' @param stats_to_include Vector of stat columns (e.g., `c("goals", "assists")`).
#'
#' @importFrom tidyr pivot_longer
#' 
#' @return Data frame with player ID, stat name, stat value, and percentile.
#' @export
calculate_percentiles <- function(stats, player_full_name, per_possession) {
  # Filter out players with less than 10 total points, unless it's the selected player
  stats <- stats[(stats$oPointsPlayed + stats$dPointsPlayed) >= 10 | stats$fullName == player_full_name, ]
  
  if (per_possession) {
    sum_stats <- stats %>%
      group_by(fullName) %>%
      summarise(
        total_yards = coalesce(sum(yardsThrown, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_goals = coalesce(sum(goals, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_completions = coalesce(sum(completions, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_assists = coalesce(sum(assists, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_blocks = coalesce(sum(blocks, na.rm = TRUE) / sum(dOpportunities, na.rm = TRUE), 0),
        total_yardsReceived = coalesce(sum(yardsReceived, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0),
        total_hockeyAssists = coalesce(sum(hockeyAssists, na.rm = TRUE) / sum(oOpportunities, na.rm = TRUE), 0)
      )
   } else {
    sum_stats <- stats %>%
      group_by(fullName) %>%
      summarise(
        total_yards = sum(yardsThrown, na.rm = TRUE),
        total_goals = sum(goals, na.rm = TRUE),
        total_completions = sum(completions, na.rm = TRUE),
        total_assists = sum(assists, na.rm = TRUE),
        total_blocks = sum(blocks, na.rm = TRUE),
        total_yardsReceived = sum(yardsReceived, na.rm = TRUE),
        total_hockeyAssists = sum(hockeyAssists, na.rm = TRUE)
      )
   }

  sum_stats <- sum_stats %>%
    # Calculate percentiles for sum stats
    mutate(
      yards_percentile = sum(total_yards < total_yards[fullName == player_full_name]) / n() * 100,
      goals_percentile = sum(total_goals < total_goals[fullName == player_full_name]) / n() * 100,
      completions_percentile = sum(total_completions < total_completions[fullName == player_full_name]) / n() * 100,
      assists_percentile = sum(total_assists < total_assists[fullName == player_full_name]) / n() * 100,
      blocks_percentile = sum(total_blocks < total_blocks[fullName == player_full_name]) / n() * 100,
      yardsReceived_percentile = sum(total_yardsReceived < total_yardsReceived[fullName == player_full_name]) / n() * 100,
      hockeyAssists_percentile = sum(total_hockeyAssists < total_hockeyAssists[fullName == player_full_name]) / n() * 100
    )
  
  sum_stats <- sum_stats %>%
    filter(fullName == player_full_name) %>%
    pivot_longer(cols = c(total_yards, total_goals, total_completions, total_assists, total_blocks, total_yardsReceived, total_hockeyAssists),
                 names_to = "stats", values_to = "value") %>%
    mutate(
      stats = case_when(
        stats == "total_yards" ~ "Throwing Yards",
        stats == "total_goals" ~ "Goals",
        stats == "total_assists" ~ "Assists",
        stats == "total_completions" ~ "Completions",
        stats == "total_blocks" ~ "Blocks",
        stats == "total_yardsReceived" ~ "Yards Received",
        stats == "total_hockeyAssists" ~ "Hockey Assists",
        TRUE ~ stats
      ),
      percentile = case_when(
        stats == "Throwing Yards" ~ yards_percentile,
        stats == "Goals" ~ goals_percentile,
        stats == "Completions" ~ completions_percentile,
        stats == "Assists" ~ assists_percentile,
        stats == "Blocks" ~ blocks_percentile,
        stats == "Hockey Assists" ~ hockeyAssists_percentile,
        stats == "Yards Received" ~ yardsReceived_percentile
      ),
      percentile = round(percentile, 2)
    )
  
  # Mean statistics
  mean_stats <- stats %>%
    group_by(fullName) %>%
    summarise(
      offensive_efficiency = mean(coalesce(oOpportunityScores / oOpportunities, 0) * 100, na.rm = TRUE),
      defensive_efficiency = mean(coalesce(dOpportunityStops / dOpportunities, 0) * 100, na.rm = TRUE),
      completion_percent = mean(coalesce(completions / throwAttempts, 0) * 100, na.rm = TRUE)
    ) %>%
    # Calculate percentiles for mean stats
    mutate(
      oOpportunities_percentile = sum(offensive_efficiency < offensive_efficiency[fullName == player_full_name]) / n() * 100,
      defensive_efficiency_percentile = sum(defensive_efficiency < defensive_efficiency[fullName == player_full_name]) / n() * 100,
      completion_percent_percentile = sum(completion_percent < completion_percent[fullName == player_full_name]) / n() * 100
    ) %>%
    filter(fullName == player_full_name) %>%
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
  max_y <- max(table(cut(throws, breaks = bin_cutoffs)))

  ggplot(data.frame(throws), aes(x = throws)) +
    geom_histogram(breaks = bin_cutoffs, fill = "blue", color = "white", boundary = 0) +
    coord_polar(start = pi, clip = "off") +
    scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
    theme_minimal() +
    annotate("text", x = 0, y = max_y+5, label = "Forward", size = 5, fontface = "bold", hjust=0.5, vjust=0) +
    annotate("text", x = -90, y = max_y+5, label = "Left", size = 5, fontface = "bold", hjust=1, vjust=0.5) +
    annotate("text", x = 90, y = max_y+5, label = "Right", size = 5, fontface = "bold", hjust=0, vjust=0.5) +
    annotate("text", x = 180, y = max_y+5, label = "Backward", size = 5, fontface = "bold", hjust=0.5, vjust=1) +
    theme(
      axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Adjust y-axis label size
      axis.text.y = element_text(size = 12, margin = margin(r = 20)),
      axis.title.x = element_text(size = 14),  # Adjust y-axis label size
      axis.text.x = element_blank(),  # Hides angle labels
      axis.ticks = element_blank(),
      plot.title = element_text(size = 18), 
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)  
    ) +
    labs(
      title = "Throwing Direction Distribution",
      x = "Angle",
      y = "Number of Throws"
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

#' Create a Custom Plotly Visualization
#'
#' Generates a Plotly visualization with segments and markers to display metrics and percentiles.
#'
#' @param plot_data A data frame containing the data to be visualized. It should include `metric`, `percentile`, and `value` columns.
#' @param addition A string to append to the y-axis title and hover text for additional context.
#'
#' @return A Plotly object representing the custom visualization.
#' @export
#'
#' @importFrom tidyr drop_na
create_percentiles_plot <- function(plot_data, addition) {
  plot_data <- plot_data %>% drop_na()
  
  plot_ly() %>%
    add_segments(
      data = plot_data,
      x = 0, 
      xend = ~percentile, 
      y = ~reorder(metric, percentile), 
      yend = ~reorder(metric, percentile),
      line = list(color = "black", width = 2),
      text = ~paste(metric, addition, ":", sub("\\.0+$", "", scales::comma(value, accuracy = 0.01)), "<br>Percentile:", percentile),
      hoverinfo = "text", # Display custom hover text
      hoveron = "points+fills",  # Enable hover effects on both points and lines
      showlegend = FALSE
    ) %>%
    add_trace(
      data = plot_data,
      x = ~percentile, 
      y = ~reorder(metric, percentile), 
      type = "scatter", 
      mode = "markers", 
      marker = list(size = 8, color = "black"),
      text = ~paste(metric, addition, ":", sub("\\.0+$", "", scales::comma(value, accuracy = 0.01)), "<br>Percentile:", percentile),
      hoverinfo = "text", # Display custom hover text
      hoveron = "points+fills",  # Enable hover effects on both points and lines
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = list(title = "Percentile", range = c(0, 102)), 
      yaxis = list(title = paste("Metric", addition)),
      showlegend = FALSE 
    ) %>%
    config(displayModeBar = FALSE)
}



convert_to_metric_df <- function(df, category, player_full_name, year, handler_value, offense_value) {
  handler_value <- ifelse(is.null(handler_value), FALSE, handler_value)
  offense_value <- ifelse(is.null(offense_value), FALSE, offense_value)
  df <- adjust_for_category(df, category)
  all_metrics <- get_metrics(category)
  df_year <- filter_year(df, year)
  df_year <- adjust_for_role(df_year, handler_value, offense_value, year, player_full_name)
  metric_data <- list()
  percentage_metrics <- c("offensive_efficiency", "defensive_efficiency", "completion_percentage", "cpoe", "xcp")
  
  for (metric in all_metrics) {
    player_value <- df_year[df_year$fullName == player_full_name, metric]
    df_final_players <- df_year %>% filter(games >= 3)
    if (length(player_value) > 0) {
      
      metric_values <- df_final_players[[metric]]
      percentile_value <- calc_percentile(player_value, metric_values) %>% round(2)
      if (metric %in% percentage_metrics) {
        player_value <- player_value * 100
      }
      metric_data[[length(metric_data) + 1]] <- list(
        metric = metric,
        percentile = percentile_value,
        value = player_value
      )
    }
  }
  
  return(do.call(rbind, lapply(metric_data, function(x) data.frame(x, stringsAsFactors = FALSE))))
}


get_thrower_usage_plot <- function(df, player_full_name, thrower_handler_value, thrower_offense_value) {
  thrower_handler_value <- ifelse(is.null(thrower_handler_value), FALSE, thrower_handler_value)
  thrower_offense_value <- ifelse(is.null(thrower_offense_value), FALSE, thrower_offense_value)
  
  all_metrics <- c("completions",  "completions_per_possession", "games", "oOpportunities")
  df <- df %>% mutate(
      completions_per_possession = completions / oOpportunities,
    )
  
  metric_data <- prepare_metric_data(df, player_full_name, thrower_handler_value, thrower_offense_value, all_metrics)
  metric_df <- format_metric_data(metric_data)
  plot <- generate_percentile_plot(metric_df, "Usage")
  
  return(plot)
}

get_thrower_contribution_plot <- function(df, player_full_name, thrower_handler_value, thrower_offense_value) {
  thrower_handler_value <- ifelse(is.null(thrower_handler_value), FALSE, thrower_handler_value)
  thrower_offense_value <- ifelse(is.null(thrower_offense_value), FALSE, thrower_offense_value)
  
  all_metrics <- c("yardsThrown",  "thrower_ec", "thrower_aec")
  df <- df %>% mutate(
    yardsThrown = 100 * yardsThrown / oOpportunities,
    thrower_ec = 100 * thrower_ec / oOpportunities,
    thrower_aec = 100 * thrower_aec / oOpportunities,
    )
  
  metric_data <- prepare_metric_data(df, player_full_name, thrower_handler_value, thrower_offense_value, all_metrics)
  metric_df <- format_metric_data(metric_data)
  plot <- generate_percentile_plot(metric_df, "Contribution Per 100 Possessions")
  
  return(plot)
}


get_thrower_efficiency_plot <- function(df, player_full_name, thrower_handler_value, thrower_offense_value) {
  thrower_handler_value <- ifelse(is.null(thrower_handler_value), FALSE, thrower_handler_value)
  thrower_offense_value <- ifelse(is.null(thrower_offense_value), FALSE, thrower_offense_value)
  
  all_metrics <- c("completion_percentage", "xcp", "cpoe", "offensive_efficiency")
  df[all_metrics] <- df[all_metrics] * 100         
  metric_data <- prepare_metric_data(df, player_full_name, thrower_handler_value, thrower_offense_value, all_metrics)
  metric_df <- format_metric_data(metric_data)
  plot <- generate_percentile_plot(metric_df, "Efficiency")
  
  return(plot)
}

get_thrower_scores_plot <- function(df, player_full_name, thrower_handler_value, thrower_offense_value) {
  thrower_handler_value <- ifelse(is.null(thrower_handler_value), FALSE, thrower_handler_value)
  thrower_offense_value <- ifelse(is.null(thrower_offense_value), FALSE, thrower_offense_value)

  
  all_metrics <- c("assists", "hockeyAssists", "turnovers")
  
  df <- df %>% mutate(
    turnovers = throwAttempts - completions,
    assists = 100 * assists / oOpportunities,
    hockeyAssists = 100 * hockeyAssists / oOpportunities,
    turnovers = 100 * turnovers / oOpportunities
  )

  metric_data <- prepare_metric_data(df, player_full_name, thrower_handler_value, thrower_offense_value, all_metrics)
  metric_df <- format_metric_data(metric_data)
  plot <- generate_percentile_plot(metric_df, "Scoring Per 100 Possessions")
  
  return(plot)
}

get_thrower_grade <- function(df, player_full_name, handler_value, offense_value, player_year) {
  handler_value <- ifelse(is.null(handler_value), FALSE, handler_value)
  offense_value <- ifelse(is.null(offense_value), FALSE, offense_value)

  usage_metrics <- c("oOpportunities", "completions", "completions_per_possession", "games")
  efficiency_metrics <- c("cpoe", "xcp", "offensive_efficiency", "completion_percentage")
  contribution_metrics <- c("thrower_ec", "thrower_aec", "yardsThrown")
  scoring_metrics <- c("assists", "hockeyAssists", "turnovers")
  df <- df %>% mutate(
    yardsThrown = 100 * yardsThrown / oOpportunities,
    thrower_ec = 100 * thrower_ec / oOpportunities,
    thrower_aec = 100 * thrower_aec / oOpportunities,
    turnovers = throwAttempts - completions,
    assists = 100 * assists / oOpportunities,
    hockeyAssists = 100 * hockeyAssists / oOpportunities,
    turnovers = 100 * turnovers / oOpportunities,
    completions_per_possession = completions / oOpportunities,
  )

  df_year <- filter_year(df, player_year)
  df_year <- adjust_for_role(df_year, handler_value, offense_value, player_year, player_full_name)
  metric_data <- list()
  
  for (metric in c(usage_metrics, efficiency_metrics, scoring_metrics, contribution_metrics)) {
    player_value <- df_year[df_year$fullName == player_full_name, metric]
    df_final_players <- df_year %>% filter(games >= 3)
    if (length(player_value) > 0) {
      
      metric_values <- df_final_players[[metric]]
      percentile_value <- calc_percentile(player_value, metric_values) %>% round(2)
      metric_data[[length(metric_data) + 1]] <- list(
        metric = metric,
        percentile = percentile_value,
        value = player_value
      )
    }
  }
  
  metric_df <- do.call(rbind, lapply(metric_data, function(x) data.frame(x, stringsAsFactors = FALSE))) %>% rename_metrics(keep_category = TRUE)
  usage_percentile <- metric_df %>% 
    filter(metric %in% rename_metrics(data.frame(metric = usage_metrics), keep_category = TRUE)$metric) %>% 
    summarize(median_value = median(percentile, na.rm = TRUE)) %>% 
    pull(median_value) %>% round(2)
  efficiency_percentile <- metric_df %>% 
    filter(metric %in% rename_metrics(data.frame(metric = efficiency_metrics), keep_category = TRUE)$metric) %>% 
    summarize(median_value = median(percentile, na.rm = TRUE)) %>% 
    pull(median_value) %>% round(2)
  scoring_percentile <- metric_df %>% 
    filter(metric %in% rename_metrics(data.frame(metric = scoring_metrics), keep_category = TRUE)$metric) %>% 
    summarize(median_value = median(percentile, na.rm = TRUE)) %>% 
    pull(median_value) %>% round(2)
  contribution_percentile <- metric_df %>% 
    filter(metric %in% rename_metrics(data.frame(metric = contribution_metrics), keep_category = TRUE)$metric) %>% 
    summarize(median_value = median(percentile, na.rm = TRUE)) %>% 
    pull(median_value) %>% round(2)
  return(list(
    usage_percentile = usage_percentile,
    efficiency_percentile = efficiency_percentile,
    scoring_percentile = scoring_percentile,
    contribution_percentile = contribution_percentile
  ))
}