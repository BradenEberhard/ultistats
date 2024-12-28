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
    
    if (length(player_value) > 0) {
      
      metric_values <- df_year[[metric]]
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


get_thrower_metrics_plot <- function(df, player_full_name, thrower_handler_value, thrower_offense_value, category) {
  thrower_handler_value <- ifelse(is.null(thrower_handler_value), FALSE, thrower_handler_value)
  thrower_offense_value <- ifelse(is.null(thrower_offense_value), FALSE, thrower_offense_value)
  counting_metrics <- c("assists", "completions", "hockeyAssists", 
                        "yardsThrown", "turnovers")
  percentage_metrics <- c("completion_percentage", "cpoe", "xcp")
  addition <- ifelse(category == "Per Possession", "_per_possession", 
                     ifelse(category == "Per Game", "_per_game", ""))
  counting_metrics <- paste0(counting_metrics, addition)
  all_metrics <- c(counting_metrics, percentage_metrics)
  metric_data <- list()
  df <- df %>% mutate(turnovers = throwAttempts - completions)
  player_df <- df %>% filter(fullName == player_full_name, year != "Career") %>% arrange(year)

  for (i in 1:nrow(player_df)) {
    # Get the player value for each metric in the current row
    year <- player_df[i, "year"]
    df_year <- adjust_for_category(df, category)
    df_year <- adjust_for_role(df_year, thrower_handler_value, thrower_offense_value, year, player_full_name)
    
    filtered_df <- df_year %>% filter(fullName == player_full_name, year != "Career") %>% arrange(year)
    player_value <- filtered_df[i, all_metrics]
    # For each metric, calculate the percentile and store the result
    for (metric in all_metrics) {
      if (!is.na(player_value[[metric]])) {
        # Get all values for the current metric and year
        metric_values <- df_year[df_year$year == year, metric]
        
        # Calculate the percentile for the player's value
        percentile_value <- calc_percentile(player_value[[metric]], metric_values) %>% round(2)
        if (metric == "turnovers") {
          percentile_value <- 100 - percentile_value  # Reverse the percentile scale
        }
        if (metric %in% percentage_metrics) {
          player_value[[metric]] <- player_value[[metric]] * 100  # Convert to percentage if necessary
        }
        
        # Store the data (metric, percentile, year, and value)
        metric_data[[length(metric_data) + 1]] <- list(
          year = year,
          metric = metric,
          percentile = percentile_value,
          value = player_value[[metric]]
        )
      }
    }
  }
  metric_df <- bind_rows(lapply(metric_data, function(x) {
    data.frame(
      year = x$year,
      metric = x$metric,
      percentile = x$percentile,
      value = x$value
    )
  })) %>% arrange(year) %>% rename_metrics()
  plot <- plot_ly(metric_df, 
    x = ~year, 
    y = ~percentile, 
    color = ~metric, 
    type = 'scatter', 
    mode = 'lines+markers', 
    line = list(width = 2),
    marker = list(size = 6),
    hoverinfo = 'text',  # Specify custom hover info
    text = ~paste0(
      metric, ": ", 
      ifelse(value %% 1 == 0, scales::comma(value, accuracy = 1), sprintf("%.2f", value)), 
      "\nPercentile: ", scales::percent(percentile / 100)
    )) %>%
  layout(title = "Metrics Over Time",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Percentile", range=c(0,102), tickvals = seq(0, 100, by = 20)),
    margin = list(t = 50),
    showlegend = TRUE) %>%
  config(displayModeBar = FALSE)

  return(plot)
}