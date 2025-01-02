# Define a function to generate the plots dynamically
generate_plot_outputs <- function(role, stats, input, output, plot_configs) {
  # Loop over each plot function and generate the output dynamically
  lapply(names(plot_configs), function(config_name) {
    plot_config <- plot_configs[[config_name]]
    output[[paste0(role, "_", plot_config[["label"]], "_plot")]] <- renderGirafe({
      req(input$player_selector)
      get_passer_plot(input, stats, plot_config[["metrics"]], plot_config[["title"]])
    })
  })
}

# function linking inputs, outputs and plotting functions
generate_thrower_plots <- function(plot_list, all_player_stats, input, output, ns) {
  plot_ui_list <- lapply(plot_list, function(plot_info) {
    plot_output_id <- paste0("plot_", plot_info$label)
    girafeOutput(ns(plot_output_id))
  })
  
  lapply(plot_list, function(plot_info) {
    plot_output_id <- paste0("plot_", plot_info$label)
    output[[plot_output_id]] <- renderGirafe({
      plot_func <- plot_info$plot_func
      plot_func(all_player_stats, input$player_selector, input$handler_switch_value, input$offense_switch_value)
    })
  })
  
  return(plot_ui_list)
}

# Logic for displaying radial histograms
#' @importFrom stats na.omit
generate_radial_histogram_plot <- function(input, player_selector, year_selector, all_player_stats, db_path, role = "thrower") {
  req(input$player_selector, input$year_selector)
  player_id <- get_playerID_by_fullName(input, all_player_stats)
  conn <- open_db_connection(db_path)
  on.exit(close_db_connection(conn))

  player_passes <- get_player_passes_by_role(conn, player_id, role)
  player_passes$adjusted_angle <- (as.integer(player_passes$throw_angle) + 90) %% 360 - 180
  player_passes$year <- substr(player_passes$gameID, 1, 4)
  passes <- if (year_selector == "Career") {
    player_passes
  } else {
    player_passes %>% filter(.data$year == year_selector)
  }

  passes <- na.omit(passes$adjusted_angle)
  radial_histogram_plot(passes, role = role)
}

# Logic to update overal skill percentiles plot with a new player
generate_skill_percentiles_plot <- function(input, session, all_player_stats) {
  req(input$player_selector, input$year_selector)
  all_metrics <- get_metrics(input$stat_category)
  plot_data <- convert_to_metric_df(input, all_player_stats, all_metrics)
  create_skill_percentiles_plot(session, plot_data)
}

get_passer_plot <- function(input, df, metrics, title) {
  metric_data <- convert_to_metric_df(input, df, metrics, all_years = TRUE)
  plot <- generate_yearly_percentile_plot(metric_data, title)
  return(plot)
}

#' @importFrom stats median
get_thrower_grade <- function(input, df) {
  usage_metrics <- c("completions", "completions_per_possession", "games", "offensive_points_per_game")
  efficiency_metrics <- c("cpoe", "xcp", "offensive_efficiency", "completion_percentage")
  contribution_metrics <- c("thrower_ec_per_possession", "thrower_aec_per_possession", "yardsThrown_per_possession")
  scores_metrics <- c("assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession")


  metric_data <- convert_to_metric_df(input, df, c(usage_metrics, efficiency_metrics, contribution_metrics, scores_metrics))

  usage_percentile <- metric_data[metric_data$metric %in% usage_metrics, ]$percentile %>% median() %>% round(0)
  efficiency_percentile <- metric_data[metric_data$metric %in% efficiency_metrics, ]$percentile %>% median() %>% round(0)
  scores_percentile <- metric_data[metric_data$metric %in% scores_metrics, ]$percentile %>% median() %>% round(0)
  contribution_percentile <- metric_data[metric_data$metric %in% contribution_metrics, ]$percentile %>% median() %>% round(0)

  return(list(
    usage_percentile = usage_percentile,
    efficiency_percentile = efficiency_percentile,
    scores_percentile = scores_percentile,
    contribution_percentile = contribution_percentile,
    overall_percentile = median(c(efficiency_percentile, usage_percentile, scores_percentile, contribution_percentile))
  ))
}

#' @importFrom stats median
get_receiver_grade <- function(input, df) {
  usage_metrics <- c("receptions", "receptions_per_possession", "games", "offensive_points_per_game")
  efficiency_metrics <- c("offensive_efficiency", "receiver_aec_per_possession")
  contribution_metrics <- c("goals_per_possession", "drops_per_possession", "yardsReceived_per_possession")


  metric_data <- convert_to_metric_df(input, df, c(usage_metrics, efficiency_metrics, contribution_metrics))

  usage_percentile <- metric_data[metric_data$metric %in% usage_metrics, ]$percentile %>% median() %>% round(0)
  efficiency_percentile <- metric_data[metric_data$metric %in% efficiency_metrics, ]$percentile %>% median() %>% round(0)
  contribution_percentile <- metric_data[metric_data$metric %in% contribution_metrics, ]$percentile %>% median() %>% round(0)

  return(list(
    usage_percentile = usage_percentile,
    efficiency_percentile = efficiency_percentile,
    contribution_percentile = contribution_percentile,
    overall_percentile = median(c(efficiency_percentile, usage_percentile, contribution_percentile))

  ))
}

#' @importFrom stats median
get_defense_grade <- function(input, df) {
  usage_metrics <- c("games", "defensive_points_per_game")
  efficiency_metrics <- c("defensive_efficiency", "blocks", "blocks_per_possession")


  metric_data <- convert_to_metric_df(input, df, c(usage_metrics, efficiency_metrics))

  usage_percentile <- metric_data[metric_data$metric %in% usage_metrics, ]$percentile %>% median() %>% round(0)
  efficiency_percentile <- metric_data[metric_data$metric %in% efficiency_metrics, ]$percentile %>% median() %>% round(0)

  return(list(
    usage_percentile = usage_percentile,
    efficiency_percentile = efficiency_percentile,
    overall_percentile = median(c(efficiency_percentile, usage_percentile))
  ))
}

