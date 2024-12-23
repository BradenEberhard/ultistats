render_stat <- function(stat_name, filtered_stats, all_player_stats) {
  all_player_stats[[stat_name]] <- as.numeric(all_player_stats[[stat_name]])
  player_mean_stats <- all_player_stats %>%
    group_by(playerID) %>%
    summarize(mean_stat = sum(get(stat_name), na.rm = TRUE))
  renderText({
    stats <- filtered_stats()
    total <- sum(as.integer(stats[[stat_name]]), na.rm = TRUE)
    total <- as.character(scales::comma(total))
    percentile <- sum(player_mean_stats$mean_stat <= total) / nrow(player_mean_stats) * 100
    paste0(total, "; ", round(percentile, 0), "th percentile")
  })
}

create_stat_value_box <- function(title, output_id, css_class = "mb-4", ns) {
  bslib::value_box(
    title = title,
    value = textOutput(ns(output_id)),
    class = css_class
  )
}

create_stat_value_box_dual <- function(title, output_ids, css_class = "mb-4", ns, fixed_height = "150px") {
  bslib::value_box(
    title = title,
    value = div(
      div(
        class = "col",
        style = "display: inline-block; width: 50%; text-align: center;",
        div(style = "font-weight: bold;", "Throwing Yards"),
        textOutput(ns(output_ids$throwing))
      ),
      div(
        class = "col",
        style = "display: inline-block; width: 50%; text-align: center;",
        div(style = "font-weight: bold;", "Receiving Yards"),
        textOutput(ns(output_ids$receiving))
      )
    ),
    class = css_class,
    min_height = "100px"
  )
}

#' Calculate Percentiles for Stats
#'
#' Calculates the percentile ranks for specified stats (e.g., goals, assists) of a player,
#' and filters out rows where the sum of `dPointsPlayed` and `oPointsPlayed` is less than 50.
#'
#' @param stats Data frame of player stats.
#' @param player_id Player ID to filter.
#' @param stats_to_include Vector of stat columns (e.g., `c("goals", "assists")`).
#'
#' @return Data frame with player ID, stat name, stat value, and percentile.
#' @export
calculate_percentiles <- function(stats, player_id, stats_to_include) {
  stats <- stats[(stats$oPointsPlayed + stats$dPointsPlayed) >= 50, ]

  percentile_df <- stats %>% 
    group_by(playerID) %>%
    summarise(across(all_of(stats_to_include), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    gather(key = "stats", value = "value", all_of(stats_to_include)) %>%
    mutate(percentile = percent_rank(value) * 100) %>%
    filter(playerID == player_id) %>%
    arrange(percentile)
  
  return(percentile_df)
}
