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