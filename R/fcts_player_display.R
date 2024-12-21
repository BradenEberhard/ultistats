render_stat <- function(stat_name, filtered_stats) {
  renderText({
    tryCatch({
      stats <- filtered_stats()
      if (!stat_name %in% colnames(stats)) {
        warning(paste("Column", stat_name, "not found in player stats"))
        return("N/A")
      }
      total <- sum(as.integer(stats[[stat_name]]), na.rm = TRUE)
      if (is.na(total) || length(total) == 0) {
        return("N/A")
      }
      as.character(total)
    }, error = function(e) {
      warning(paste("Error in rendering", stat_name, ":", e$message))
      "Error"
    })
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