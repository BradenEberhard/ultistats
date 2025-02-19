# Helper function to format the datatable
format_dt <- function(metric_table, column_names) {
  if (is.null(metric_table)) {
    # Return a message or empty data frame when the table is empty
    return(DT::datatable(
      data.frame(Message = "No data available"),
      rownames = FALSE,
      colnames = "Message",
      options = list(
        pageLength = 1, 
        autoWidth = TRUE,
        select = list(
          style = "os",
          selector = 'tr>td:nth-child(1)'
        )
      ),
      extensions = c("Select"),
      selection = 'none',
      class = "compact"
    ) %>%
      DT::formatStyle(
        columns = "Message", 
        fontSize = '12px', 
        textAlign = 'center'
      )
    )
  }

  DT::datatable(
    metric_table,
    rownames = FALSE,
    colnames = column_names,
    options = list(
      pageLength = 10, 
      autoWidth = TRUE,
      select = list(
        style = "os",
        selector = 'tr>td:nth-child(1)'
      )
    ),
    extensions = c("Select"),
    selection = 'none',
    class = "compact"
  ) %>%
    DT::formatStyle(
      columns = colnames(metric_table), 
      fontSize = '12px', 
      lineHeight = '70%',
    ) %>%
    DT::formatStyle(
      columns = "fullName", # Apply style only to the "Name" column
      cursor = "pointer",
      `white-space` = "nowrap", # Prevent text wrapping
      `overflow` = "hidden",    # Hide overflowing text
      `text-overflow` = "ellipsis", # Display ellipses for overflowed text
      `textDecoration` = "underline"
    )
}



get_grade_table <- function(input, all_player_stats) {
  filtered_stats <- all_player_stats %>% filter(year == input$year_selector)
  filtered_stats <- remove_low_opportunities(filtered_stats, percentage = 0.2)
  filtered_stats <- add_percentiles(filtered_stats)
  
  filtered_stats <- calculate_thrower_percentile(filtered_stats)
  filtered_stats <- calculate_receiver_percentile(filtered_stats)
  filtered_stats <- calculate_defense_percentile(filtered_stats)
  filtered_stats <- calculate_overall_percentile(filtered_stats)
  filtered_stats <- filtered_stats[order(-filtered_stats$overall_percentile), ]
  metric_table <- filtered_stats %>% select(fullName, overall_percentile, thrower_percentile, receiver_percentile, defense_percentile, oOpportunities, dOpportunities)
  if(nrow(metric_table) == 0) {
    return(NULL)
  }
  return(metric_table)
}

get_metric_table <- function(input, all_player_stats, selected_metric) {
  current_metric <- paste0(selected_metric, ifelse(input$stat_category == "Total", "", "_per_possession"))
  filtered_stats <- all_player_stats %>% filter(year == input$year_selector)
  filtered_stats <- remove_low_opportunities(filtered_stats, percentage = 0.2)
  mapped_metrics <- map_metrics_to_formula(filtered_stats, current_metric)
  metric_table <- data.frame(
    fullName = filtered_stats$fullName,
    value = mapped_metrics[[current_metric]]$value,
    possessions = filtered_stats$oOpportunities,
    dpossessions = filtered_stats$dOpportunities
  )

  if (nrow(metric_table) == 0) {
    return(metric_table)
  }

  metric_table$percentile <- sapply(metric_table$value, calc_percentile, all_values = mapped_metrics[[current_metric]]$value)
  metric_table$percentile <- metric_table$percentile %>% round(2)
  metric_table$value <- metric_table$value %>% round(2)
  metric_table <- metric_table[order(-metric_table$percentile), ]

  return(list(metric_table = metric_table, metric_name = mapped_metrics[[current_metric]]$display_name))
}