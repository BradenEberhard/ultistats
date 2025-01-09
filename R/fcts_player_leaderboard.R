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

  dt <- DT::datatable(
    metric_table,
    options = list(pageLength = 10, autoWidth = TRUE),
    rownames = FALSE,
    class = "compact",
    colnames = c("Name", "Overall Percentile", "Thrower Percentile", "Receiver Percentile", "Defense Percentile", "Offensive Possessions", "Defensive Possessions")
  ) %>%
    DT::formatStyle(
      columns = colnames(metric_table), 
      fontSize = '12px', 
      lineHeight='70%'
    ) %>%
      DT::formatStyle(
        columns = "fullName", # Apply style only to the "Name" column
        `white-space` = "nowrap", # Prevent text wrapping
        `overflow` = "hidden",    # Hide overflowing text
        `text-overflow` = "ellipsis" # Display ellipses for overflowed text
      ) 
  return(dt)
}

get_metric_table <- function(input, all_player_stats) {
  current_metric <- paste0(input$metric_selector, ifelse(input$stat_category == "Total", "", "_per_possession"))
  filtered_stats <- all_player_stats %>% filter(year == input$year_selector)
  filtered_stats <- remove_low_opportunities(filtered_stats, percentage = 0.2)
  mapped_metrics <- map_metrics_to_formula(filtered_stats, current_metric)
  metric_table <- data.frame(
    fullName = filtered_stats$fullName,
    value = mapped_metrics[[current_metric]]$value,
    possessions = filtered_stats$oOpportunities,
    dpossessions = filtered_stats$dOpportunities
  )

  metric_table$percentile <- sapply(metric_table$value, calc_percentile, all_values = mapped_metrics[[current_metric]]$value)
  metric_table$percentile <- metric_table$percentile %>% round(2)
  metric_table$value <- metric_table$value %>% round(2)
  metric_table <- metric_table[order(-metric_table$percentile), ]

  dt <- DT::datatable(
    metric_table,
    options = list(pageLength = 10, autoWidth = TRUE),
    rownames = FALSE,
    colnames = c("Name", mapped_metrics[[current_metric]]$display_name, "O Possessions", "D Possessions", "%"),
    class = "compact"
  ) %>%
    DT::formatStyle(
      columns = colnames(metric_table), 
      fontSize = '12px', 
      lineHeight='70%'
    ) %>%
      DT::formatStyle(
        columns = "fullName", # Apply style only to the "Name" column
        `white-space` = "nowrap", # Prevent text wrapping
        `overflow` = "hidden",    # Hide overflowing text
        `text-overflow` = "ellipsis" # Display ellipses for overflowed text
      ) 
  return(dt)
}