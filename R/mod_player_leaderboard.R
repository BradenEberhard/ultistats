#' player_leaderboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_leaderboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = sidebar(
        title="Controls",
        selectInput(ns("year_selector"), "Year", choices = current_year <- c(2021:as.numeric(format(Sys.Date(), "%Y")), "Career"), selected = "Career"),
        selectInput(ns("stat_category"), "Category", choices = c("Total", "Per 100 Possessions"), selected = "Total"),
        selectInput(inputId = ns("metric_selector"), label = "Metric", choices = c("Plus Minus"="plus_minus", "Goals"="goals", "Assists"="assists", "Receiver Adjusted Expected Contribution"="receiver_aec"), selected = "Plus Minus")
      ),
      shiny::mainPanel(
        layout_columns(
          card(DT::dataTableOutput(ns("metrics_table"))),
          card(DT::dataTableOutput(ns("grade_table")), fillable=FALSE)
        )
      )
    )
  )
}
    
#' player_leaderboard Server Functions
#'
#' @noRd 
mod_player_leaderboard_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    conn <- open_db_connection()
    session$onSessionEnded(function() {close_db_connection(conn)})

    all_player_stats <- get_all_player_stats(conn)  
    
    output$metrics_table <- DT::renderDataTable({
      current_metric <- paste0(input$metric_selector, ifelse(input$stat_category == "Total", "", "_per_possession"))
      filtered_stats <- all_player_stats %>% filter(year == input$year_selector)
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

      DT::datatable(
        metric_table,
        options = list(pageLength = 10, autoWidth = TRUE, dom = "t"),
        colnames = c("Name", "Value", "Offensive Possessions", "Defensive Possessions", "Percentile")
      )
    })

    output$grade_table <- DT::renderDataTable({
      usage_metrics <- c("completions", "completions_per_possession", "games", "offensive_points_per_game")
      efficiency_metrics <- c("cpoe", "xcp", "offensive_efficiency", "completion_percentage")
      contribution_metrics <- c("thrower_ec_per_possession", "thrower_aec_per_possession", "yardsThrown_per_possession")
      scores_metrics <- c("assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession")    

      filtered_stats <- all_player_stats %>% filter(year == input$year_selector)
      filtered_stats <- add_percentiles(filtered_stats)

      all_metrics <- c(usage_metrics, efficiency_metrics, contribution_metrics, scores_metrics)
      percentiles_columns <- paste0(all_metrics, "_percentile")
      filtered_stats$thrower_percentile <- apply(filtered_stats[, percentiles_columns], 1, median, na.rm = TRUE)
      filtered_stats$thrower_percentile <- filtered_stats$thrower_percentile %>% round(2)
      filtered_stats <- filtered_stats[order(-filtered_stats$thrower_percentile), ]

      DT::datatable(
        filtered_stats %>% select(fullName, thrower_percentile),
        options = list(pageLength = 10, autoWidth = TRUE, dom = "t"),
        colnames = c("Name", "Thrower Percentile")
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_player_leaderboard_ui("player_leaderboard_1")
    
## To be copied in the server
# mod_player_leaderboard_server("player_leaderboard_1")
