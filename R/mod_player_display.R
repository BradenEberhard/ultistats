#' player_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
mod_player_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      h1(textOutput(ns("player_name"))),
      sidebar = sidebar(
        selectInput(ns("year_selector"), "Year", choices = NULL), 
        selectInput(ns("per_possession"), "Use Per Possession", choices = c("No", "Yes"), selected = "No"),
        open=FALSE
      ),
      layout_column_wrap(
        # First value box for Plotly plot
        value_box(
          plotlyOutput(ns("percentiles_plot")), 
          title = "Percentiles Plot", 
          subtitle = "Categorical Percentile Distribution",
          width = 12
        ),
        
        # Second value box for Radial Histogram plot
        value_box(
          plotOutput(ns("radial_histogram_plot")), 
          title = "Radial Histogram", 
          subtitle = "Plot of Radial Histogram",
          width = 12
        )
      )
    )
  )
}

    
#' player_display Server Functions
#' @import ggplot2
#' @import plotly
#' @importFrom tidyr gather
#' @noRd 
mod_player_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    player_id <- "beberhard" 
    db_path <- get_golem_config("db_path")
    
    conn <- open_db_connection(db_path)
    all_player_stats <- get_all_player_stats(conn)  
    close_db_connection(conn)

    filtered_stats <- reactive({
      filter_stats_by_year(all_player_stats, input$year_selector)
    })

    filtered_throws <- reactive({
      get_filtered_throws(db_path, player_id)
    })

    output$player_name <- renderText({
      stats <- all_player_stats %>% filter(playerID == player_id)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    output$percentiles_plot <- renderPlotly({
      stats <- filtered_stats()
      if (nrow(stats) > 0) {
        per_possession <- input$per_possession == "Yes"
        player_percentiles <- calculate_percentiles(stats, player_id, per_possession)
        percentiles_plot(player_percentiles, per_possession)
      }
    })    


    observe({
      stats <- all_player_stats %>% filter(playerID == player_id)
      updateSelectInput(session, "year_selector", choices = c("Career", sort(stats$year)), selected = "Career")
    })

    output$radial_histogram_plot <- renderPlot({
      throws <- na.omit(filtered_throws()$adjusted_angle)
      radial_histogram_plot(throws)
    })
    
  })
}

