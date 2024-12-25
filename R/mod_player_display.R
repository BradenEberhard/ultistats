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
    tags$style(get_HTML()),
    bslib::page_sidebar(
      h1(textOutput(ns("player_name"))),
      sidebar = sidebar(
        selectizeInput(ns("player_selector"), "Player:", choices = NULL, options = list(maxItems = 1)),
        selectInput(ns("year_selector"), "Year", choices = NULL), 
        selectInput(ns("stat_category"), "Category", choices = c("Total", "Per Game", "Per Possession"), selected = "Total"),
        open=FALSE
      ),
      layout_column_wrap(
        value_box(
          plotlyOutput(ns("percentiles_plot")), 
          title = "Percentiles Plot", 
          subtitle = "Categorical Percentile Distribution",
          width = 12
        ),
        
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
    db_path <- get_golem_config("db_path")
    
    conn <- open_db_connection(db_path)
    all_player_stats <- get_all_player_stats(conn)  
    close_db_connection(conn)

    filtered_throws <- reactive({
      req(input$player_selector)
      get_filtered_throws(db_path, input$player_selector)
    })

    output$player_name <- renderText({
      req(input$player_selector)
      stats <- all_player_stats %>% filter(playerID == input$player_selector)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    output$percentiles_plot <- renderPlotly({
      addition <- ifelse(input$stat_category == "Per Possession", "(Per Possession)", ifelse(input$stat_category == "Per Game", "(Per Game)", ""))
      req(input$player_selector)
      req(input$year_selector)
      plot_data <- all_player_stats %>% filter(playerID == input$player_selector) %>% filter(year == input$year_selector) 
      plot_data <- convert_to_metric_df(plot_data, input$stat_category) %>% rename_metrics()
      plot_ly() %>%
      add_segments(
        data = plot_data,
        x = 0, 
        xend = ~percentile, 
        y = ~reorder(metric, percentile), 
        yend = ~reorder(metric, percentile),
        line = list(color = "black", width = 2),
        text = ~paste(metric, addition, ":",sub("\\.0+$", "", scales::comma(value, accuracy = 0.01)), "<br>Percentile:", percentile),
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
        text = ~paste(metric, addition, ":",sub("\\.0+$", "", scales::comma(value, accuracy = 0.01)), "<br>Percentile:", percentile),
        hoverinfo = "text", # Display custom hover text
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Percentile", range = c(0, 101)), 
        yaxis = list(title = paste("Metric", addition)),
        showlegend = FALSE 
      ) %>%
      config(displayModeBar = FALSE)
    })    


    observe({
      stats <- all_player_stats %>% filter(playerID == input$player_selector)
      updateSelectInput(session, "year_selector", choices = c(sort(stats$year)), selected = sort(stats$year)[length(stats$year)])
    })

    # output$radial_histogram_plot <- renderPlot({
    #   req(input$player_selector)
    #   throws <- na.omit(filtered_throws()$adjusted_angle)
    #   radial_histogram_plot(throws)
    # })

    observe({
      updateSelectizeInput(session, "player_selector", server = TRUE, choices = unique(all_player_stats$playerID), selected = "jkerr")
    })
    
  })
}

