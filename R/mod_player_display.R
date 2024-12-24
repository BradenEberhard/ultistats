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
    tags$style(HTML("
  .selectize-input {
    color: white; /* Text color inside the input box */
  }
  .selectize-dropdown {
    color: white; /* Text color in the dropdown options */
  }
  .selectize-input > .selectize-control.single .selectize-input.focus {
    color: white !important; /* Color for the selected text */
  }
  .selectize-dropdown .option {
    color: white !important; /* Color for the dropdown options */
  }
")),
    bslib::page_sidebar(
      h1(textOutput(ns("player_name"))),
      sidebar = sidebar(
        selectizeInput(ns("player_selector"), "Player:", 
                     choices = NULL,
                     options = list(maxItems = 1)),
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
    db_path <- get_golem_config("db_path")
    
    conn <- open_db_connection(db_path)
    all_player_stats <- get_all_player_stats(conn)  
    close_db_connection(conn)

    # filtered_throws <- reactive({
    #   req(input$player_selector)
    #   get_filtered_throws(db_path, input$player_selector)
    # })

    output$player_name <- renderText({
      req(input$player_selector)
      stats <- all_player_stats %>% filter(playerID == input$player_selector)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    output$percentiles_plot <- renderPlotly({
      req(input$player_selector)
      req(input$year_selector)
      plot_data <- all_player_stats %>% filter(playerID == input$player_selector) %>% filter(year == input$year_selector) 
      plot_data <- convert_to_metric_df(plot_data)
      plot_ly() %>%
    # Add horizontal lines for each metric
    add_segments(
      data = plot_data,
      x = 0, 
      xend = ~percentile, 
      y = ~metric, 
      yend = ~metric,
      line = list(color = "black", width = 2),
      showlegend = FALSE
    ) %>%
    # Add markers
    add_trace(
      data = plot_data,
      x = ~percentile, 
      y = ~metric, 
      type = "scatter", 
      mode = "markers", 
      marker = list(size = 8, color = "black"),
      text = ~paste(metric, ":",value, "<br>Percentile:", percentile), # Hover text for markers
      hoverinfo = "text", # Display custom hover text
      showlegend = FALSE
    ) %>%
    # Layout settings
    layout(
      xaxis = list(title = "Skill", range = c(0, 100)), 
      yaxis = list(title = "Metric"),
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

