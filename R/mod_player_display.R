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
    player_id <- "tmounga" 
    db_path <- get_golem_config("db_path")
    
    conn <- open_db_connection(db_path)
    all_player_stats <- get_all_player_stats(conn)  
    close_db_connection(conn)

    filtered_stats <- reactive({
      if (input$year_selector == "Career") {
        return(all_player_stats)
      } else {
        return(all_player_stats[all_player_stats$year == input$year_selector, ])
      }
    })

    filtered_throws <- reactive({
      conn <- open_db_connection(db_path)
      player_throws <- get_player_throws(conn, player_id)
      close_db_connection(conn)
      player_throws$adjusted_angle <- (as.integer(player_throws$throw_angle) + 90) %% 360 - 180
      player_throws$year <- substr(player_throws$gameID, 1, 4)

      if (input$year_selector == "Career") {
        return(player_throws)
      } else {
        return(player_throws[player_throws$year == input$year_selector, ])  # Filter stats by year
      }
    })

    output$player_name <- renderText({
      stats <- all_player_stats %>% filter(playerID == player_id)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    output$percentiles_plot <- renderPlotly({
      stats <- filtered_stats()
      if (nrow(stats) > 0) {
        stats_to_include <- c("goals", "assists", "yardsThrown", "yardsReceived", "hockeyAssists", "completions", "blocks", "assists")
        player_percentiles <- calculate_percentiles(stats, player_id = player_id, stats_to_include)

        p <- ggplot(player_percentiles, aes(x = reorder(stats, percentile), y = percentile, text = paste(stats, ": ", scales::comma(value)))) +
          geom_point(color = "black", size = 3) + # Points for each category
          geom_segment(aes(x = stats, xend = stats, y = 0, yend = percentile), 
                      color = "black", size = 1) + # Lines from 0 to Percentiles
          labs(title = "Categorical Percentile Distribution",
              x = "Metric",
              y = "Percentile") + scale_y_continuous(limits = c(0, 100)) +
          theme_minimal() + coord_flip() 
        
          # Convert ggplot to plotly for interactivity
          ggplotly(p, tooltip = c("text", "y")) %>%
            config(
              displaylogo = FALSE,
              displayModeBar = FALSE
            )
        }
  
      })


    observe({
      stats <- all_player_stats %>% filter(playerID == player_id)
      updateSelectInput(session, "year_selector", choices = c("Career", sort(stats$year)), selected = "Career")
    })

    output$radial_histogram_plot <- renderPlot({
      req(filtered_throws)
      test_data <- data.frame(
        angle = c(0, 90, 180, 270, 360),
        value = c(1, 2, 3, 4, 5)
      )
      bin_cutoffs <- seq(-180, 180, by = 24)
      throws <- na.omit(filtered_throws()$adjusted_angle)
      radial_plot <- ggplot(data.frame(throws), aes(x = throws)) +
        geom_histogram(breaks = bin_cutoffs, fill = "blue", color = "white", boundary = 0) +
        coord_polar(start = pi) +
        scale_x_continuous(limits = c(-180, 180), breaks = bin_cutoffs) +
        theme_minimal() +
        labs(
          title = "Radial Histogram of Throw Angles",
          x = "Angle (Degrees)",
          y = "Frequency"
        )
      radial_plot
    })
  })
}

