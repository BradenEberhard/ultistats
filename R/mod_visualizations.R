#' visualizations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualizations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar = sidebar(
        title="Controls",
        selectizeInput(
          inputId = ns("team_selector"),
          label = "Team",
          choices = get_teams_names(),
          selected = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("player_selector"),
          label = "Players To Highlight",
          choices = NULL, # Initially, we will set this to NULL until the team is selected
          selected = NULL,
          multiple = TRUE,
        ),
        selectInput(ns("year_selector"), "Year", choices = current_year <- c(2021:as.numeric(format(Sys.Date(), "%Y")), "Career"), selected = 2024),
        selectizeInput(inputId = ns("metric_selector1"), label = "Metric 1", choices = get_table_choices(), selected = "assists"),
        selectInput(ns("stat_category1"), "Category 1", choices = c("Total", "Per Game", "Per Possession"), selected = "Total"),
        selectizeInput(inputId = ns("metric_selector2"), label = "Metric 2", choices = get_table_choices(), selected = "goals"),
        selectInput(ns("stat_category2"), "Category 2", choices = c("Total", "Per Game", "Per Possession"), selected = "Per Possession"),
      ),
      page_fluid(
        card(
          max_height="700px", 
          girafeOutput(ns("scatter_plot"))  |> 
            withSpinner() |> 
            bslib::as_fill_carrier()
        )
      )
    )
  )
}
    
#' visualizations Server Functions
#'
#' @noRd 
mod_visualizations_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    pool <- get_db_pool()
    all_player_stats <- get_all_player_stats(pool) 
    players <- reactiveVal(NULL)
    graph_is_ready <- reactiveVal(NULL)
    cached_plot <- reactiveVal(NULL)
    cached_player_list <- reactiveVal(NULL)

    observeEvent(c(input$team_selector, input$year_selector), {
      req(input$year_selector)  # Ensure inputs are available
    
      pool <- get_db_pool()
      players_list <- get_team_players(pool, input$team_selector, input$year_selector)$fullName 
    
      players(players_list)  # Update the reactive value with the list of players
      if (length(input$player_selector) > 0) {
        selected_players <- input$player_selector[input$player_selector %in% players_list]
        updateSelectizeInput(session, "player_selector", choices = players_list, selected = selected_players, server = TRUE)
        cached_player_list(selected_players)
      } else {
        updateSelectizeInput(session, "player_selector", choices = players_list, selected = NULL, server = TRUE)
      }
    })

    observeEvent(input$player_selector, {
      if (is.null(input$player_selector) || is.null(cached_player_list()) || !identical(input$player_selector, cached_player_list())) {
        cached_player_list(input$player_selector)
      }
    })

    observe({
      req(input$metric_selector1, input$metric_selector2, input$stat_category1, input$stat_category2, input$year_selector)
      cached_player_list()
      input$team_selector
      graph_is_ready(TRUE)
    })

    observeEvent(graph_is_ready(), {
      if (graph_is_ready()) {
        isolate({
          df <- adjust_for_year(all_player_stats, input$year_selector)
          if (!is.null(players())) {
            df <- df %>% filter(fullName %in% players())
          }

          metric1 <- modify_metric_name(input$metric_selector1, input$stat_category1)
          metric2 <- modify_metric_name(input$metric_selector2, input$stat_category2)

          plot_df <- prepare_plot_data(df, metric1, metric2, cached_player_list())
          interactive_plot <- create_interactive_plot(plot_df, map_metrics_to_formula(df, metric1)[[metric1]], map_metrics_to_formula(df, metric2)[[metric2]])
          cached_plot(interactive_plot)  # Store the new plot
          graph_is_ready(FALSE)  # Reset ready state
        })
      }
    })

    prepare_plot_data <- function(df, metric1, metric2, selected_players) {
      metric1_info <- map_metrics_to_formula(df, metric1)[[metric1]]
      metric2_info <- map_metrics_to_formula(df, metric2)[[metric2]]
      
      plot_df <- data.frame(
        fullName = df$fullName,
        x_value = metric1_info$value,
        y_value = metric2_info$value
      ) %>% na.omit()
      
      plot_df$tooltip_text <- paste(
        "Name: ", plot_df$fullName, 
        "<br>", metric1_info$abbreviation, ": ", round(plot_df$x_value, 2),
        "<br>", metric2_info$abbreviation, ": ", round(plot_df$y_value, 2)
      )
      
      plot_df$highlight <- ifelse(plot_df$fullName %in% selected_players, TRUE, FALSE)
      return(plot_df)
    }

    create_interactive_plot <- function(plot_df, metric1_info, metric2_info) {
      p <- ggplot(plot_df, aes(x = x_value, y = y_value, tooltip = tooltip_text)) +
        geom_point_interactive(
          size = ifelse(plot_df$highlight, 2.5, 1), 
          shape = 21,
          fill = ifelse(plot_df$highlight, "red", "navy"), 
          color = ifelse(plot_df$highlight, "red", "navy"), 
          alpha = ifelse(plot_df$highlight, 0.8, 0.6)
        ) +
        theme_minimal() +
        labs(
          title = paste("Metric Interactions for", metric1_info$abbreviation, "vs", metric2_info$abbreviation),
          x = metric1_info$display_name, 
          y = metric2_info$display_name
        ) + 
        theme(legend.position = "none")
      
      interactive_plot <- girafe(ggobj = p)
      interactive_plot <- girafe_options(
        interactive_plot,
        opts_hover(css = "stroke:black; stroke-width:20px; r:20px; transition: all 0.1s ease;"),
        opts_hover_inv(css = "opacity:0.5; filter:saturate(10%);"),
        opts_toolbar(saveaspng = FALSE, hidden = c("selection")),
        opts_selection(type = "none")
      )
      return(interactive_plot)
    }

    output$scatter_plot <- renderGirafe({
      req(cached_plot())
      cached_plot()
    })
  })
}
    
## To be copied in the UI
# mod_visualizations_ui("visualizations_1")
    
## To be copied in the server
# mod_visualizations_server("visualizations_1")
